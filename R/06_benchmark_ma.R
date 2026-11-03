# 06_benchmark_ma.R
# Q7: how does my journal compare with a simple moving-average trend rule?
#
# The rule (daily, long only):
#   If the 20-day MA is above the 50-day MA at today's close,
#   hold 1 MNQ contract for the NEXT day. Otherwise stay flat.
#   (Acting on the next day avoids using information we would not have had.)
# Compared with buy & hold (always long 1 contract).
#
# Important framing for the report: this is CONTEXT, not a like-for-like race.
# The rule is daily and passive; my trading is intraday and discretionary,
# with different size and risk.
#
# Change from brief: reads data/market/mnq_daily.csv (from 03) instead of a
# second download, and uses dplyr instead of xts so dplyr::lag() and
# stats::lag() cannot be mixed up (tidyquant masks them).
#
# Limitation: MNQ=F is not back-adjusted. Each quarterly roll creates a small
# artificial jump in the series, which leaks into daily returns.

source(here::here("R", "00_setup.R"))

mnq  <- read_csv(here("data", "market", "mnq_daily.csv"), show_col_types = FALSE)
days <- read_csv(here("data", "processed", "days_clean.csv"), show_col_types = FALSE) |>
  mutate(account_type = factor(account_type, levels = account_levels))

# ---- Build the rule ----------------------------------------------------------

bench <- mnq |>
  arrange(date) |>
  mutate(
    ret       = close / dplyr::lag(close) - 1,             # daily % return
    pts       = close - dplyr::lag(close),                 # daily points move
    sma20     = TTR::SMA(close, n = 20),
    sma50     = TTR::SMA(close, n = 50),
    in_market = dplyr::lag(as.integer(sma20 > sma50)),     # yesterday's signal
    ma_ret    = ret * in_market,
    bh_ret    = ret,
    ma_usd    = pts * mnq_point_value * in_market,         # 1 MNQ contract
    bh_usd    = pts * mnq_point_value
  ) |>
  drop_na(in_market, ret)

# ---- Full-history stats ------------------------------------------------------
# Annualised return: average yearly growth.     252 trading days per year.
# Volatility: how much returns jump around (annualised standard deviation).
# Sharpe: return per unit of volatility (risk-free rate taken as 0). Higher = better.
# Max drawdown: worst fall from a peak, in %.

strategy_stats <- function(r) {
  growth <- cumprod(1 + r)
  tibble(
    years          = length(r) / 252,
    ann_return     = prod(1 + r)^(252 / length(r)) - 1,
    ann_volatility = sd(r) * sqrt(252),
    sharpe         = ann_return / ann_volatility,
    max_drawdown   = min(growth / cummax(growth) - 1),
    time_in_market = mean(r != 0)
  )
}

q7_full <- bind_rows(
  strategy_stats(bench$ma_ret) |> mutate(strategy = "MA 20/50 rule", .before = 1),
  strategy_stats(bench$bh_ret) |> mutate(strategy = "Buy & hold",    .before = 1)
) |>
  mutate(from = min(bench$date), to = max(bench$date), .after = strategy)

save_tbl(q7_full, "q7_benchmark_full")

# Cross-check with PerformanceAnalytics (brief, Phase 4)
pa_check <- xts::xts(as.matrix(bench |> select(MA_rule = ma_ret, Buy_hold = bh_ret)),
                     order.by = bench$date)
print(PerformanceAnalytics::table.AnnualizedReturns(pa_check))
print(PerformanceAnalytics::maxDrawdown(pa_check))

# ---- Same window as my live trading ------------------------------------------
# Live = forward test, eval and funded days (not backtest).

live <- days |> filter(account_type != "backtest")
window <- c(min(live$date), max(live$date))

bench_window <- bench |> filter(between(date, window[1], window[2]))

q7_window <- tibble(
  strategy = c("MA 20/50 rule (1 MNQ)", "Buy & hold (1 MNQ)"),
  days     = nrow(bench_window),
  pnl_usd  = c(sum(bench_window$ma_usd), sum(bench_window$bh_usd))
) |>
  bind_rows(
    live |>
      group_by(strategy = paste0("My journal: ", account_type)) |>
      summarise(days = n(), pnl_usd = sum(day_pnl_usd), .groups = "drop")
  ) |>
  mutate(from = window[1], to = window[2])

save_tbl(q7_window, "q7_benchmark_window")

# ---- Charts ------------------------------------------------------------------

# Full history: growth of $1
p_full <- bench |>
  mutate(`MA 20/50 rule` = cumprod(1 + ma_ret), `Buy & hold` = cumprod(1 + bh_ret)) |>
  pivot_longer(c(`MA 20/50 rule`, `Buy & hold`), names_to = "strategy", values_to = "growth") |>
  ggplot(aes(date, growth, colour = strategy)) +
  geom_line() +
  scale_y_continuous(labels = label_dollar()) +
  scale_colour_manual(values = c(`MA 20/50 rule` = "#2b6cb0", `Buy & hold` = "#a0aec0")) +
  labs(title = "MNQ: MA 20/50 rule vs buy & hold (growth of $1)",
       x = NULL, y = NULL, colour = NULL) +
  theme(legend.position = "bottom")
save_fig(p_full, "q7_ma_vs_buyhold")

# Live window: cumulative $ (1 MNQ for the rules, my real size for the journal)
p_window <- bind_rows(
  bench_window |> transmute(date, strategy = "MA 20/50 rule (1 MNQ)", pnl = ma_usd),
  bench_window |> transmute(date, strategy = "Buy & hold (1 MNQ)",    pnl = bh_usd),
  live |> transmute(date, strategy = "My journal (live, all accounts)", pnl = day_pnl_usd)
) |>
  arrange(date) |>
  group_by(strategy) |>
  mutate(cum_pnl = cumsum(pnl)) |>
  ggplot(aes(date, cum_pnl, colour = strategy)) +
  geom_step() +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Same window as my live trading",
       subtitle = "Rules trade 1 MNQ contract; my journal uses my real (larger, varying) size",
       x = NULL, y = "Cumulative PnL", colour = NULL) +
  theme(legend.position = "bottom")
save_fig(p_window, "q7_live_window")

# ---- Print -------------------------------------------------------------------

message("\nQ7 full history:");  print(q7_full, width = Inf)
message("\nQ7 live window:");   print(q7_window, width = Inf)
