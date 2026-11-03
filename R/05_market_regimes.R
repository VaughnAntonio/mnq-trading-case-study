# 05_market_regimes.R
# Q6: do my days go better on trend days or on choppy days?
#
# Tag every MNQ daily bar, then join the tags to my trading days by date.
#   range_pct  = (high - low) / open           how big the day's range was
#   body_ratio = |close - open| / (high - low) how much of the range was "body"
#   Trend day : body_ratio > 0.6  (closed near one end of its range)
#   Chop day  : body_ratio < 0.3  (closed near where it opened)
#   Otherwise : "normal"
#   Context   : close above its 50-day moving average = bull, below = bear
#
# Inputs:  data/market/mnq_daily.csv, data/processed/days_clean.csv
# Outputs: data/market/mnq_regimes.csv (public), data/processed/days_regimes.csv
#          (private), outputs/tables/q6_*.csv, outputs/figures/q6_*.png
#
# Limitations (state in report):
# - Yahoo's daily bar is the full Globex session (6pm-5pm New York).
#   I trade the New York session, so a "trend day" here is a whole-session label.
# - Backtest dates on weekends have no bar and get no tag.

source(here::here("R", "00_setup.R"))

mnq  <- read_csv(here("data", "market", "mnq_daily.csv"), show_col_types = FALSE)
days <- read_csv(here("data", "processed", "days_clean.csv"), show_col_types = FALSE) |>
  mutate(account_type = factor(account_type, levels = account_levels))

set.seed(2025)

# ---- Tag market days ---------------------------------------------------------

regimes <- mnq |>
  arrange(date) |>
  mutate(
    range_pct  = (high - low) / open,
    body_ratio = if_else(high > low, abs(close - open) / (high - low), NA_real_),
    regime     = case_when(
      body_ratio > 0.6 ~ "trend",
      body_ratio < 0.3 ~ "chop",
      !is.na(body_ratio) ~ "normal"
    ),
    direction  = if_else(close >= open, "up", "down"),
    ma50       = TTR::SMA(close, n = 50),
    context    = case_when(close > ma50 ~ "bull", close <= ma50 ~ "bear")
  )

write_csv(regimes, here("data", "market", "mnq_regimes.csv"))

# Baseline: how common is each regime in the market overall (2025 onwards)?
baseline <- regimes |>
  filter(date >= as.Date("2025-01-01"), !is.na(regime)) |>
  count(regime) |>
  mutate(share_market = n / sum(n))

# ---- Join to my trading days -------------------------------------------------

days_regimes <- days |>
  left_join(regimes |> select(date, regime, direction, context, range_pct, body_ratio),
            by = "date")

write_csv(days_regimes, here("data", "processed", "days_regimes.csv"))

untagged <- days_regimes |> filter(is.na(regime))
message("Trading days with no market tag (weekends): ", nrow(untagged))

tagged <- days_regimes |> filter(!is.na(regime))

# ---- Q6 tables ---------------------------------------------------------------

q6_regime  <- perf_summary(tagged, day_pnl_usd, day_r, account_type, regime)
q6_context <- perf_summary(tagged |> filter(!is.na(context)),
                           day_pnl_usd, day_r, account_type, context)

# Do I trade trend days more or less often than they occur?
q6_mix <- tagged |>
  count(account_type, regime) |>
  group_by(account_type) |>
  mutate(share_my_days = n / sum(n)) |>
  ungroup() |>
  left_join(baseline |> select(regime, share_market), by = "regime")

save_tbl(q6_regime,  "q6_by_regime")
save_tbl(q6_context, "q6_by_context")
save_tbl(q6_mix,     "q6_regime_mix")

# ---- Q6 charts ---------------------------------------------------------------

p_regime <- q6_regime |>
  mutate(regime = factor(regime, levels = c("trend", "normal", "chop"))) |>
  plot_expectancy(regime, "Expectancy by market regime (days)")
save_fig(p_regime, "q6_expectancy_by_regime", width = 10)

p_context <- plot_expectancy(q6_context, context, "Expectancy: above vs below 50-day MA (days)")
save_fig(p_context, "q6_expectancy_by_context", width = 10)

# ---- Print -------------------------------------------------------------------

message("\nMarket baseline (2025+):"); print(baseline)
message("\nQ6 by regime:");   print(q6_regime  |> select(account_type, regime,  n, win_rate, expectancy_usd, expectancy_r, expectancy_r_ci_low, expectancy_r_ci_high), n = Inf)
message("\nQ6 by context:");  print(q6_context |> select(account_type, context, n, win_rate, expectancy_usd, expectancy_r, expectancy_r_ci_low, expectancy_r_ci_high), n = Inf)
