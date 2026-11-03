# 04_metrics.R
# Part A metrics: Q1-Q5. Every metric is split by account_type
# (backtest / forward_test / eval / funded), never pooled.
#
# Two levels of data:
#   days   = one row per trading day. Complete record. Used for Q1, Q3, Q4, Q5.
#   trades = journalled trades only (a subset). Used for Q1 (trade level), Q2.
#
# Outputs: outputs/tables/*.csv, outputs/figures/*.png
# Both contain dollar values: keep them out of git until the privacy decision.

source(here::here("R", "00_setup.R"))

days <- read_csv(here("data", "processed", "days_clean.csv"), show_col_types = FALSE) |>
  mutate(account_type = factor(account_type, levels = account_levels))
trades <- read_csv(here("data", "processed", "trades_clean.csv"), show_col_types = FALSE) |>
  mutate(account_type = factor(account_type, levels = account_levels))

set.seed(2025)   # bootstrap results are random; a seed makes them repeatable

# Helpers boot_ci(), perf_summary() and plot_expectancy() live in 00_setup.R

# ---- Q1: win rate, avg win/loss, expectancy ----------------------------------

q1_days   <- perf_summary(days, day_pnl_usd, day_r, account_type)
q1_trades <- perf_summary(trades, pnl_usd, r_multiple, account_type)

save_tbl(q1_days,   "q1_summary_days")
save_tbl(q1_trades, "q1_summary_trades")

# How complete is the trade journal? (trades written up vs trades taken)
journal_coverage <- days |>
  group_by(account_type, account_name) |>
  summarise(days = n(),
            trades_taken      = sum(trades_taken, na.rm = TRUE),
            trades_journalled = sum(trades_journalled),
            .groups = "drop") |>
  mutate(coverage = if_else(trades_taken > 0, trades_journalled / trades_taken, NA_real_))

save_tbl(journal_coverage, "journal_coverage")

# ---- Q2: do A setups beat B and C? (trade level) -----------------------------
# Grades grouped into families: A+/A -> A, B+/B/B- -> B, C+/C/C- -> C.

trades <- trades |>
  mutate(grade_family = case_when(
    str_starts(setup_grade, "A") ~ "A",
    str_starts(setup_grade, "B") ~ "B",
    str_starts(setup_grade, "C") ~ "C",
    str_starts(setup_grade, "E") ~ "E",
    .default = "Ungraded"
  ))

q2_grade <- perf_summary(trades, pnl_usd, r_multiple, account_type, grade_family)
save_tbl(q2_grade, "q2_by_grade")

# ---- Q3: which strategy? (day level: strategy is a day property) -------------

q3_strategy <- days |>
  mutate(strategy_main = replace_na(strategy_main, "Not recorded")) |>
  perf_summary(day_pnl_usd, day_r, account_type, strategy_main)
save_tbl(q3_strategy, "q3_by_strategy")

# ---- Q4: backtest -> forward -> eval -> funded -------------------------------
# Same as q1_days; kept as its own table/chart for the report.

q4_stage <- q1_days
save_tbl(q4_stage, "q4_by_stage")

# ---- Q5: drawdowns vs prop-firm limits ---------------------------------------
# Prop firms stop you out if equity falls to a "threshold" below your peak.
# The threshold trails the peak, then LOCKS once you have enough profit:
#   APEX 50k:    trails $2,500 below peak, locks at +$100 (start + $100)
#   TOPSTEP 50k: trails $2,000 below peak, locks at $0    (starting balance)
# So a big drop after big profits may NOT breach. Check these rules against
# your own account dashboards.
# Limitation: daily data only sees end-of-day equity. An intraday dip can
# breach even when the day closes above the threshold.

prop_limits <- tribble(
  ~account_name,            ~limit_usd, ~lock_usd,
  "APEX 50k Eval",          2500,       100,
  "APEX 50k PA",            2500,       100,
  "TOPSTEP 50k Combine 1",  2000,       0,
  "TOPSTEP 6x50k Combine",  2000,       0
)

equity <- days |>
  arrange(date) |>
  group_by(account_type, account_name) |>
  mutate(
    equity   = cumsum(day_pnl_usd),
    peak     = pmax(cummax(equity), 0),   # starting balance counts as a peak
    drawdown = equity - peak              # drop from the highest point so far
  ) |>
  ungroup() |>
  left_join(prop_limits, by = "account_name") |>
  mutate(
    threshold = pmin(peak - limit_usd, lock_usd),   # where the firm stops you
    cushion   = equity - threshold                  # room left before breach
  )

q5_drawdown <- equity |>
  group_by(account_type, account_name) |>
  summarise(
    days             = n(),
    final_pnl_usd    = dplyr::last(equity),
    max_drawdown_usd = min(drawdown),
    worst_day_usd    = min(day_pnl_usd),
    limit_usd        = dplyr::first(limit_usd),
    min_cushion_usd  = min(cushion),
    .groups = "drop"
  ) |>
  mutate(
    drawdown_vs_limit = abs(max_drawdown_usd) / limit_usd,
    breached_eod      = min_cushion_usd <= 0     # end-of-day equity hit threshold
  )

save_tbl(q5_drawdown, "q5_drawdowns")

# ---- Charts ------------------------------------------------------------------

# 1. Equity curve per account
p_equity <- equity |>
  ggplot(aes(date, equity)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_step(colour = "#2b6cb0") +
  facet_wrap(~ account_name, scales = "free") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Equity curve by account", x = NULL, y = "Cumulative PnL")
save_fig(p_equity, "equity_curve", width = 10, height = 6)

# 1b. Equity vs prop-firm threshold (red dashed = where you get stopped out)
p_threshold <- equity |>
  filter(!is.na(limit_usd)) |>
  ggplot(aes(date)) +
  geom_step(aes(y = equity), colour = "#2b6cb0") +
  geom_step(aes(y = threshold), colour = "#c53030", linetype = "dashed") +
  facet_wrap(~ account_name, scales = "free") +
  scale_y_continuous(labels = label_dollar()) +
  labs(title = "Equity vs prop-firm stop-out threshold (dashed)",
       x = NULL, y = "Cumulative PnL")
save_fig(p_threshold, "equity_vs_threshold", width = 10, height = 6)

# 2. Expectancy by grade (Q2)
save_fig(plot_expectancy(q2_grade, grade_family, "Expectancy by setup grade (trades)"),
         "q2_expectancy_by_grade", width = 10)

# 3. Expectancy by strategy (Q3)
save_fig(plot_expectancy(q3_strategy, strategy_main, "Expectancy by strategy (days)") +
           theme(axis.text.x = element_text(angle = 30, hjust = 1)),
         "q3_expectancy_by_strategy", width = 10)

# 4. Stage comparison (Q4): win rate and expectancy
p_stage <- q4_stage |>
  select(account_type, n, win_rate, expectancy_r) |>
  pivot_longer(c(win_rate, expectancy_r), names_to = "metric") |>
  ggplot(aes(account_type, value)) +
  geom_col(fill = "#2b6cb0") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
  facet_wrap(~ metric, scales = "free_y",
             labeller = as_labeller(c(win_rate = "Win rate (days)",
                                      expectancy_r = "Expectancy (R per day)"))) +
  labs(title = "Backtest vs forward test vs eval vs funded", x = NULL, y = NULL)
save_fig(p_stage, "q4_stage_comparison")

# 5. PnL by weekday (bonus)
p_weekday <- days |>
  mutate(weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) |>
  group_by(account_type, weekday) |>
  summarise(avg_r = mean(day_r), n = n(), .groups = "drop") |>
  ggplot(aes(weekday, avg_r)) +
  geom_col(fill = "#2b6cb0") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
  facet_wrap(~ account_type, nrow = 1) +
  labs(title = "Average day result by weekday", x = NULL, y = "Avg R per day")
save_fig(p_weekday, "weekday_r", width = 10)

# ---- Print key tables --------------------------------------------------------

message("\nQ1/Q4 (days):"); print(q1_days, width = Inf)
message("\nQ1 (journalled trades):"); print(q1_trades, width = Inf)
message("\nQ5 drawdowns:"); print(q5_drawdown, width = Inf)
message("\nJournal coverage:"); print(journal_coverage)
