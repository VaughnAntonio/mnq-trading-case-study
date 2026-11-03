# 07_make_public_data.R
# Build the anonymised, public versions of the clean data (brief, Section 5,
# privacy option 2: scaled dollars).
#
# Every dollar value is multiplied by one hidden factor, PNL_SCALE, stored in
# .Renviron (gitignored). Shapes, ratios, win rates and R multiples stay true.
# Real dollar amounts do not appear in the public repo.
#
# Change from brief: new script 07 (brief has 00-06), and public files live in
# data/public/ so data/processed/ can stay fully private.
#
# Inputs:  data/processed/days_regimes.csv, data/processed/trades_clean.csv
#          outputs/tables/q5_drawdowns.csv, q7_benchmark_window.csv (from 04, 06)
# Outputs: data/public/days_public.csv, data/public/trades_public.csv,
#          data/public/comparisons_public.csv
#
# Run order: 04 and 06 must run before this script.
#
# Dropped on purpose: free-text notes, pair, contract counts (size can help
# reverse the scale factor), and anything identifying an account number.

source(here::here("R", "00_setup.R"))

scale <- as.numeric(Sys.getenv("PNL_SCALE"))
if (is.na(scale) || scale <= 0) {
  stop("PNL_SCALE missing. Add a hidden number to .Renviron, then restart R.")
}

days   <- read_csv(here("data", "processed", "days_regimes.csv"), show_col_types = FALSE)
trades <- read_csv(here("data", "processed", "trades_clean.csv"), show_col_types = FALSE)

days_public <- days |>
  transmute(
    day_id, date, weekday, account_type, account_name, day_tag, market,
    strategy, strategy_main, trades_taken, trades_journalled,
    pnl_scaled  = round(day_pnl_usd * scale, 2),
    risk_scaled = round(risk_usd * scale, 2),
    r_method,
    day_r,                       # unitless: unchanged by scaling
    win,
    regime, direction, context, range_pct, body_ratio
  )

trades_public <- trades |>
  transmute(
    trade_id, date, weekday, account_type, account_name, trade_no, market,
    strategy, strategy_main, setup_grade, planned_rr,
    pnl_scaled  = round(pnl_usd * scale, 2),
    risk_scaled = round(risk_usd * scale, 2),
    r_method,
    r_multiple,                  # unitless: unchanged by scaling
    win, rr_had_colon, rr_fixed
  )

# ---- Checks ------------------------------------------------------------------
# Scaling must not change any unitless result.

stopifnot(
  nrow(days_public) == nrow(days),
  nrow(trades_public) == nrow(trades),
  all.equal(days_public$win, days$day_pnl_usd > 0),
  all.equal(trades_public$r_multiple, trades$r_multiple),
  !any(c("notes", "pnl_usd", "day_pnl_usd", "risk_usd", "contracts", "pair")
       %in% c(names(days_public), names(trades_public)))
)

# ---- Unitless comparisons to known dollar amounts ----------------------------
# Decision (2026-11-03): the report shows exact comparisons with prop-firm
# limits and the 1-MNQ benchmark. These are ratios, so they carry no dollar
# unit, but they do let a reader work back to real amounts. Accepted trade-off.

q5 <- read_csv(here("outputs", "tables", "q5_drawdowns.csv"), show_col_types = FALSE)
q7 <- read_csv(here("outputs", "tables", "q7_benchmark_window.csv"), show_col_types = FALSE)

bench_1mnq <- q7 |> filter(strategy == "Buy & hold (1 MNQ)") |> pull(pnl_usd)

comparisons_public <- bind_rows(
  q5 |>
    filter(!is.na(limit_usd)) |>
    transmute(question = "Q5", account_type, account_name,
              metric = "max_drawdown_vs_limit", value = abs(max_drawdown_usd) / limit_usd),
  q5 |>
    filter(!is.na(limit_usd)) |>
    transmute(question = "Q5", account_type, account_name,
              metric = "worst_day_vs_limit", value = abs(worst_day_usd) / limit_usd),
  q5 |>
    filter(!is.na(limit_usd)) |>
    transmute(question = "Q5", account_type, account_name,
              metric = "min_cushion_vs_limit", value = min_cushion_usd / limit_usd),
  q7 |>
    filter(str_starts(strategy, "My journal")) |>
    transmute(question = "Q7", account_type = str_remove(strategy, "My journal: "),
              account_name = NA_character_,
              metric = "journal_pnl_vs_1mnq_buyhold", value = pnl_usd / bench_1mnq)
) |>
  mutate(value = round(value, 3))

write_csv(comparisons_public, here("data", "public", "comparisons_public.csv"))

write_csv(days_public,   here("data", "public", "days_public.csv"))
write_csv(trades_public, here("data", "public", "trades_public.csv"))

message("Public files written: ", nrow(days_public), " days, ",
        nrow(trades_public), " trades. Scale factor not printed on purpose.")
