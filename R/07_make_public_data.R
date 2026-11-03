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
# Outputs: data/public/days_public.csv, data/public/trades_public.csv
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

write_csv(days_public,   here("data", "public", "days_public.csv"))
write_csv(trades_public, here("data", "public", "trades_public.csv"))

message("Public files written: ", nrow(days_public), " days, ",
        nrow(trades_public), " trades. Scale factor not printed on purpose.")
