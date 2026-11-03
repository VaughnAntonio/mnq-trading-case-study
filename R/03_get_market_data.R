# 03_get_market_data.R
# Download MNQ daily prices from Yahoo Finance.
#
# Output: data/market/mnq_daily.csv  (public market data, safe to commit)
#
# Notes:
# - MNQ=F is a continuous front-month series. Prices jump at each quarterly
#   contract roll. Fine for daily regime tags; state it in the report.
# - We download from MNQ's launch (May 2019) so the same file serves
#   05 (needs 50 days before the first journal date) and 06 (MA benchmark).
#   Change from brief: brief used from = "2025-01-01" here and a second
#   download in 06. One download keeps the analysis reproducible.

source(here::here("R", "00_setup.R"))
library(tidyquant)

mnq <- tq_get("MNQ=F", get = "stock.prices", from = "2019-05-01") |>
  drop_na(open, high, low, close) |>
  select(date, open, high, low, close, volume) |>
  arrange(date)

if (nrow(mnq) == 0) stop("No data returned from Yahoo. Check your connection and retry.")

write_csv(mnq, here("data", "market", "mnq_daily.csv"))

message("MNQ daily rows: ", nrow(mnq),
        " | from ", min(mnq$date), " to ", max(mnq$date))

# ---- Check: do journal dates have a market day? ------------------------------
# Weekends and holidays have no daily bar. Those journal days cannot get a
# trend/chop tag in 05.

days <- read_csv(here("data", "processed", "days_clean.csv"), show_col_types = FALSE)

no_bar <- days |>
  anti_join(mnq, by = "date") |>
  select(date, weekday, account_type, account_name)

message("\nJournal days with no MNQ daily bar: ", nrow(no_bar))
print(no_bar, n = Inf)
