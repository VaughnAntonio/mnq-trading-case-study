# 02_clean_trades.R
# Clean the raw Notion extract into two analysis tables.
#
# Inputs:  data/raw/days_raw.csv, data/raw/trades_raw.csv   (from 01)
#          data/processed/manual_fixes.csv                  (hand-checked fixes)
# Outputs: data/processed/days_clean.csv    one row per trading day (complete)
#          data/processed/trades_clean.csv  one row per journalled trade (subset)
#
# Both outputs hold dollar PnL, so they are gitignored until the privacy
# decision (brief, Section 5). A public, anonymised version comes later.

source(here::here("R", "00_setup.R"))

days_raw   <- read_csv(here("data", "raw", "days_raw.csv"),   col_types = cols(.default = "c"))
trades_raw <- read_csv(here("data", "raw", "trades_raw.csv"), col_types = cols(.default = "c"))
fixes      <- read_csv(here("data", "processed", "manual_fixes.csv"), col_types = cols(.default = "c"))

# ---- Helpers -----------------------------------------------------------------

# "17/9/25 (FOMC)" -> 2025-09-17 ; "30/06/2025" -> 2025-06-30
parse_title_date <- function(x) dmy(str_extract(x, "\\d{1,2}/\\d{1,2}/\\d{2,4}"))

# Instrument family from the free-text Pair property
market_from_pair <- function(pair) {
  p <- str_to_upper(pair)
  case_when(
    is.na(p)                                       ~ NA_character_,
    str_detect(p, "MGC|MGZ|XAU") & str_detect(p, "NQ") ~ "mixed",
    str_detect(p, "MGC|MGZ|XAU|GOLD")              ~ "gold",
    str_detect(p, "SP500|S&P|\\bMES\\b")           ~ "sp500",
    str_detect(p, "NQ|NAS|NASDAQ")                 ~ "nasdaq",
    .default = "other"
  )
}

# One main strategy per day. Named models win over tags like Scalp/Gamba.
main_strategy <- function(s) {
  case_when(
    is.na(s)                           ~ NA_character_,
    str_detect(s, "ModelV2")           ~ "ModelV2",
    str_detect(s, "Forever Model")     ~ "Forever Model",
    str_detect(s, "FVG")               ~ "FVG",
    str_detect(s, regex("gamba", ignore_case = TRUE)) ~ "Gamba",
    .default = "Other"
  )
}

# ---- Manual fixes ------------------------------------------------------------

# Page renames (date typos), applied to day titles by account + page name
name_fixes <- fixes |>
  filter(action == "fix", field == "page_name") |>
  select(account_name, name = page_name, new_name = new_value)

# Planned RR fixes, matched on account + page + exact trade text
rr_fixes <- fixes |>
  filter(action == "fix", field == "planned_rr") |>
  select(account_name, name = page_name, text, rr_fix = new_value)

# ---- Days --------------------------------------------------------------------

days <- days_raw |>
  left_join(name_fixes, by = c("account_name", "name")) |>
  mutate(
    title_fixed  = !is.na(new_name),
    name         = coalesce(new_name, name),
    date         = parse_title_date(name),
    day_tag      = str_match(name, "\\(([^)]+)\\)")[, 2],      # e.g. FOMC, Fund trial
    pnl_text     = str_remove_all(pnl, regex("pnl|\\s", ignore_case = TRUE)),
    day_pnl_usd  = if_else(str_to_upper(pnl_text) == "BE", 0,
                           suppressWarnings(as.numeric(pnl_text))),
    contracts    = as.integer(contract_size),                  # day-level property
    trades_taken = as.integer(trades_taken),
    strategy     = na_if(strategy, ""),
    strategy_main = main_strategy(strategy),
    market       = market_from_pair(pair)
  ) |>
  select(page_id, date, account_type, account_name, name, day_tag, title_fixed,
         market, pair, strategy, strategy_main, contracts, trades_taken, day_pnl_usd)

# ---- Scope: Nasdaq only ------------------------------------------------------
# The case study is about MNQ. Keep Nasdaq-100 instruments only
# (MNQ, NQ, NAS100). Gold, S&P 500 and mixed days are left out; R multiples
# make NQ/NAS100 comparable with MNQ despite different contract sizes.
# Empty pages (no market, no PnL) are kept for now and dropped further down.

out_of_scope <- days |> filter(!is.na(market), market != "nasdaq")
message("Out of scope (not Nasdaq): ", nrow(out_of_scope), " days")
print(count(out_of_scope, account_name, market))

days <- days |> filter(is.na(market) | market == "nasdaq")

# ---- Trades ------------------------------------------------------------------

trades <- trades_raw |>
  left_join(rr_fixes, by = c("account_name", "name", "text")) |>
  mutate(
    planned_rr  = as.numeric(coalesce(rr_fix, planned_rr)),
    rr_fixed    = !is.na(rr_fix),
    trade_no    = as.integer(trade_no),
    block_order = as.integer(block_order),
    pnl_usd     = as.numeric(pnl_usd),
    setup_grade = str_to_upper(setup_grade),
    rr_had_colon = as.logical(rr_had_colon)
  ) |>
  select(page_id, block_order, text, trade_no, planned_rr, rr_had_colon, rr_fixed,
         pnl_usd, setup_grade, notes = rest) |>
  distinct(page_id, block_order, .keep_all = TRUE) |>
  # Date, account, strategy and contracts come from the day page
  inner_join(days |> select(page_id, date, account_type, account_name, market,
                            strategy, strategy_main, contracts),
             by = "page_id")

# ---- Fill missing day PnL from trades ----------------------------------------
# Some backtest days have trades written but no PnL property.

trade_day_pnl <- trades |>
  group_by(page_id) |>
  summarise(trades_journalled = n(), trades_pnl_usd = sum(pnl_usd))

days <- days |>
  left_join(trade_day_pnl, by = "page_id") |>
  mutate(
    trades_journalled = replace_na(trades_journalled, 0L),
    pnl_source  = case_when(!is.na(day_pnl_usd)    ~ "page",
                            !is.na(trades_pnl_usd) ~ "trades",
                            .default = NA_character_),
    day_pnl_usd = coalesce(day_pnl_usd, trades_pnl_usd)
  )

# Empty pages (made in advance, never traded) have no PnL and no trades
empty_days <- days |> filter(is.na(day_pnl_usd))
days <- days |> filter(!is.na(day_pnl_usd))
message("Dropped ", nrow(empty_days), " empty day pages.")

# ---- R multiples (brief, Section 6.3) ---------------------------------------
# 1R = the dollar amount risked per trade. Two methods, best first:
#
#  Option 1 (stated): risk Vaughn gave for the account. Midpoint of his range.
#    R = pnl / risk for every trade. A loss bigger than planned shows as
#    worse than -1R, which is useful: it shows stops not respected.
#  Option 2 (estimated): no stated risk, so 1R = median losing trade per
#    account and market (fallback: median losing day).
#    Loss = -1R, break-even = 0R, win = pnl / 1R.

stated_risk <- tribble(
  ~account_name,            ~risk_usd, ~risk_note,
  "APEX 50k PA",            100,       "stated $50-150",
  "TOPSTEP 50k Combine 1",  375,       "stated $250-500",
  "TOPSTEP 6x50k Combine",  375,       "stated $250-500"
) |>
  mutate(market = "nasdaq", risk_n = NA_integer_, r_method = "stated")

risk_trades <- trades |>
  filter(pnl_usd < 0) |>
  group_by(account_name, market) |>
  summarise(risk_usd = median(abs(pnl_usd)), risk_n = n(), .groups = "drop") |>
  mutate(risk_note = "median losing trade", r_method = "estimated")

risk_days <- days |>
  filter(day_pnl_usd < 0) |>
  group_by(account_name, market) |>
  summarise(risk_usd = median(abs(day_pnl_usd)), risk_n = n(), .groups = "drop") |>
  mutate(risk_note = "median losing day", r_method = "estimated")

# Stated first, then losing trades, then losing days
keys <- c("account_name", "market")
risk_units <- bind_rows(stated_risk, anti_join(risk_trades, stated_risk, by = keys))
risk_units <- bind_rows(risk_units, anti_join(risk_days, risk_units, by = keys)) |>
  select(account_name, market, risk_usd, r_method, risk_note, risk_n)

trades <- trades |>
  left_join(risk_units |> select(account_name, market, risk_usd, r_method),
            by = c("account_name", "market")) |>
  mutate(r_multiple = case_when(
    r_method == "stated" ~ pnl_usd / risk_usd,
    pnl_usd < 0          ~ -1,
    pnl_usd == 0         ~ 0,
    .default             = pnl_usd / risk_usd
  ))

days <- days |>
  left_join(risk_units |> select(account_name, market, risk_usd, r_method),
            by = c("account_name", "market")) |>
  mutate(day_r = day_pnl_usd / risk_usd)

# ---- Final columns (brief, Section 5) ----------------------------------------

trades_clean <- trades |>
  arrange(date, account_name, block_order) |>
  mutate(
    trade_id = row_number(),
    weekday  = as.character(wday(date, label = TRUE, abbr = TRUE, week_start = 1)),
    win      = pnl_usd > 0
  ) |>
  select(trade_id, date, weekday, account_type, account_name, trade_no, market,
         strategy, strategy_main, setup_grade, planned_rr, pnl_usd, contracts,
         risk_usd, r_method, r_multiple, win, rr_had_colon, rr_fixed, notes)

days_clean <- days |>
  arrange(date, account_name) |>
  mutate(
    day_id  = row_number(),
    weekday = as.character(wday(date, label = TRUE, abbr = TRUE, week_start = 1)),
    win     = day_pnl_usd > 0
  ) |>
  select(day_id, date, weekday, account_type, account_name, day_tag, market, pair,
         strategy, strategy_main, contracts, trades_taken, trades_journalled,
         day_pnl_usd, pnl_source, risk_usd, r_method, day_r, win, title_fixed)

write_csv(trades_clean, here("data", "processed", "trades_clean.csv"))
write_csv(days_clean,   here("data", "processed", "days_clean.csv"))
write_csv(risk_units, here("data", "processed", "risk_units.csv"))   # dollar values: private

# ---- Checks ------------------------------------------------------------------

stopifnot(!anyNA(days_clean$date), !anyNA(trades_clean$date))

message("\nDays per account type and market:")
days_clean |> count(account_type, market) |> print()

message("\nTrades per account type and grade:")
trades_clean |> count(account_type, setup_grade) |> print(n = Inf)

message("\nRisk units (1R) used:")
print(risk_units)

message("\nSmall-sample warning: groups under ", min_sample, " trades/days ",
        "cannot support conclusions.")
