# 01_extract_journal.R
# Pull the "Trading 2025" journal from the Notion API.
#
# Outputs (all in data/raw/, gitignored, personal data):
#   days_raw.csv               one row per day page, with database properties
#   blocks_raw.csv             every text line and image caption on each day page
#   trades_raw.csv             trade lines parsed from captions/text
#   trade_lines_unmatched.csv  lines starting with "Trade" that the regex missed
#
# Needs NOTION_TOKEN in .Renviron (see .Renviron.example).
# The integration must be connected to the "Trading 2025" page in Notion.

source(here::here("R", "00_setup.R"))
library(httr2)

# ---- Config ------------------------------------------------------------------

# Token is read inside notion_req(), never stored in a variable,
# so it does not show in the Environment pane or in screenshots.
if (Sys.getenv("NOTION_TOKEN") == "") {
  stop("NOTION_TOKEN is missing. Add it to .Renviron, then restart R.")
}

# Page ID of "Trading 2025". Not a secret: useless without the token.
root_page_id <- "2219295abc4f80799b35d7f57a4f20b5"

# Map each account page in Notion to account_name / account_type (brief, Section 5)
accounts <- tribble(
  ~page_title,                ~account_name,            ~account_type,
  "Back Testing Data",        "Backtest",               "backtest",
  "Forward Testing",          "Forward Test",           "forward_test",
  "50k APEX Funded Eval",     "APEX 50k Eval",          "eval",
  "50k TOPSTEP Combine (1)",  "TOPSTEP 50k Combine 1",  "eval",
  "6 x 50k TOPSTEP Combine",  "TOPSTEP 6x50k Combine",  "eval",
  "50k PA tradovate",         "APEX 50k PA",            "funded"
)

# ---- Notion API helpers ------------------------------------------------------

notion_req <- function(path) {
  request("https://api.notion.com/v1") |>
    req_url_path_append(path) |>
    req_headers(`Notion-Version` = "2022-06-28") |>
    req_auth_bearer_token(Sys.getenv("NOTION_TOKEN")) |>
    req_retry(max_tries = 5) |>
    req_throttle(rate = 3)          # Notion allows ~3 requests per second
}

# All child blocks of a page or block (handles pagination)
get_children <- function(block_id) {
  out <- list()
  cursor <- NULL
  repeat {
    resp <- notion_req(c("blocks", block_id, "children")) |>
      req_url_query(page_size = 100, start_cursor = cursor) |>
      req_perform() |>
      resp_body_json()
    out <- c(out, resp$results)
    if (!isTRUE(resp$has_more)) break
    cursor <- resp$next_cursor
  }
  out
}

# All pages (rows) in a database (handles pagination)
query_database <- function(db_id) {
  out <- list()
  cursor <- NULL
  repeat {
    body <- list(page_size = 100)
    if (!is.null(cursor)) body$start_cursor <- cursor
    resp <- notion_req(c("databases", db_id, "query")) |>
      req_body_json(body) |>
      req_perform() |>
      resp_body_json()
    out <- c(out, resp$results)
    if (!isTRUE(resp$has_more)) break
    cursor <- resp$next_cursor
  }
  out
}

# Rich text list -> one plain string
plain <- function(rt) paste(map_chr(rt, "plain_text"), collapse = "")

# One Notion property -> character value (cleaning happens in 02)
prop_value <- function(p) {
  switch(p$type,
    title        = plain(p$title),
    rich_text    = plain(p$rich_text),
    number       = if (is.null(p$number)) NA_character_ else as.character(p$number),
    select       = p$select$name %||% NA_character_,
    multi_select = paste(map_chr(p$multi_select, "name"), collapse = ";"),
    date         = p$date$start %||% NA_character_,
    checkbox     = as.character(p$checkbox),
    NA_character_
  )
}

# One database page -> one tibble row
page_to_row <- function(page) {
  props <- map(page$properties, prop_value)
  as_tibble(c(list(page_id = page$id, created_time = page$created_time), props))
}

# Text lines and image captions on a day page, in page order
page_lines <- function(page_id) {
  blocks <- get_children(page_id)
  empty <- tibble(block_order = integer(), block_type = character(), text = character())
  if (length(blocks) == 0) return(empty)   # day page with no content
  map(seq_along(blocks), \(i) {
    b <- blocks[[i]]
    content <- b[[b$type]]
    rt <- if (b$type == "image") content$caption else content$rich_text
    tibble(block_order = i, block_type = b$type, text = plain(rt %||% list()))
  }) |>
    list_rbind() |>
    filter(text != "")
}

# ---- 1. Find each account's database -----------------------------------------

account_pages <- get_children(root_page_id) |>
  keep(\(b) b$type == "child_page") |>
  map(\(b) tibble(page_title = str_trim(b$child_page$title), account_page_id = b$id)) |>
  list_rbind() |>
  inner_join(accounts, by = "page_title")

missing <- setdiff(accounts$page_title, account_pages$page_title)
if (length(missing) > 0) warning("Account pages not found: ", toString(missing))

account_dbs <- account_pages |>
  mutate(db_id = map_chr(account_page_id, \(id) {
    dbs <- get_children(id) |> keep(\(b) b$type == "child_database")
    if (length(dbs) == 0) return(NA_character_)
    if (length(dbs) > 1) warning("More than one database on page ", id, "; using the first.")
    dbs[[1]]$id
  }))

if (anyNA(account_dbs$db_id)) {
  warning("No database found on: ",
          toString(account_dbs$page_title[is.na(account_dbs$db_id)]))
}

# ---- 2. Day pages (one row per day) ------------------------------------------

days_raw <- account_dbs |>
  filter(!is.na(db_id)) |>
  mutate(rows = map(db_id, \(id) query_database(id) |> map(page_to_row) |> list_rbind())) |>
  select(account_name, account_type, rows) |>
  unnest(rows) |>
  rename_with(\(x) x |> str_trim() |> str_to_lower() |> str_replace_all("\\s+", "_"))

# Backtest database has no "Trades Taken" property; add it so later steps work
if (!"trades_taken" %in% names(days_raw)) days_raw$trades_taken <- NA_character_

message("Day pages found: ", nrow(days_raw))

# ---- 3. Page content (captions + text) ---------------------------------------
# One API call per day page, so this takes about a minute.

blocks_raw <- days_raw |>
  select(page_id, account_name, account_type, name) |>
  mutate(lines = map(page_id, page_lines, .progress = TRUE)) |>
  unnest(lines)

# ---- 4. Parse trade lines ----------------------------------------------------
# Formats seen in the journal:
#   "Trade 1 2rr -250pnl C Setup"   "Trade 2 3rr -250 B Setup"
#   "Trade 1 1:5rr 68pnl"           "Trade 4 3rr +1000pnl GAMBA"
#   "Trade 1 2.6rr - 200pnl C+"     "Trade 1 2rr BE pnl B- setup"
#   "Trade 2 3.5 +590pbl B+ setup"  "Trade 1 1:6rr +450rr +A Setup"
# RR is TradingView's risk/reward ratio. A colon is read as a decimal point:
# "1:5" = 1.5, "2:32" = 2.32. Flagged in rr_had_colon so 02 can review it.
# "BE" (break-even) = 0 PnL. "+A" is read as "A+".

trade_pattern <- regex(
  paste0(
    "^\\s*trade[sd]?\\s*(\\d+)\\s+",          # trade number
    "(-?[\\d.:]+)\\s*(?:rr)?\\s*",              # planned RR ("rr" sometimes missing)
    "([+-]?\\s*\\d+(?:\\.\\d+)?|BE)\\s*",   # PnL, or BE
    "(?:p[a-z]{1,2}|rr)?\\s*",                    # "pnl" (typos: "pbl", "rr")
    "(.*)$"                                       # rest: grade, tags, notes
  ),
  ignore_case = TRUE
)

trade_lines <- blocks_raw |>
  filter(str_detect(text, regex("^\\s*trade", ignore_case = TRUE)))

m <- str_match(trade_lines$text, trade_pattern)

# Grade like "C", "B-", "A+", "+A"; case-sensitive on purpose ("GAMBA" is not A)
g <- str_match(m[, 5], "(?:^|\\s)([+-]?)([ABCE])([+-]?)(?=\\s|$)")

trades_raw <- trade_lines |>
  mutate(
    matched      = !is.na(m[, 1]),
    trade_no     = as.integer(m[, 2]),
    rr_text      = m[, 3],
    rr_had_colon = str_detect(rr_text, ":"),
    planned_rr   = as.numeric(str_replace(rr_text, ":", ".")),
    pnl_text     = str_remove_all(m[, 4], "\\s"),
    pnl_usd      = if_else(str_to_upper(pnl_text) == "BE", 0, suppressWarnings(as.numeric(pnl_text))),
    rest         = str_trim(m[, 5]),
    setup_grade  = if_else(is.na(g[, 3]), NA_character_,
                           paste0(g[, 3], if_else(g[, 4] != "", g[, 4], g[, 2])))
  )

unmatched <- trades_raw |> filter(!matched) |> select(account_name, name, text)
trades_raw <- trades_raw |> filter(matched) |> select(-matched)

# ---- 5. Save + quick checks --------------------------------------------------

write_csv(days_raw,   here("data", "raw", "days_raw.csv"))
write_csv(blocks_raw, here("data", "raw", "blocks_raw.csv"))
write_csv(trades_raw, here("data", "raw", "trades_raw.csv"))
write_csv(unmatched,  here("data", "raw", "trade_lines_unmatched.csv"))

# Same trade number twice on one day = check in Notion
dupes <- trades_raw |> count(account_name, name, trade_no) |> filter(n > 1)
if (nrow(dupes) > 0) { message("Duplicate trade numbers:"); print(dupes) }

message("Trade lines parsed: ", nrow(trades_raw),
        " | unmatched: ", nrow(unmatched),
        " | RR with colon: ", sum(trades_raw$rr_had_colon))

# Journal completeness: parsed trades vs "Trades Taken" property per account
trades_raw |>
  count(account_name, name, name = "parsed") |>
  right_join(days_raw |> select(account_name, name, trades_taken),
             by = c("account_name", "name")) |>
  mutate(parsed = replace_na(parsed, 0L)) |>
  group_by(account_name) |>
  summarise(days = n(),
            trades_parsed = sum(parsed),
            trades_taken  = sum(as.numeric(trades_taken), na.rm = TRUE)) |>
  print()


library(httr2)
request("https://api.notion.com/v1/users/me") |>
  req_headers(`Notion-Version` = "2022-06-28") |>
  req_auth_bearer_token(Sys.getenv("NOTION_TOKEN")) |>
  req_perform() |>
  resp_status()
