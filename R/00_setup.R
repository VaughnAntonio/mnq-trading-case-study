# 00_setup.R
# Packages, plot theme and helpers shared by every script.
# First run only (in the R console, not in this script):
#   install.packages(c("renv", "here"))
#   renv::init()
#   install.packages(c("tidyverse", "lubridate", "tidyquant", "quantmod",
#                      "PerformanceAnalytics", "TTR", "scales", "gt",
#                      "httr2", "quarto"))
#   renv::snapshot()

library(tidyverse)
library(lubridate)
library(here)
library(scales)

# Account types, always kept separate in metrics
account_levels <- c("backtest", "forward_test", "eval", "funded")

# MNQ contract value
mnq_point_value <- 2  # USD per point per contract

# Minimum trades before we draw a conclusion from a group
min_sample <- 30

theme_set(theme_minimal(base_size = 12))

# Save a ggplot to outputs/figures/
save_fig <- function(plot, name, width = 8, height = 5) {
  ggsave(here("outputs", "figures", paste0(name, ".png")),
         plot, width = width, height = height, dpi = 300)
}

# Save a table to outputs/tables/
save_tbl <- function(tbl, name) {
  write_csv(tbl, here("outputs", "tables", paste0(name, ".csv")))
}

