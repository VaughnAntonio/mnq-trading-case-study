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


# ---- Metric helpers (used by 04, 05) ----------------------------------------

# Bootstrap 95% confidence interval for the mean.
# Idea: resample the trades many times (with replacement), take the mean each
# time, and see where the middle 95% of those means fall. Wide interval =
# we are unsure. If it crosses 0, we cannot say the edge is real.
boot_ci <- function(x, reps = 2000) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(ci_low = NA_real_, ci_high = NA_real_))
  means <- replicate(reps, mean(sample(x, replace = TRUE)))
  c(ci_low = quantile(means, 0.025, names = FALSE),
    ci_high = quantile(means, 0.975, names = FALSE))
}

# Standard performance table for any grouping
perf_summary <- function(df, pnl, r, ...) {
  df |>
    group_by(...) |>
    summarise(
      n              = n(),
      win_rate       = mean({{ pnl }} > 0),
      avg_win_usd    = mean({{ pnl }}[{{ pnl }} > 0]),
      avg_loss_usd   = mean({{ pnl }}[{{ pnl }} < 0]),
      expectancy_usd = mean({{ pnl }}),
      expectancy_r   = mean({{ r }}),
      ci             = list(boot_ci({{ r }})),
      profit_factor  = sum({{ pnl }}[{{ pnl }} > 0]) / abs(sum({{ pnl }}[{{ pnl }} < 0])),
      .groups = "drop"
    ) |>
    unnest_wider(ci) |>
    rename(expectancy_r_ci_low = ci_low, expectancy_r_ci_high = ci_high) |>
    mutate(enough_data = n >= min_sample)
}

# Shared plot for "expectancy in R with 95% CI"
plot_expectancy <- function(tbl, x, title) {
  tbl |>
    ggplot(aes({{ x }}, expectancy_r, colour = enough_data)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_pointrange(aes(ymin = expectancy_r_ci_low, ymax = expectancy_r_ci_high)) +
    geom_text(aes(label = paste0("n=", n)), vjust = -1.2, size = 3, colour = "grey30") +
    facet_wrap(~ account_type, nrow = 1) +
    scale_colour_manual(values = c(`TRUE` = "#2b6cb0", `FALSE` = "#a0aec0"),
                        labels = c(`TRUE` = "n >= 30", `FALSE` = "n < 30 (not conclusive)"),
                        name = NULL) +
    labs(title = title, x = NULL, y = "Expectancy (R per trade/day)") +
    theme(legend.position = "bottom")
}

