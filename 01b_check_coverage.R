# 01b_check_coverage.R
# Purpose: document why some countries drop out of the benchmark.
# Needs the object `raw` from 01_build_benchmark.R (Source that first).

library(dplyr); library(here)

# ---- 1. First batch: Colombia, Guatemala, Malawi, Ghana, Kenya --------------
check <- c("COL", "GTM", "MWI", "GHA", "KEN")
d <- raw |> filter(iso3 %in% check)

in_period <- d$date >= as.Date("2022-01-01") & d$date <= as.Date("2024-12-31")
funnel <- d |>
  mutate(period = in_period,
         retail = period & pricetype == "Retail",
         actual = retail & priceflag == "actual") |>
  group_by(iso3) |>
  summarise(all_rows = n(),
            in_2022_24 = sum(period, na.rm = TRUE),
            retail = sum(retail, na.rm = TRUE),
            retail_actual = sum(actual, na.rm = TRUE))
cat("\n1. Rows surviving each filter:\n"); print(funnel)

cat("\n2. Markets and products in 2023–24, by price type and flag:\n")
d |> filter(date >= as.Date("2023-01-01"), date <= as.Date("2024-12-31")) |>
  group_by(iso3, pricetype, priceflag) |>
  summarise(markets = n_distinct(market), products = n_distinct(commodity),
            example_market = first(market), .groups = "drop") |>
  print(n = Inf)

# ---- 2. Second batch: Ecuador, Namibia, Moldova, Tanzania -------------------
check2 <- c("ECU", "NAM", "MDA", "TZA")
cat("\n3. Second batch, what exists in 2023–24:\n")
raw |>
  filter(iso3 %in% check2,
         date >= as.Date("2023-01-01"), date <= as.Date("2024-12-31")) |>
  group_by(iso3, pricetype, priceflag) |>
  summarise(rows = n(), markets = n_distinct(market),
            products = n_distinct(commodity), .groups = "drop") |>
  print(n = Inf)
# Findings: COL and TZA wholesale only; GTM and MDA one market; ECU two markets;
# PER data end in June 2022; NAM no data in 2023–24.
