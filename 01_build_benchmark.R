# 01_build_benchmark.R
# Purpose: turn the raw WFP price files into the benchmark —
#   one row per country x product x month, with the median retail price
#   across markets (the "right answer"), a tolerance band (10th–90th
#   percentile), two controls, and the World Bank income group in force
#   that month.
# Input:  data_raw/wfp_food_prices_*.csv, data_raw/OGHIST_*.xlsx
# Output: data_clean/benchmark.csv, output/tables/coverage.csv
# Why some countries drop out: see 01b_check_coverage.R

library(readr); library(dplyr); library(tidyr); library(stringr)
library(lubridate); library(purrr); library(readxl); library(here)

# ---- 1. Read all WFP files, drop the tag row, add the country code ---------
files <- list.files(here("data_raw"), pattern = "^wfp_food_prices_.*\\.csv$",
                    full.names = TRUE)
read_wfp <- function(f) {
  read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
    filter(!str_starts(date, "#")) |>                 # remove the "#date ..." tag row
    mutate(iso3 = toupper(str_match(basename(f), "wfp_food_prices_([a-z]{3})")[, 2]))
}
raw <- map_dfr(files, read_wfp) |>
  mutate(date = as.Date(date),
         price = as.numeric(price),
         usdprice = as.numeric(usdprice))
cat("Files:", length(files), "| rows read:", nrow(raw), "\n")

# ---- 2. Filters ------------------------------------------------------------
# Retail only: wholesale is a different price.
# Price flags: "actual" and "aggregate" are both observed market prices
# ("aggregate" = monthly value computed from more frequent collection).
# share_actual (step 5) allows a robustness check with "actual" only.
# 2022 is kept only to measure volatility in the 12 months before 2023.
prices <- raw |>
  filter(pricetype == "Retail",
         priceflag %in% c("actual", "aggregate", "actual,aggregate"),
         date >= as.Date("2022-01-01"), date <= as.Date("2024-12-31"),
         !is.na(price), price > 0)
cat("After retail / flag / 2022–24 filter:", nrow(prices), "\n")

# ---- 3. Six product groups -------------------------------------------------
# The exact WFP name is kept (it goes into the question);
# the group is used for the fixed effect.
prices <- prices |>
  mutate(c_low = str_to_lower(commodity),
         group = case_when(
           str_detect(c_low, "^maize") & !str_detect(c_low, "flour|meal") ~ "maize",
           str_detect(c_low, "^rice")                                     ~ "rice",
           str_detect(c_low, "wheat flour|^flour \\(wheat")              ~ "wheat_flour",
           str_detect(c_low, "^beans")                                    ~ "beans",
           str_detect(c_low, "^sugar")                                    ~ "sugar",
           str_detect(c_low, "^oil")                                      ~ "oil",
           TRUE ~ NA_character_)) |>
  filter(!is.na(group))
cat("After keeping the six product groups:", nrow(prices), "\n")

# ---- 4. Convert every price to per kilogram (or per litre) -----------------
# Units look like "KG", "50 KG", "100 Pounds", "Tin (20 L)".
to_base <- c(KG = 1, G = 0.001, MT = 1000, POUND = 0.453592, POUNDS = 0.453592,
             LB = 0.453592, L = 1, ML = 0.001)
prices <- prices |>
  mutate(unit_up   = str_to_upper(str_squish(unit)),
         # "TIN (20 L)" -> "20 L": take the quantity inside the brackets
         unit_up   = str_replace(unit_up, "^.*\\(([0-9.]+) ?([A-Z]+)\\)$", "\\1 \\2"),
         unit_qty  = suppressWarnings(as.numeric(str_extract(unit_up, "^[0-9.]+"))),
         unit_qty  = coalesce(unit_qty, 1),
         unit_name = str_squish(str_remove(unit_up, "^[0-9.]+")),
         conv      = unname(to_base[unit_name]),
         unit_base = case_when(unit_name %in% c("L", "ML") ~ "L",
                               !is.na(conv)                ~ "KG",
                               TRUE                        ~ NA_character_))
cat("\nUnits we cannot convert (dropped):\n")
print(prices |> filter(is.na(unit_base)) |> count(iso3, unit, sort = TRUE), n = Inf)
prices <- prices |>
  filter(!is.na(unit_base)) |>
  mutate(p     = price    / (unit_qty * conv),     # local currency per KG or L
         p_usd = usdprice / (unit_qty * conv))

# ---- 5. One row per country x exact product x month ------------------------
cells <- prices |>
  mutate(month = floor_date(date, "month")) |>
  group_by(iso3, group, commodity, unit_base, currency, month) |>
  summarise(n_markets    = n_distinct(market),
            p_star       = median(p),              # the "right answer"
            p10          = quantile(p, 0.10),      # tolerance band, low
            p90          = quantile(p, 0.90),      # tolerance band, high
            p_star_usd   = median(p_usd),
            share_actual = mean(priceflag == "actual"),
            .groups = "drop")

# ---- 6. Controls -----------------------------------------------------------
# vol12: sd of monthly log changes in the 12 months before
# dispersion: log(p90 / p10) across markets in the same month
vol_prev12 <- function(month, p) {
  map_dbl(seq_along(month), function(k) {
    w <- month >= (month[k] %m-% months(12)) & month < month[k]
    s <- p[w][order(month[w])]
    if (length(s) < 6) return(NA_real_)
    sd(diff(log(s)))
  })
}
cells <- cells |>
  group_by(iso3, commodity, unit_base, currency) |>
  mutate(vol12 = vol_prev12(month, p_star)) |>
  ungroup() |>
  mutate(dispersion = log(p90 / p10))

# ---- 7. Keep 2023–24 and cells with at least 3 markets ---------------------
benchmark <- cells |>
  filter(month >= as.Date("2023-01-01"), n_markets >= 3)

# ---- 8. World Bank income group in force each month ------------------------
# Layout of the file: column 1 = country code, column 2 = name,
# then one column per fiscal year (FY89 ... FY27).
oh_file <- list.files(here("data_raw"), pattern = "^OGHIST.*\\.xlsx$", full.names = TRUE)
oh  <- read_excel(oh_file, sheet = "Country Analytical History",
                  col_names = FALSE, skip = 4)
hdr <- as.character(unlist(oh[1, ]))
hdr <- ifelse(is.na(hdr), paste0("x", seq_along(hdr)), hdr)
names(oh) <- c("iso3", "country", hdr[-(1:2)])
income_hist <- oh |>
  slice(-(1:2)) |>                                   # drop the two header rows
  filter(str_detect(iso3, "^[A-Z]{3}$")) |>
  select(iso3, country, starts_with("FY")) |>
  pivot_longer(starts_with("FY"), names_to = "fy", values_to = "income_code") |>
  mutate(income_code = str_extract(income_code, "^(LM|UM|L|H)"))  # LM before L!

# The fiscal year runs July–June: Jan 2023 = FY23, Jul 2023 = FY24, Jul 2024 = FY25
benchmark <- benchmark |>
  mutate(fy = sprintf("FY%02d",
                      (year(month) + (lubridate::month(month) >= 7)) %% 100)) |>
  left_join(income_hist, by = c("iso3", "fy")) |>
  mutate(income = factor(income_code, levels = c("L", "LM", "UM", "H"),
                         labels = c("Low", "Lower-middle", "Upper-middle", "High")))

cat("\nIncome group by country during 2023–24 (an arrow = it changed):\n")
groups <- benchmark |>
  arrange(month) |>
  group_by(iso3, country) |>
  summarise(income = paste(unique(as.character(income)), collapse = " -> "),
            .groups = "drop")
print(groups, n = Inf)
if (any(is.na(benchmark$income))) warning("Some cells have no income group.")

# ---- 9. Coverage and save --------------------------------------------------
coverage <- benchmark |>
  count(iso3, group, name = "months") |>
  pivot_wider(names_from = group, values_from = months, values_fill = 0) |>
  left_join(groups, by = "iso3") |>
  relocate(country, income, .after = iso3)
cat("\nBenchmark cells:", nrow(benchmark), "\n")
print(coverage, n = Inf, width = Inf)

write_csv(benchmark, here("data_clean", "benchmark.csv"))
write_csv(coverage,  here("output", "tables", "coverage.csv"))
cat("\nSaved: data_clean/benchmark.csv and output/tables/coverage.csv\n")