# 05_shocks.R
# Question: are the AI's price errors explained by recent price shocks
#           (inflation, devaluation) rather than by income level?
# Idea ("stale knowledge"): if the model remembers older prices, it should
#   UNDER-estimate prices that rose fast, and the error should grow with the shock.
# Two shock measures, both built from the WFP files you already have:
#   drift  = recent change in the local price of the same product:
#            ln p*(t-1) - ln p*(t-12), i.e. the 11 months BEFORE the question month.
#            Month t itself is left out on purpose: the answer key p*(t) is measured
#            with noise, and that noise would otherwise appear in both the error and
#            the shock, creating a fake negative relationship.
#   dep12  = 12-month currency depreciation: ln xr(t-12) - ln xr(t), where
#            xr = US dollars per unit of local currency (WFP usdprice / price).
#            Positive = the local currency lost value.
# Input:  data_raw/wfp_food_prices_*.csv, data_clean/analysis.csv
# Output: output/tables/shocks_*.csv, output/tables/shocks_regressions.tex,
#         output/figures/stale_knowledge.png

library(readr); library(dplyr); library(tidyr); library(stringr)
library(lubridate); library(purrr); library(fixest); library(ggplot2); library(here)

# ---- 1. Rebuild monthly median prices for 2022–24 (same rules as 01) -------
files <- list.files(here("data_raw"), pattern = "^wfp_food_prices_.*\\.csv$", full.names = TRUE)
raw <- map_dfr(files, function(f)
  read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
    filter(!str_starts(date, "#")) |>
    mutate(iso3 = toupper(str_match(basename(f), "wfp_food_prices_([a-z]{3})")[, 2]))) |>
  mutate(date = as.Date(date), price = as.numeric(price), usdprice = as.numeric(usdprice),
         month = floor_date(date, "month")) |>
  filter(date >= as.Date("2022-01-01"), date <= as.Date("2024-12-31"),
         !is.na(price), price > 0)

to_base <- c(KG = 1, G = 0.001, MT = 1000, POUND = 0.453592, POUNDS = 0.453592,
             LB = 0.453592, L = 1, ML = 0.001)
cells <- raw |>
  filter(pricetype == "Retail",
         priceflag %in% c("actual", "aggregate", "actual,aggregate")) |>
  mutate(unit_up   = str_to_upper(str_squish(unit)),
         unit_up   = str_replace(unit_up, "^.*\\(([0-9.]+) ?([A-Z]+)\\)$", "\\1 \\2"),
         unit_qty  = coalesce(suppressWarnings(as.numeric(str_extract(unit_up, "^[0-9.]+"))), 1),
         unit_name = str_squish(str_remove(unit_up, "^[0-9.]+")),
         conv      = unname(to_base[unit_name]),
         unit_base = case_when(unit_name %in% c("L", "ML") ~ "L",
                               !is.na(conv)                ~ "KG",
                               TRUE                        ~ NA_character_)) |>
  filter(!is.na(unit_base)) |>
  mutate(p = price / (unit_qty * conv)) |>
  group_by(iso3, commodity, unit_base, currency, month) |>
  summarise(p_star = median(p), .groups = "drop")

# ---- 2. Price drift: ln p*(t-1) - ln p*(t-12) -------------------------------
# If a month is missing, the month before it is used (t-2 instead of t-1, t-13 instead of t-12).
keys <- c("iso3", "commodity", "unit_base", "currency")
lag_price <- function(k) cells |> mutate(month = month %m+% months(k)) |>
  select(all_of(keys), month, p = p_star)          # p = price k months before 'month'
drift <- cells |>
  select(all_of(keys), month) |>
  left_join(rename(lag_price(1),  p1  = p), by = c(keys, "month")) |>
  left_join(rename(lag_price(2),  p2  = p), by = c(keys, "month")) |>
  left_join(rename(lag_price(12), p12 = p), by = c(keys, "month")) |>
  left_join(rename(lag_price(13), p13 = p), by = c(keys, "month")) |>
  mutate(drift = log(coalesce(p1, p2) / coalesce(p12, p13))) |>
  select(all_of(keys), month, drift)

# ---- 3. Currency depreciation over 12 months --------------------------------
# Implied exchange rate = usdprice / price, median over all items in a country-month.
# Very small USD prices are skipped because their rounding makes the ratio noisy.
xr <- raw |>
  filter(!is.na(usdprice), usdprice >= 0.1) |>
  group_by(iso3, currency, month) |>
  summarise(xr = median(usdprice / price), .groups = "drop")
xr <- xr |>
  left_join(xr |> mutate(month = month %m+% months(12)) |> select(iso3, currency, month, xr12 = xr),
            by = c("iso3", "currency", "month")) |>
  mutate(dep12 = log(xr12 / xr))

cat("1. Sanity check - 12-month depreciation in December 2023 and December 2024\n",
    "   (Nigeria and Malawi should be large and positive):\n")
print(xr |> filter(month %in% as.Date(c("2023-12-01", "2024-12-01"))) |>
        select(iso3, currency, month, dep12) |>
        pivot_wider(names_from = month, values_from = dep12) |>
        arrange(iso3), n = Inf)

# ---- 4. Add the shocks to the analysis data ---------------------------------
d <- read_csv(here("data_clean", "analysis.csv"), show_col_types = FALSE) |>
  mutate(month = as.Date(month)) |>
  left_join(drift, by = c(keys, "month")) |>
  left_join(select(xr, iso3, currency, month, dep12), by = c("iso3", "currency", "month")) |>
  mutate(e         = log(p_hat / answer_true),   # SIGNED error: < 0 = AI price too low
         abs_drift = abs(drift),
         post      = as.integer(month > as.Date("2024-06-01")),   # after the model's
         drift_post = drift * post)                               # training cutoff (Jun 2024)
dep_c <- d |> filter(type == "price") |>                  # country average, for the placebo
  group_by(iso3) |> summarise(dep_c = mean(dep12, na.rm = TRUE))
d  <- left_join(d, dep_c, by = "iso3")
pr <- filter(d, type == "price")
write_csv(d, here("data_clean", "analysis_shocks.csv"))   # used by 06_robustness.R
cat("\n2. Price answers with a drift value:", sum(!is.na(pr$drift)), "of", nrow(pr),
    "| with a depreciation value:", sum(!is.na(pr$dep12)), "\n")

# ---- 5. Regressions (product x month fixed effects, SE clustered by country) --
ctrl <- "vol12_f + vol12_na + disp_f"
s1 <- feols(e ~ drift | cell, pr, cluster = ~iso3)                                   # stale knowledge
s2 <- feols(as.formula(paste("y ~ abs_drift + inc_low + inc_lmid +", ctrl, "| cell")),
            pr, cluster = ~iso3)                                                     # size of shock
s3 <- feols(as.formula(paste("y ~ dep12 + inc_low + inc_lmid +", ctrl, "| cell")),
            pr, cluster = ~iso3)                                                     # devaluation
s4 <- feols(e ~ drift + drift_post | cell, pr, cluster = ~iso3)                      # after cutoff
s0 <- feols(as.formula(paste("y ~ inc_low + inc_lmid +", ctrl, "| cell")),           # main model on
            filter(pr, !is.na(drift)), cluster = ~iso3)                              # s2's sample
p1 <- feols(y ~ dep_c | cell, filter(d, type == "control"), cluster = ~iso3)         # placebo
mods  <- list(s1, s4, s0, s2, s3, p1)
heads <- c("Signed e", "Signed e", "|e| no shock", "|e| + |drift|", "|e| + deval.", "Placebo")
cat("\n3. Regressions:\n")
print(etable(mods, headers = heads, keep = "drift|dep|inc_"))
etable(mods, headers = heads, keep = "drift|dep|inc_",
       file = here("output", "tables", "shocks_regressions.tex"), replace = TRUE)

# ---- 6. Wild cluster bootstrap (same method as 04) --------------------------
wild_boot <- function(dd, yvar, xvars, fe, param, cluster = "iso3",
                      B = 9999, seed = 20261020) {
  dd <- as.data.frame(dd)
  dd <- dd[complete.cases(dd[, c(yvar, xvars, fe, cluster)]), ]
  dm <- fixest::demean(as.matrix(dd[, c(yvar, xvars)]), dd[, fe, drop = FALSE])
  y  <- dm[, 1]
  X  <- dm[, -1, drop = FALSE]
  X  <- X[, c(param, setdiff(colnames(X), param)), drop = FALSE]
  qx <- qr(X); X <- X[, qx$pivot[seq_len(qx$rank)], drop = FALSE]
  k  <- match(param, colnames(X))
  cl <- as.integer(factor(dd[[cluster]])); G <- max(cl)
  XtXi <- solve(crossprod(X))
  tstat <- function(yy) {
    b <- XtXi %*% crossprod(X, yy)
    u <- as.vector(yy - X %*% b)
    S <- rowsum(X * u, cl)
    V <- XtXi %*% crossprod(S) %*% XtXi
    c(b[k], b[k] / sqrt(V[k, k]))
  }
  obs <- tstat(y)
  Xr  <- X[, -k, drop = FALSE]
  fr  <- if (ncol(Xr) > 0) as.vector(Xr %*% solve(crossprod(Xr), crossprod(Xr, y))) else rep(0, length(y))
  ur  <- y - fr
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  set.seed(seed)
  tb <- replicate(B, { v <- sample(webb, G, replace = TRUE); tstat(fr + ur * v[cl])[2] })
  tibble(param = param, estimate = obs[1], t = obs[2],
         p_boot = mean(abs(tb) >= abs(obs[2])), n = nrow(dd), clusters = G)
}
cv <- c("vol12_f", "vol12_na", "disp_f")
cat("\n4. Wild cluster bootstrap p-values (about a minute)...\n")
wb <- bind_rows(
  wild_boot(pr, "e", "drift",                                   "cell", "drift")      |> mutate(model = "Signed e"),
  wild_boot(pr, "e", c("drift", "drift_post"),                  "cell", "drift_post") |> mutate(model = "Signed e, after cutoff"),
  wild_boot(pr, "y", c("abs_drift", "inc_low", "inc_lmid", cv), "cell", "abs_drift")  |> mutate(model = "|e| + |drift|"),
  wild_boot(pr, "y", c("dep12", "inc_low", "inc_lmid", cv),     "cell", "dep12")      |> mutate(model = "|e| + deval."),
  wild_boot(pr, "y", c("dep12", "inc_low", "inc_lmid", cv),     "cell", "inc_low")    |> mutate(model = "|e| + deval."),
  wild_boot(filter(d, type == "control"), "y", "dep_c",         "cell", "dep_c")      |> mutate(model = "Placebo")
) |> relocate(model)
print(wb, width = Inf)
write_csv(wb, here("output", "tables", "shocks_bootstrap.csv"))

# ---- 7. Country table: signed error next to the shocks ----------------------
ct <- pr |> group_by(iso3, country, income) |>
  summarise(n = n(), mean_abs_error = mean(y, na.rm = TRUE),
            mean_signed_error = mean(e, na.rm = TRUE),
            mean_drift = mean(drift, na.rm = TRUE), mean_dep12 = mean(dep12, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(mean_abs_error))
cat("\n5. By country (signed error < 0 = AI price too low):\n")
print(ct |> mutate(across(where(is.double), ~ round(.x, 2))), n = Inf, width = Inf)
write_csv(ct, here("output", "tables", "shocks_by_country.csv"))

# ---- 8. Figure: signed error against the recent price change ----------------
bins <- pr |> filter(!is.na(drift), !is.na(e)) |>
  mutate(bin = ntile(drift, 15)) |>
  group_by(bin) |> summarise(drift = mean(drift), e = mean(e), n = n(), .groups = "drop")
g <- ggplot(bins, aes(drift, e)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 2.5) +
  geom_smooth(data = filter(pr, !is.na(drift), !is.na(e)), method = "lm", formula = y ~ x,
              se = FALSE, colour = "black", linewidth = 0.6) +
  annotate("text", x = max(bins$drift), y = -max(bins$drift) + 0.15, hjust = 1, vjust = -0.6, size = 3.2,
           label = "dashed: AI reports last year's price") +
  labs(x = "Recent price change, ln p(t-1) - ln p(t-12)",
       y = "Signed AI error, ln(AI price / market price)",
       caption = "Points: averages within 15 equal-sized bins. Line: OLS fit on all answers.") +
  coord_cartesian(xlim = range(bins$drift) + c(-0.15, 0.15),        # zoom on the bins;
                  ylim = range(bins$e) + c(-0.25, 0.25)) +           # the fit uses all data
  theme_minimal(base_size = 12)
ggsave(here("output", "figures", "stale_knowledge.png"), g, width = 7, height = 4.5, dpi = 300)

# ---- 9. Result in one sentence ----------------------------------------------
cat(sprintf(paste0("\n6. When a product's local price rose 10%% over the previous year, the AI's estimate",
                   " moves by %+.1f%% relative to the market price (bootstrap p = %.3f).",
                   " Slope 0 = AI fully up to date; slope -1 = AI reports last year's price.\n"),
            10 * coef(s1)["drift"], wb$p_boot[1]))
