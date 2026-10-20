# 04_analyse.R
# Purpose: pull the number out of each answer, compute the error y,
#          estimate the regressions fixed in the note (§ 4–6),
#          save tables and one figure.
# Input:  data_clean/questions.csv, data_clean/responses.csv,
#         data_raw/CLASS_*.xlsx (region), data_raw/API_NY.GNP.PCAP.CD*.csv (GNI)
# Output: data_clean/analysis.csv, output/tables/*, output/figures/*

library(readr); library(dplyr); library(tidyr); library(stringr)
library(readxl); library(fixest); library(ggplot2); library(here)

# ---- 1. Pull the number out of each answer ----------------------------------
# "1,250" or "1 250" -> 1250; then take the first number in the text
parse_num <- function(x) {
  x <- str_replace_all(x, "(?<=\\d)[,\\s](?=\\d{3}(\\D|$))", "")
  as.numeric(str_extract(x, "-?\\d+(\\.\\d+)?"))
}

q   <- read_csv(here("data_clean", "questions.csv"), show_col_types = FALSE)
res <- read_csv(here("data_clean", "responses.csv"), show_col_types = FALSE) |>
  filter(status == "ok") |>
  distinct(qid, rep, .keep_all = TRUE)

d <- res |>
  mutate(p_hat = parse_num(response_text)) |>
  left_join(q, by = "qid") |>
  mutate(valid   = !is.na(p_hat) & p_hat > 0,
         # the error: absolute log deviation from the right answer
         y       = if_else(valid, abs(log(p_hat / answer_true)), NA_real_),
         in_band = if_else(type == "price" & valid, p_hat >= p10 & p_hat <= p90, NA),
         exact   = if_else(type == "control" & valid, abs(p_hat - answer_true) < 0.5, NA),
         local   = as.integer(type == "price"))
cat("Answers without a usable number:", sum(!d$valid), "of", nrow(d), "\n")

# ---- 2. Region and GNI per capita -------------------------------------------
cls <- read_excel(list.files(here("data_raw"), "^CLASS.*\\.xlsx$", full.names = TRUE)[1],
                  sheet = "List of economies") |>
  select(iso3 = Code, region = Region)

gni_file <- list.files(here("data_raw"), pattern = "^API_NY\\.GNP\\.PCAP\\.CD.*\\.csv$",
                       full.names = TRUE, recursive = TRUE)[1]   # also looks inside folders
if (is.na(gni_file)) stop("GNI file not found. Run: list.files(here('data_raw'), recursive = TRUE)")
gni <- read_csv(gni_file, skip = 4, show_col_types = FALSE) |>
  select(iso3 = `Country Code`, g2022 = `2022`, g2023 = `2023`) |>
  mutate(log_gni = log(coalesce(g2023, g2022))) |>
  select(iso3, log_gni)

d <- d |>
  left_join(cls, by = "iso3") |>
  left_join(gni, by = "iso3") |>
  mutate(inc_low  = as.integer(income == "Low"),            # reference: Upper-middle
         inc_lmid = as.integer(income == "Lower-middle"),
         cell     = if_else(type == "price", paste(group, month), paste("ctrl", template)),
         vol12_f  = coalesce(vol12, 0),                    # missing volatility -> 0 ...
         vol12_na = as.integer(is.na(vol12)),              # ... plus a dummy for it
         disp_f   = coalesce(dispersion, 0)) |>
  group_by(iso3, type) |> mutate(w_country = 1 / n()) |> ungroup()   # R1 weights
write_csv(d, here("data_clean", "analysis.csv"))

# ---- 3. Descriptives --------------------------------------------------------
desc <- d |>
  group_by(type, income) |>
  summarise(n = n(), mean_y = mean(y, na.rm = TRUE), median_y = median(y, na.rm = TRUE),
            share_in_band = mean(in_band, na.rm = TRUE),
            share_exact = mean(exact, na.rm = TRUE), .groups = "drop")
cat("\n1. Error by question type and income group:\n"); print(desc, width = Inf)
write_csv(desc, here("output", "tables", "descriptives.csv"))

rep_check <- d |> select(qid, rep, p_hat) |>
  pivot_wider(names_from = rep, values_from = p_hat, names_prefix = "r")
cat("\n2. Identical answer in both repeats:",
    round(100 * mean(rep_check$r1 == rep_check$r2, na.rm = TRUE), 1), "%\n")

# ---- 4. Regressions ---------------------------------------------------------
pr <- filter(d, type == "price")
ctrl_vars <- "vol12_f + vol12_na + disp_f"
f <- function(rhs, fe = "cell") as.formula(paste("y ~", rhs, "+", ctrl_vars, "|", fe))

m_main <- feols(f("inc_low + inc_lmid"), pr, cluster = ~iso3)               # § 4
m_pool <- feols(y ~ inc_low + inc_lmid + inc_low:local + inc_lmid:local +   # § 5
                  local:vol12_f + local:vol12_na + local:disp_f | cell,
                d, cluster = ~iso3)

m_r1 <- feols(f("inc_low + inc_lmid"), pr, cluster = ~iso3, weights = ~w_country)
pr_c <- pr |>
  group_by(iso3, region, cell, inc_low, inc_lmid, log_gni) |>
  summarise(y = mean(y, na.rm = TRUE), vol12_f = mean(vol12_f),
            vol12_na = max(vol12_na), disp_f = mean(disp_f), .groups = "drop")
m_r2 <- feols(f("inc_low + inc_lmid"), pr_c, cluster = ~iso3)
sig2 <- tibble(iso3 = pr$iso3[obs(m_main)], r = resid(m_main)) |>
  group_by(iso3) |> summarise(s2 = var(r))
pr   <- pr |> left_join(sig2, by = "iso3") |> mutate(w_ivar = 1 / s2)
m_r3 <- feols(f("inc_low + inc_lmid"), pr, cluster = ~iso3, weights = ~w_ivar)
m_r4 <- feols(f("log_gni"), pr, cluster = ~iso3)
m_r5 <- feols(f("inc_low + inc_lmid", "cell + region"), pr, cluster = ~iso3)
m_r6 <- feols(f("inc_low"), filter(pr, region == "Sub-Saharan Africa"), cluster = ~iso3)
m_r7 <- feols(f("inc_low + inc_lmid"), filter(pr, share_actual == 1), cluster = ~iso3)

models <- list(m_main, m_pool, m_r1, m_r2, m_r3, m_r4, m_r5, m_r6, m_r7)
heads  <- c("Main", "Pooled", "R1 eq.wts", "R2 collapsed", "R3 inv.var",
            "R4 log GNI", "R5 region FE", "R6 Africa", "R7 actual")
cat("\n3. Regressions (clustered SE by country):\n")
print(etable(models, headers = heads, keep = "inc_|log_gni"))
etable(models, headers = heads, keep = "inc_|log_gni",
       file = here("output", "tables", "regressions.tex"), replace = TRUE)

# ---- 5. Wild cluster bootstrap p-values (few clusters) ----------------------
# Written by hand (no extra package). Method: "restricted wild cluster bootstrap-t"
# (Cameron, Gelbach & Miller 2008) with Webb six-point weights (Webb 2023),
# recommended when there are few clusters (here about 21 countries).
# Steps: (1) remove the fixed effects by demeaning every variable within its FE group;
# (2) fit the model with the tested coefficient forced to 0 (the "null" model);
# (3) many times: flip the null residuals country by country with a random weight,
#     rebuild a fake y, re-estimate, and store the t-statistic;
# (4) p-value = share of fake |t| at least as large as the real |t|.
wild_boot <- function(dd, yvar, xvars, fe, param, cluster = "iso3",
                      B = 9999, seed = 20261020) {
  dd <- as.data.frame(dd)
  dd <- dd[complete.cases(dd[, c(yvar, xvars, fe, cluster)]), ]
  dm <- fixest::demean(as.matrix(dd[, c(yvar, xvars)]), dd[, fe, drop = FALSE])
  y  <- dm[, 1]
  X  <- dm[, -1, drop = FALSE]
  X  <- X[, c(param, setdiff(colnames(X), param)), drop = FALSE]   # tested variable first
  qx <- qr(X); X <- X[, qx$pivot[seq_len(qx$rank)], drop = FALSE]   # drop collinear columns
  if (!param %in% colnames(X)) return(tibble(param = param, estimate = NA_real_,
                                             t = NA_real_, p_boot = NA_real_, clusters = NA_integer_))
  k  <- match(param, colnames(X))
  cl <- as.integer(factor(dd[[cluster]])); G <- max(cl)
  XtXi <- solve(crossprod(X))
  tstat <- function(yy) {
    b <- XtXi %*% crossprod(X, yy)
    u <- as.vector(yy - X %*% b)
    S <- rowsum(X * u, cl)                        # score summed within each country
    V <- XtXi %*% crossprod(S) %*% XtXi           # cluster-robust variance
    c(b[k], b[k] / sqrt(V[k, k]))
  }
  obs <- tstat(y)
  Xr  <- X[, -k, drop = FALSE]                    # null model: tested coefficient = 0
  fr  <- if (ncol(Xr) > 0) as.vector(Xr %*% solve(crossprod(Xr), crossprod(Xr, y))) else rep(0, length(y))
  ur  <- y - fr
  webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
  set.seed(seed)
  tb <- replicate(B, {
    v <- sample(webb, G, replace = TRUE)          # one random weight per country
    tstat(fr + ur * v[cl])[2]
  })
  tibble(param = param, estimate = obs[1], t = obs[2],
         p_boot = mean(abs(tb) >= abs(obs[2])), clusters = G)
}

ctrls <- c("vol12_f", "vol12_na", "disp_f")
dp <- d |> mutate(low_x_local  = inc_low  * local,      # interactions as plain columns
                  lmid_x_local = inc_lmid * local,
                  vol_x_local  = vol12_f  * local,
                  vna_x_local  = vol12_na * local,
                  disp_x_local = disp_f   * local)
pool_x <- c("inc_low", "inc_lmid", "low_x_local", "lmid_x_local",
            "vol_x_local", "vna_x_local", "disp_x_local")
pr7 <- filter(pr, share_actual == 1)

cat("\n4. Wild cluster bootstrap (Webb weights, 9,999 draws; takes about a minute)...\n")
wb <- bind_rows(
  wild_boot(pr,  "y", c("inc_low", "inc_lmid", ctrls), "cell", "inc_low")      |> mutate(model = "Main"),
  wild_boot(pr,  "y", c("inc_low", "inc_lmid", ctrls), "cell", "inc_lmid")     |> mutate(model = "Main"),
  wild_boot(dp,  "y", pool_x,                          "cell", "low_x_local")  |> mutate(model = "Pooled"),
  wild_boot(dp,  "y", pool_x,                          "cell", "lmid_x_local") |> mutate(model = "Pooled"),
  wild_boot(pr7, "y", c("inc_low", "inc_lmid", ctrls), "cell", "inc_low")      |> mutate(model = "R7 actual"),
  wild_boot(pr7, "y", c("inc_low", "inc_lmid", ctrls), "cell", "inc_lmid")     |> mutate(model = "R7 actual")
) |> relocate(model)
print(wb, width = Inf)
cat("Check: 'estimate' must equal the coefficient in table 3 for the same model.\n")
write_csv(wb, here("output", "tables", "wild_bootstrap.csv"))

# ---- 5b. Error by country (to see which countries drive the averages) --------
by_country <- d |> filter(type == "price", !is.na(y)) |>
  group_by(iso3, country, income, region) |>
  summarise(n = n(), mean_y = mean(y), median_y = median(y),
            share_in_band = mean(in_band, na.rm = TRUE),
            share_actual = mean(share_actual == 1), .groups = "drop") |>
  arrange(desc(mean_y))
cat("\n5. Price error by country (largest first):\n"); print(by_country, n = Inf, width = Inf)
write_csv(by_country, here("output", "tables", "error_by_country.csv"))

# ---- 6. Figure --------------------------------------------------------------
fig <- d |> filter(!is.na(y)) |>
  group_by(type, income) |>
  summarise(mean_y = mean(y), se = sd(y) / sqrt(n()), .groups = "drop") |>
  mutate(income = factor(income, c("Low", "Lower-middle", "Upper-middle")),
         type = recode(type, price = "Price questions (need local knowledge)",
                       control = "Arithmetic controls (no local knowledge)"))
p <- ggplot(fig, aes(income, mean_y)) +
  geom_col(width = 0.6, fill = "grey35") +
  geom_errorbar(aes(ymin = mean_y - 1.96 * se, ymax = mean_y + 1.96 * se), width = 0.15) +
  facet_wrap(~type) +
  labs(x = "World Bank income group", y = "Mean absolute log error",
       caption = "Bars: naive 95% intervals. Model: gpt-4.1-mini-2025-04-14.") +
  theme_minimal(base_size = 12)
ggsave(here("output", "figures", "error_by_income.png"), p, width = 9, height = 4.5, dpi = 300)

# ---- 7. The main result in one sentence -------------------------------------
b <- coef(m_main); s <- se(m_main)
cat(sprintf(paste0("\n6. Relative to upper-middle-income countries, the price error differs by",
                   " %+.3f log points in low-income (SE %.3f, bootstrap p = %.2f) and",
                   " %+.3f in lower-middle-income countries (SE %.3f, bootstrap p = %.2f).\n"),
            b["inc_low"], s["inc_low"], wb$p_boot[1],
            b["inc_lmid"], s["inc_lmid"], wb$p_boot[2]))
