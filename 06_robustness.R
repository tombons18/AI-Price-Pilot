# 06_robustness.R
# Purpose: check that the "stale knowledge" result of 05 is solid.
#   Main result (05): signed error e = ln(AI price / market price) falls when the
#   product's local price rose in the previous year (slope about -0.65).
# Checks:
#   A  Baseline, same as 05 (product x month fixed effects).
#   B  + country fixed effects: compares months WITHIN the same country, so the slope
#      cannot come from permanent country differences (currency, language, region).
#   C  Without Nigeria and Malawi, the two countries with the biggest shocks.
#   D  Only cells built from "actual" WFP prices (no aggregated series).
#   E  + the controls used in 04 (volatility, dispersion across markets).
#   F  Leave-one-out: drop each country in turn and re-estimate (21 regressions).
# Input:  data_clean/analysis_shocks.csv (written by 05_shocks.R)
# Output: output/tables/robustness*.csv, output/tables/robustness.tex

library(readr); library(dplyr); library(fixest); library(here)

f_in <- here("data_clean", "analysis_shocks.csv")
if (!file.exists(f_in)) source(here("05_shocks.R"))   # creates the file if it is missing
pr <- read_csv(f_in, show_col_types = FALSE) |>
  filter(type == "price", !is.na(e), !is.na(drift))
cat("Price answers used:", nrow(pr), "| countries:", n_distinct(pr$iso3), "\n")

# ---- 1. Regressions (SE clustered by country) -------------------------------
A <- feols(e ~ drift | cell,                                     pr, cluster = ~iso3)
B <- feols(e ~ drift | cell + iso3,                              pr, cluster = ~iso3)
C <- feols(e ~ drift | cell, filter(pr, !iso3 %in% c("NGA", "MWI")),  cluster = ~iso3)
D <- feols(e ~ drift | cell, filter(pr, share_actual == 1),      cluster = ~iso3)
E <- feols(e ~ drift + vol12_f + vol12_na + disp_f | cell,       pr, cluster = ~iso3)
mods  <- list(A, B, C, D, E)
heads <- c("A Baseline", "B Country FE", "C No NGA/MWI", "D Actual only", "E Controls")
cat("\n1. Signed error on recent price change:\n")
print(etable(mods, headers = heads, keep = "drift"))
etable(mods, headers = heads, keep = "drift",
       file = here("output", "tables", "robustness.tex"), replace = TRUE)

# ---- 2. Wild cluster bootstrap (same method as 04 and 05) -------------------
# 'fe' can now be one or two fixed effects (e.g. c("cell", "iso3")).
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
cat("\n2. Wild cluster bootstrap p-values (about a minute)...\n")
wb <- bind_rows(
  wild_boot(pr, "e", "drift", "cell",            "drift") |> mutate(check = heads[1]),
  wild_boot(pr, "e", "drift", c("cell", "iso3"), "drift") |> mutate(check = heads[2]),
  wild_boot(filter(pr, !iso3 %in% c("NGA", "MWI")), "e", "drift", "cell", "drift") |> mutate(check = heads[3]),
  wild_boot(filter(pr, share_actual == 1),          "e", "drift", "cell", "drift") |> mutate(check = heads[4]),
  wild_boot(pr, "e", c("drift", cv), "cell",     "drift") |> mutate(check = heads[5])
) |> relocate(check)
print(wb, width = Inf)
cat("Check: 'estimate' must equal the coefficient in table 1 for the same column.\n")
write_csv(wb, here("output", "tables", "robustness_bootstrap.csv"))

# ---- 3. Leave-one-country-out -----------------------------------------------
loo <- bind_rows(lapply(sort(unique(pr$iso3)), function(c0) {
  m <- feols(e ~ drift | cell, filter(pr, iso3 != c0), cluster = ~iso3)
  tibble(dropped = c0, slope = coef(m)[["drift"]], se = se(m)[["drift"]],
         t = slope / se)
}))
cat("\n3. Leave-one-country-out (each row drops one country):\n")
print(loo |> arrange(slope) |> mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
cat(sprintf("Slope range: %.3f to %.3f | smallest |t|: %.2f (when %s is dropped)\n",
            min(loo$slope), max(loo$slope), min(abs(loo$t)), loo$dropped[which.min(abs(loo$t))]))
write_csv(loo, here("output", "tables", "robustness_leave_one_out.csv"))
