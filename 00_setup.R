# 00_setup.R
# Purpose: prepare the project. Installs packages and creates folders.
# Run once. Safe to run again.

# ---- 1. Packages -----------------------------------------------------------
pkgs <- c(
  "readr",     # read and write CSV files
  "dplyr",     # data manipulation
  "tidyr",     # reshaping tables
  "stringr",   # text handling (to extract prices from the model's answers)
  "purrr",     # loops over lists
  "lubridate", # dates
  "httr2",     # send requests to the API
  "jsonlite",  # the format the API speaks
  "fixest",    # regressions with fixed effects and clustered errors
  "ggplot2",   # figures
  "here"       # file paths relative to the project
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) install.packages(missing)

cat("\nPackage check:\n")
for (p in pkgs) {
  ok <- suppressPackageStartupMessages(require(p, character.only = TRUE))
  cat(sprintf("  %-10s %s\n", p, if (ok) "OK" else "FAILED"))
}

# ---- 2. Folders ------------------------------------------------------------
dirs <- c("data_raw", "data_clean", "logs",
          "output", "output/tables", "output/figures")
for (d in dirs) dir.create(here::here(d), showWarnings = FALSE, recursive = TRUE)

cat("\nFolders in the project:\n")
print(list.dirs(here::here(), recursive = FALSE, full.names = FALSE))

# ---- 3. Files that must never go online -------------------------------------
# .Renviron will hold the API key. It must never be shared.
writeLines(c(".Renviron", ".Rhistory", ".RData", ".Rproj.user/", "data_raw/"),
           here::here(".gitignore"))

# ---- 4. Record the software versions ----------------------------------------
writeLines(capture.output(sessionInfo()),
           here::here("logs", "session_info.txt"))

cat("\nR version:", R.version.string, "\n")
cat("Setup complete.\n")
install.packages("readxl")