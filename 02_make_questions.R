# 02_make_questions.R
# Purpose: turn the benchmark into the list of questions for the model.
#   (a) price questions: country x product x month, for 8 survey months
#   (b) control questions: identical arithmetic in every country,
#       so no local knowledge is needed (the placebo)
# Input:  data_clean/benchmark.csv
# Output: data_clean/questions.csv

library(readr); library(dplyr); library(tidyr); library(stringr)
library(lubridate); library(purrr); library(here)

bm <- read_csv(here("data_clean", "benchmark.csv"), show_col_types = FALSE)

# ---- 1. Eight survey months, the same for every country --------------------
survey_months <- as.Date(c("2023-03-01", "2023-06-01", "2023-09-01", "2023-12-01",
                           "2024-03-01", "2024-06-01", "2024-09-01", "2024-12-01"))
price_q <- bm |> filter(month %in% survey_months)

# ---- 2. Keep only product x month cells observed in at least 2 countries ---
price_q <- price_q |>
  group_by(group, month) |>
  filter(n_distinct(iso3) >= 2) |>
  ungroup()

overlap <- price_q |>
  group_by(group, month) |>
  summarise(countries = n_distinct(iso3), income_groups = n_distinct(income),
            .groups = "drop")
cat("Product x month cells:", nrow(overlap),
    "| cells containing more than one income group:",
    sum(overlap$income_groups >= 2), "\n")

# ---- 3. Write the price questions ------------------------------------------
unit_word <- c(KG = "kilogram", L = "litre")
price_q <- price_q |>
  mutate(type        = "price",
         month_txt   = paste(month.name[lubridate::month(month)], year(month)),
         prompt      = sprintf(paste(
           "What was the average retail price of %s in %s in %s,",
           "in local currency (%s) per %s?",
           "Give your best estimate as a single number, with no text."),
           str_to_lower(commodity), country, month_txt, currency,
           unit_word[unit_base]),
         answer_true = p_star)

# ---- 4. Control questions: the same arithmetic in every country ------------
# Profit = qty x (sell - buy) - transport.
templates <- tibble(
  template  = 1:8,
  qty       = c(120L, 250L, 75L, 340L, 180L, 95L, 410L, 60L),
  buy       = c(45L, 32L, 88L, 27L, 64L, 51L, 19L, 120L),
  sell      = c(58L, 41L, 103L, 35L, 79L, 66L, 26L, 142L),
  transport = c(900L, 1200L, 450L, 1500L, 800L, 600L, 1100L, 300L)
) |>
  mutate(answer_true = qty * (sell - buy) - transport)

country_info <- bm |>
  count(iso3, country, currency, income) |>
  group_by(iso3) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-n)

control_q <- country_info |>
  cross_join(templates) |>
  mutate(type   = "control",
         prompt = sprintf(paste(
           "A trader in %s buys %d kilograms of rice at %d %s per kilogram,",
           "pays %d %s for transport, and sells all the rice at %d %s per kilogram.",
           "What is the trader's profit in %s?",
           "Give the answer as a single number, with no text."),
           country, qty, buy, currency, transport, currency, sell, currency,
           currency))

# ---- 5. Put everything together and save -----------------------------------
questions <- bind_rows(
  price_q |> select(type, iso3, country, income, group, commodity, unit_base,
                    currency, month, prompt, answer_true, p10, p90, n_markets,
                    vol12, dispersion, share_actual),
  control_q |> select(type, iso3, country, income, template, currency,
                      prompt, answer_true)
) |>
  mutate(qid = sprintf("Q%04d", row_number()), .before = 1)

cat("\nQuestions written:\n"); print(count(questions, type))
cat("\nPrice questions by income group:\n")
print(count(filter(questions, type == "price"), income))
cat("\nPrice questions by country:\n")
print(count(filter(questions, type == "price"), iso3, country), n = Inf)

write_csv(questions, here("data_clean", "questions.csv"))
cat("\nSaved: data_clean/questions.csv\n")
