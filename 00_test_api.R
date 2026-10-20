# 00_test_api.R
# Purpose: check that the key works, see which models are available,
# and send one test question. Cost: a fraction of a cent.

library(httr2); library(purrr); library(jsonlite); library(here)

# ---- 1. Is the key loaded? (we check its length, we never print it) --------
key <- Sys.getenv("OPENAI_API_KEY")
if (nchar(key) == 0) stop("Key not found. Check .Renviron and restart R.")
cat("Key loaded:", nchar(key), "characters\n")

# ---- 2. Which models can this account use? ---------------------------------
models <- request("https://api.openai.com/v1/models") |>
  req_auth_bearer_token(key) |>
  req_error(body = function(r) resp_body_json(r)$error$message) |>
  req_perform() |>
  resp_body_json()
model_ids <- sort(map_chr(models$data, "id"))
writeLines(model_ids, here("logs", "available_models.txt"))
cat("Models available:", length(model_ids), "(full list in logs/available_models.txt)\n")

# Prefer standard (non-"reasoning") models: they accept temperature = 0,
# which makes answers as repeatable as possible.
preferred <- c("gpt-4.1-mini", "gpt-4o-mini", "gpt-4.1", "gpt-4o")
MODEL <- preferred[preferred %in% model_ids][1]
if (is.na(MODEL)) stop("None of the preferred models is available. Send Claude logs/available_models.txt.")
cat("Model chosen:", MODEL, "\n")

# ---- 3. One test question --------------------------------------------------
body <- list(
  model = MODEL,
  messages = list(list(role = "user",
                       content = "What is 17 multiplied by 23? Reply with the number only.")),
  temperature = 0,
  max_completion_tokens = 20
)
resp <- request("https://api.openai.com/v1/chat/completions") |>
  req_auth_bearer_token(key) |>
  req_body_json(body) |>
  req_error(body = function(r) resp_body_json(r)$error$message) |>
  req_perform() |>
  resp_body_json()

write_json(resp, here("logs", "test_call.json"), auto_unbox = TRUE, pretty = TRUE)
cat("\nModel version that answered:", resp$model, "\n")
cat("Answer:", resp$choices[[1]]$message$content, "\n")
cat("Tokens used:", resp$usage$total_tokens, "\n")
cat("Test complete.\n")
file.edit(here::here(".Renviron"))
