# 03_query_models.R
# Purpose: send every question to the model twice and log everything.
# Input:  data_clean/questions.csv
# Output: data_clean/responses.csv     (one row per call)
#         logs/raw_responses.jsonl     (full raw API response per call)
# Resumable: if it stops, Source it again — completed calls are skipped.

library(readr); library(dplyr); library(purrr); library(httr2)
library(jsonlite); library(here)

MODEL   <- "gpt-4.1-mini"   # pinned; the dated version is recorded per call
REPEATS <- 2                # each question asked twice
BATCH   <- 10               # requests sent at the same time

key <- Sys.getenv("OPENAI_API_KEY")
if (nchar(key) == 0) stop("Key not found.")

questions <- read_csv(here("data_clean", "questions.csv"), show_col_types = FALSE)

# One row per call, in random order: the timing of a call is unrelated to its country
set.seed(20261020)
calls <- tidyr::expand_grid(qid = questions$qid, rep = seq_len(REPEATS)) |>
  slice_sample(prop = 1) |>
  left_join(select(questions, qid, prompt), by = "qid")

out_csv <- here("data_clean", "responses.csv")
out_raw <- here("logs", "raw_responses.jsonl")
done <- if (file.exists(out_csv)) {
  read_csv(out_csv, show_col_types = FALSE) |> filter(status == "ok") |> select(qid, rep)
} else tibble(qid = character(), rep = integer())
todo <- anti_join(calls, done, by = c("qid", "rep"))
cat("Calls in total:", nrow(calls), "| already done:", nrow(done),
    "| to do:", nrow(todo), "\n")

make_req <- function(prompt) {
  request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(key) |>
    req_body_json(list(
      model = MODEL,
      messages = list(list(role = "user", content = prompt)),
      temperature = 0,
      max_completion_tokens = 20)) |>
    req_retry(max_tries = 5) |>
    req_timeout(60)
}

batches <- split(todo, ceiling(seq_len(nrow(todo)) / BATCH))
for (b in seq_along(batches)) {
  bt    <- batches[[b]]
  resps <- req_perform_parallel(map(bt$prompt, make_req),
                                on_error = "continue", progress = FALSE)
  rows <- map2_dfr(resps, seq_len(nrow(bt)), function(r, k) {
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    if (inherits(r, "httr2_response")) {
      j <- resp_body_json(r)
      write(toJSON(list(qid = bt$qid[k], rep = bt$rep[k], time_utc = stamp,
                        response = j), auto_unbox = TRUE),
            out_raw, append = TRUE)
      tibble(qid = bt$qid[k], rep = bt$rep[k], time_utc = stamp, status = "ok",
             model_version     = j$model,
             response_text     = j$choices[[1]]$message$content %||% NA_character_,
             finish_reason     = j$choices[[1]]$finish_reason %||% NA_character_,
             prompt_tokens     = j$usage$prompt_tokens,
             completion_tokens = j$usage$completion_tokens,
             error = NA_character_)
    } else {
      tibble(qid = bt$qid[k], rep = bt$rep[k], time_utc = stamp, status = "error",
             model_version = NA_character_, response_text = NA_character_,
             finish_reason = NA_character_, prompt_tokens = NA_integer_,
             completion_tokens = NA_integer_, error = conditionMessage(r))
    }
  })
  write_csv(rows, out_csv, append = file.exists(out_csv))
  if (b %% 20 == 0 || b == length(batches))
    cat(sprintf("Batch %d / %d done (%s)\n", b, length(batches),
                format(Sys.time(), "%H:%M:%S")))
  Sys.sleep(1)
}

res <- read_csv(out_csv, show_col_types = FALSE)
cat("\nRows logged:", nrow(res), "| ok:", sum(res$status == "ok"),
    "| errors:", sum(res$status == "error"), "\n")
print(count(filter(res, status == "ok"), model_version))
cat("Total tokens:", sum(res$prompt_tokens + res$completion_tokens, na.rm = TRUE), "\n")
