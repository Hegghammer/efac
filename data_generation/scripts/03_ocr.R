# ----------------------------------------
# OCR PDFs and write plain-text transcripts
#
# Sends PDFs to the Mistral OCR API, stores JSON
# responses in data/json/ocr, and writes .txt files
# to data/txt for downstream parsing.
# ----------------------------------------

library(tidyverse)
library(fs)
library(httr2)
library(base64enc)
library(jsonlite)

pdfs <- dir_ls("data/pdf")


mistral_ocr_pdf <- function(file_path, max_retries = 5, base_delay = 2) {
  file_bytes <- readBin(file_path, "raw", file.info(file_path)$size)
  base64_data <- base64encode(file_bytes)

  for (attempt in 1:max_retries) {
    tryCatch(
      {
        resp <- request("https://api.mistral.ai/v1/ocr") |>
          req_headers(
            Authorization = paste("Bearer", Sys.getenv("MISTRAL_API_KEY")),
            `Content-Type` = "application/json"
          ) |>
          req_body_json(list(
            model = "mistral-ocr-2505",
            document = list(
              type = "document_url",
              document_url = paste0("data:application/pdf;base64,", base64_data)
            )
          )) |>
          req_retry(
            max_tries = 3,
            is_transient = \(resp) {
              resp_status(resp) %in% c(429, 500, 502, 503, 504)
            },
            backoff = ~ 2^.x # exponential backoff
          ) |>
          req_perform() |>
          resp_body_json()

        return(resp)
      },
      error = function(e) {
        if (attempt == max_retries) {
          stop("Failed after ", max_retries, " attempts: ", conditionMessage(e))
        }

        delay <- base_delay * (2^(attempt - 1)) + runif(1, 0, 1) # exponential backoff + jitter
        message("  Attempt ", attempt, " failed: ", conditionMessage(e))
        message("  Retrying in ", round(delay, 1), " seconds...")
        Sys.sleep(delay)
      }
    )
  }
}

# OCR PDFs ----------------------------------------
process_pdfs <- function(pdfs, output_dir = "data/json/ocr") {
  dir_create(output_dir)

  for (i in seq_along(pdfs)) {
    pdf_path <- pdfs[i]
    json_path <- file.path(
      output_dir,
      str_replace(basename(pdf_path), "\\.pdf$", ".json")
    )

    # Skip if already processed
    if (file_exists(json_path)) {
      message(
        "Skipping ",
        i,
        " of ",
        length(pdfs),
        ": ",
        basename(pdf_path),
        " (already exists)"
      )
      next
    }

    message(
      "Processing pdf ",
      i,
      " of ",
      length(pdfs),
      ": ",
      basename(pdf_path),
      " .."
    )

    result <- tryCatch(
      {
        resp <- mistral_ocr_pdf(pdf_path)
        json <- toJSON(resp, pretty = TRUE)
        write(json, json_path)
        message("  ✓ Saved to ", basename(json_path))
        "success"
      },
      error = function(e) {
        message("  ✗ Failed: ", conditionMessage(e))
        "failed"
      }
    )

    # Small delay between requests to be nice to the API
    Sys.sleep(0.5)
  }
}

process_pdfs(pdfs)

# Convert OCR JSON to TXT ----------------------------------------
ocr_jsons <- dir_ls("data/json/ocr")
# ocr_jsons_post_75 <- ocr_jsons[grepl("data/json/ocr/19", ocr_jsons)]


for (i in seq_along(ocr_jsons)) {
  message("Processing json file ", i, " of ", length(ocr_jsons), " ...")
  json <- ocr_jsons[i]
  content <- fromJSON(json)
  text <- paste0(content$pages$markdown, collapse = "\n\n")
  path <- file.path("data/txt", str_replace(basename(json), "json", "txt"))
  write(text, path)
}
