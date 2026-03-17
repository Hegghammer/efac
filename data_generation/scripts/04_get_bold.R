# ----------------------------------------
# Detect bold speaker markers in pre-1976 PDFs
#
# Uses a vision LLM to extract bold words per page
# and stores JSON outputs in data/json/bold.
# ----------------------------------------

library(ellmer)
library(tidyverse)
library(fs)
library(jsonlite)

# Load pre-1976 PDFs ----------------------------------------
pdfs <- dir_ls("data/pdf")
pdfs <- pdfs[!grepl("data/pdf/19", pdfs)]

type_page <- type_object(
  "Bold words found on a single page. Remove the colon at the end.",
  page_number = type_number("The page number"),
  bold_words = type_array(type_string(), "All words in bold on this page")
)

type_bold <- type_object(
  "Bold words found across all pages of the document",
  pages = type_array(type_page, "One entry per page in the document")
)

# Run bold-word extraction ----------------------------------------
output_dir <- "data/json/bold"
dir_create(output_dir)

for (i in seq_along(pdfs)) {
  pdf_path <- pdfs[i]
  json_path <- file.path(output_dir, str_replace(basename(pdf_path), "\\.pdf$", ".json"))

  # Skip if already processed
  if (file_exists(json_path)) {
    message("Skipping ", i, "/", length(pdfs), ": ", basename(pdf_path))
    next
  }

  message("Processing ", i, "/", length(pdfs), ": ", basename(pdf_path))

  chat <- chat_anthropic(
    model = "claude-opus-4-5-20251101",
    system_prompt = "You are an expert at telling bold from regular font in images of text. Your job is to identify the words or expressions in bold. Ignore text in regular font or in italics.",
    params = list(temperature = 0)
  )

  resp <- chat$chat_structured(
    "Find all words in bold in this Norwegian text. Examine EVERY page and return a separate entry for each page.",
    type = type_bold,
    content_pdf_file(pdf_path)
  )

  write(toJSON(resp, pretty = TRUE), json_path)
  message("  ✓ Done")
}
