#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)

validation_path <- if (length(args) >= 1) args[[1]] else "csv/validation_human.csv"
remarks_path <- if (length(args) >= 2) args[[2]] else "csv/remarks.csv"
out_path <- if (length(args) >= 3) args[[3]] else "csv/validation_metrics.csv"

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringi::stri_trans_nfkc(x)
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

compute_dist_python <- function(df) {
  py <- Sys.which("python")
  if (!nzchar(py)) {
    stop("Python executable not found in PATH.")
  }

  tmp_in <- tempfile(fileext = ".csv")
  tmp_out <- tempfile(fileext = ".csv")
  tmp_py <- tempfile(fileext = ".py")
  on.exit(unlink(c(tmp_in, tmp_out, tmp_py)), add = TRUE)

  write_csv(df, tmp_in)

  py_code <- c(
    "import sys",
    "import pandas as pd",
    "import jellyfish",
    "inp, out = sys.argv[1], sys.argv[2]",
    "df = pd.read_csv(inp)",
    "df['text_a'] = df['text_a'].fillna('')",
    "df['text_b'] = df['text_b'].fillna('')",
    "df['dist'] = [jellyfish.levenshtein_distance(a, b) for a, b in zip(df['text_a'], df['text_b'])]",
    "df[['id', 'dist']].to_csv(out, index=False)"
  )

  writeLines(py_code, tmp_py)
  status <- system2(py, c(tmp_py, tmp_in, tmp_out), stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    stop("Python distance calculation failed.")
  }

  read_csv(tmp_out, show_col_types = FALSE) |>
    arrange(id)
}

cat("[1/5] Reading validation and remarks data...\n")
val <- read_csv(validation_path, show_col_types = FALSE)
rem <- read_csv(remarks_path, show_col_types = FALSE)

cat("[2/5] Aligning rows within meeting by sequence...\n")
val_aligned <- val |>
  arrange(meeting_id, index) |>
  group_by(meeting_id) |>
  mutate(seq = row_number()) |>
  ungroup()

rem_aligned <- rem |>
  filter(meeting_id %in% unique(val_aligned$meeting_id)) |>
  arrange(meeting_id, index) |>
  group_by(meeting_id) |>
  mutate(seq = row_number()) |>
  ungroup()

paired <- val_aligned |>
  inner_join(
    rem_aligned |>
      select(meeting_id, seq, remark_ocr = remark),
    by = c("meeting_id", "seq")
  ) |>
  transmute(
    meeting_id,
    seq,
    gt = normalize_text(remark),
    ocr = normalize_text(remark_ocr)
  )

char_tbl <- paired |>
  transmute(
    id = row_number(),
    text_a = gt,
    text_b = ocr
  )

cat("[3/5] Computing OCR-level accuracy metrics...\n")
char_dist <- compute_dist_python(char_tbl)$dist

paired <- paired |>
  mutate(
    gt_chars = nchar(gt),
    char_dist = char_dist,
    exact = gt == ocr
  )

n_validation_remarks <- nrow(paired)
validation_chars <- sum(paired$gt_chars)
n_char_edits <- sum(paired$char_dist)
n_exact_match <- sum(paired$exact)

cat("[4/5] Computing adjacent-swap diagnostics...\n")
adj <- paired |>
  group_by(meeting_id) |>
  arrange(seq, .by_group = TRUE) |>
  mutate(
    gt_next = lead(gt),
    ocr_next = lead(ocr),
    pair_id = row_number()
  ) |>
  filter(!is.na(gt_next), !is.na(ocr_next)) |>
  ungroup() |>
  transmute(
    pair_uid = row_number(),
    same_a = gt,
    same_b = ocr,
    same_c = gt_next,
    same_d = ocr_next,
    cross_a = gt,
    cross_b = ocr_next,
    cross_c = gt_next,
    cross_d = ocr,
    len_a = pmax(nchar(gt), 1L),
    len_c = pmax(nchar(gt_next), 1L)
  )

if (nrow(adj) > 0) {
  dist_input <- bind_rows(
    adj |> transmute(id = 4 * pair_uid - 3, text_a = same_a, text_b = same_b),
    adj |> transmute(id = 4 * pair_uid - 2, text_a = same_c, text_b = same_d),
    adj |> transmute(id = 4 * pair_uid - 1, text_a = cross_a, text_b = cross_b),
    adj |> transmute(id = 4 * pair_uid, text_a = cross_c, text_b = cross_d)
  ) |> arrange(id)

  dist_out <- compute_dist_python(dist_input)

  d <- dist_out$dist
  same <- d[seq(1, length(d), by = 4)] / adj$len_a + d[seq(2, length(d), by = 4)] / adj$len_c
  cross <- d[seq(3, length(d), by = 4)] / adj$len_a + d[seq(4, length(d), by = 4)] / adj$len_c
  n_adjacent_pairs <- nrow(adj)
  n_swap_signatures <- sum(cross < same)
} else {
  n_adjacent_pairs <- 0
  n_swap_signatures <- 0
}

aser <- ifelse(n_adjacent_pairs > 0, n_swap_signatures / n_adjacent_pairs, 0)

cat("[5/5] Writing summary metrics...\n")
out <- tibble(
  n_validation_remarks = n_validation_remarks,
  validation_chars = validation_chars,
  n_char_edits = n_char_edits,
  n_exact_match = n_exact_match,
  n_adjacent_pairs = n_adjacent_pairs,
  n_swap_signatures = n_swap_signatures,
  aser = aser
)

write_csv(out, out_path)
cat(sprintf("Saved validation metrics to %s\n", out_path))
