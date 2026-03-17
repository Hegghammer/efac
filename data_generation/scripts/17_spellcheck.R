# ----------------------------------------
# Detect likely OCR spelling errors in remarks
#
# Uses a Norvig-style edit-distance approach to
# propose typo-correction candidates from rare words.
# Writes a raw candidate list for manual review.
# ----------------------------------------

# Based on: Norvig (2007) https://norvig.com/spell-correct.html

library(tidyverse)
library(hunspell)
library(cli)

# Configuration ----------------------------------------
data_dir <- Sys.getenv("EFAC_CORE_CSV_DIR", "../article/csv")
remarks_path <- file.path(data_dir, "remarks.csv")
# For testing against the pre-spellcheck version:
# remarks_path <- "/tmp/remarks_pre_spellcheck.csv"
output_path <- "scripts/spelling_candidates_raw.csv"
min_word_length <- 5
max_word_length <- 20
max_typo_freq <- 2       # only consider words appearing this many times or fewer
min_freq_ratio <- 10      # correction must be >= this many times more frequent
old_spelling_pattern <- "aa" # filter out pre-1938 Norwegian spelling

# Load data ----------------------------------------
cli_h1("Loading remarks")
remarks <- read_csv(remarks_path, show_col_types = FALSE)
cli_alert_info("{nrow(remarks)} remarks loaded")

# Step 1: Extract word frequencies ----------------------------------------
cli_h1("Extracting word frequencies")

# Extract all lowercase words (4+ chars) from the remark column
all_words <- remarks |>
  pull(remark) |>
  na.omit() |>
  str_to_lower() |>
  str_extract_all("\\b[a-zæøå]{4,}\\b") |>
  unlist()

word_freq <- tibble(word = all_words) |>
  count(word, name = "freq") |>
  arrange(desc(freq))

cli_alert_info("{nrow(word_freq)} unique lowercase words (4+ chars)")

# Step 2: Hunspell filter ----------------------------------------
# Check against bokmål and (if available) nynorsk dictionaries.
# A word is "misspelled" only if it fails all available dictionaries.
dicts <- "nb_NO"
nn_available <- file.exists("/usr/share/hunspell/nn_NO.dic")
if (nn_available) dicts <- c(dicts, "nn_NO")
cli_h1("Running hunspell spellcheck ({paste(dicts, collapse = ' + ')})")

nb_bad <- hunspell(word_freq$word, dict = "nb_NO")
is_misspelled <- map_lgl(nb_bad, ~ length(.x) > 0)

if (nn_available) {
  nn_bad <- hunspell(word_freq$word, dict = "nn_NO")
  is_misspelled_nn <- map_lgl(nn_bad, ~ length(.x) > 0)
  is_misspelled <- is_misspelled & is_misspelled_nn
}

misspelled_words <- word_freq$word[is_misspelled]
cli_alert_info("{length(misspelled_words)} words flagged by hunspell")

# Step 3: Filter to rare misspelled words ----------------------------------------
cli_h1("Filtering candidates")

rare_misspelled <- word_freq |>
  filter(
    word %in% misspelled_words,
    freq <= max_typo_freq,
    nchar(word) >= min_word_length,
    nchar(word) <= max_word_length,
    !str_detect(word, fixed(old_spelling_pattern))
  )

cli_alert_info("{nrow(rare_misspelled)} rare misspelled words after filtering")

# Step 4: Generate edit-distance-1 candidates ----------------------------------------
cli_h1("Computing edit-distance-1 neighbours (Norvig method)")

# Build lookup of frequent words for fast matching
frequent_words <- word_freq |>
  filter(freq >= 10) |>
  deframe()  # named vector: word -> freq

# Norvig's edits1: all strings that are 1 edit away from the input
letters_no <- c(letters, "æ", "ø", "å")

edits1 <- function(word) {
  chars <- str_split(word, "")[[1]]
  n <- length(chars)

  # Deletions
  deletes <- map_chr(seq_len(n), ~ paste0(chars[-.x], collapse = ""))

  # Transpositions
  transposes <- character(0)
  if (n > 1) {
    transposes <- map_chr(seq_len(n - 1), function(i) {
      swapped <- chars
      swapped[c(i, i + 1)] <- swapped[c(i + 1, i)]
      paste0(swapped, collapse = "")
    })
  }

  # Replacements
  replaces <- unlist(map(seq_len(n), function(i) {
    map_chr(letters_no, function(c) {
      replaced <- chars
      replaced[i] <- c
      paste0(replaced, collapse = "")
    })
  }))

  # Insertions
  inserts <- unlist(map(seq(0, n), function(i) {
    map_chr(letters_no, function(c) {
      paste0(
        paste0(chars[seq_len(i)], collapse = ""),
        c,
        paste0(chars[seq(i + 1, n, length.out = max(0, n - i))], collapse = "")
      )
    })
  }))

  unique(c(deletes, transposes, replaces, inserts))
}

# For each rare misspelled word, find the best frequent-word match
cli_alert_info("Processing {nrow(rare_misspelled)} candidates...")
pb <- cli_progress_bar("Finding corrections", total = nrow(rare_misspelled))

results <- list()
for (i in seq_len(nrow(rare_misspelled))) {
  w <- rare_misspelled$word[i]
  w_freq <- rare_misspelled$freq[i]

  neighbours <- edits1(w)
  matches <- intersect(neighbours, names(frequent_words))

  if (length(matches) > 0) {
    # Pick the most frequent match
    match_freqs <- frequent_words[matches]
    best <- names(which.max(match_freqs))
    best_freq <- unname(match_freqs[best])

    # Only keep if the correction is sufficiently more frequent
    if (best_freq >= min_freq_ratio * w_freq) {
      results[[length(results) + 1]] <- tibble(
        typo = w,
        typo_freq = w_freq,
        correction = best,
        correction_freq = as.integer(best_freq),
        ratio = as.integer(best_freq / w_freq)
      )
    }
  }
  cli_progress_update(id = pb)
}
cli_progress_done(id = pb)

# Step 5: Assemble and write output ----------------------------------------
cli_h1("Writing results")

if (length(results) > 0) {
  candidates <- bind_rows(results) |>
    arrange(desc(correction_freq))

  write_csv(candidates, output_path)
  cli_alert_success("{nrow(candidates)} candidates written to {output_path}")
} else {
  cli_alert_warning("No candidates found")
}
