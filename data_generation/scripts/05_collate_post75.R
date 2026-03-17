# ----------------------------------------
# Segment speaker turns in post-1975 transcripts
#
# Parses OCR text files where speaker markers are
# mostly uppercase labels followed by colons.
# Writes parsed remarks to out/remarks.csv.
# ----------------------------------------

library(tidyverse)
library(fs)

txts <- dir_ls("data/txt")
txts <- txts[grepl("data/txt/19", txts)]
txts <- "data/txt/1992_1201_-sladda_.txt"

parse_transcript <- function(filepath) {
  # Exceptions dictionaries ----------------------------------------

  # Exception speaker names (rare OCR errors, etc.)
  # These are matched exactly as "\nEXCEPTION:"
  exceptions <- c(
    "\u0446\u0456\u0442ENRIKSMINISTER KNUT FRYDENLUND", # цітENRIKSMINISTER (cyrillic OCR error)
    "K\u00e5re Willoch",
    "Trygve Bratteli",
    "Fung. formann",
    "JOHAN J: JAKOBSEN"
  )

  # Explicit blocklist - these words are never speakers
  blocklist <- c(
    "Ad",
    "LORAN",
    "OMEGA",
    "EU",
    "NATO",
    "FN",
    "EFTA",
    "VEDLEGG",
    "DAGSORDEN"
  )

  # Blocklist patterns for terms with special characters
  # Note: Ø may be counted as 1 or 2 chars depending on R's locale
  blocklist_patterns <- c(
    "^E.{1,2}S$", # EØS
    "^E.{1,2}F$" # EØF
  )

  # OCR text fixes applied before parsing
  # For cases where OCR produces wrong punctuation
  ocr_text_fixes <- c(
    "FUNG:" = "FUNG." # OCR mistook colon for period in "FUNG. FORMANN"
  )
  # End exceptions dictionaries ----------------------------------------

  # Read the file
  text <- readLines(filepath, encoding = "UTF-8", warn = FALSE)
  # Join all lines into one string
  full_text <- paste(text, collapse = "\n")

  # Apply OCR text fixes
  for (i in seq_along(ocr_text_fixes)) {
    full_text <- gsub(names(ocr_text_fixes)[i], ocr_text_fixes[i], full_text, fixed = TRUE)
  }

  # First, find and mark all exception matches
  exception_markers <- list()
  for (exc in exceptions) {
    exc_pattern <- paste0("(^|\\n)(", gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", exc, perl = TRUE), "):\\s*")
    exc_matches <- gregexpr(exc_pattern, full_text, perl = TRUE)[[1]]
    if (exc_matches[1] != -1) {
      exc_cap_starts <- attr(exc_matches, "capture.start")[, 2]
      exc_cap_lengths <- attr(exc_matches, "capture.length")[, 2]
      exc_match_lengths <- attr(exc_matches, "match.length")
      for (j in seq_along(exc_matches)) {
        exception_markers[[length(exception_markers) + 1]] <- list(
          match_start = exc_matches[j],
          match_length = exc_match_lengths[j],
          cap_start = exc_cap_starts[j],
          cap_length = exc_cap_lengths[j],
          speaker = exc
        )
      }
    }
  }

  # Pattern to match speaker names: UPPERCASE WORDS followed by colon
  # Must appear at start of text or after a newline
  #
  # Character codes:
  #   Norwegian uppercase: Æ=\u00C6, Ø=\u00D8, Å=\u00C5
  #   Norwegian lowercase: æ=\u00E6, ø=\u00F8, å=\u00E5
  #   Umlauts upper: Ü=\u00DC, Ö=\u00D6, Õ=\u00D5
  #   Umlauts lower: ü=\u00FC, ö=\u00F6, õ=\u00F5

  # First char: uppercase A-Z plus Norwegian and umlauts
  first_char <- "A-Z\u00C6\u00D8\u00C5\u00DC\u00D6\u00D5"

  # Rest of name: same plus lowercase (including a-z for OCR errors, filtered later)
  # Use [ \t] instead of \s to avoid matching across newlines
  rest_chars <- "A-Za-z\u00C6\u00D8\u00C5\u00DC\u00D6\u00D5\u00F8\u00E5\u00FC\u00F6\u00F5 \t.,-"

  # Margin signs that may appear before speaker names (OCR artifacts)
  # Can appear with or without parentheses, and with linebreaks before the name
  margin_signs <- c("8W", "KL", "IS", "ME", "AGO", "SL", "BW", "SA", "LR", "IL", "EL", "EK", "KML", "GSR", "LM", "AGK", "RML")
  margin_pattern_prefix <- paste0("(?:\\(?(?:", paste(margin_signs, collapse = "|"), ")\\)?\\s*)")

  speaker_pattern <- paste0(
    "(^|\\n)",
    "#*\\s*", # Zero or more hashes
    "(", margin_pattern_prefix, "?", # Optional margin sign (with or without parens) - in capture
    "(?:HEMMELI[GC]\\s*)?", # Optional HEMMELIG/HEMMELIC prefix
    "\\(?", # Optional opening paren for (FORMANNEN
    "[", first_char, "]", # First char must be uppercase
    "[", rest_chars, "]+)", # Rest of name
    ":\\s*"
  )

  # Helper: count regular lowercase a-z (not Norwegian/umlaut lowercase)
  count_lowercase <- function(s) {
    nchar(gsub("[^a-z]", "", s))
  }

  # Find all speaker matches
  matches <- gregexpr(speaker_pattern, full_text, perl = TRUE)[[1]]

  if (matches[1] == -1 && length(exception_markers) == 0) {
    # No speakers found
    return(data.frame(
      speaker_verbatim = character(),
      remark = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Extract match positions and lengths
  if (matches[1] != -1) {
    match_starts <- as.integer(matches)
    match_lengths <- attr(matches, "match.length")
    capture_starts <- attr(matches, "capture.start")[, 2]
    capture_lengths <- attr(matches, "capture.length")[, 2]
  } else {
    match_starts <- integer()
    match_lengths <- integer()
    capture_starts <- integer()
    capture_lengths <- integer()
  }

  # Add exception markers, REPLACING any main pattern match at the same position
  # (exceptions should take priority because they handle edge cases)
  for (em in exception_markers) {
    # Check if this position is already in matches - if so, replace it
    pos_idx <- which(match_starts == em$match_start)
    if (length(pos_idx) > 0) {
      # Replace the existing match with the exception
      match_lengths[pos_idx] <- em$match_length
      capture_starts[pos_idx] <- em$cap_start
      capture_lengths[pos_idx] <- em$cap_length
    } else {
      # Add new match
      match_starts <- c(match_starts, em$match_start)
      match_lengths <- c(match_lengths, em$match_length)
      capture_starts <- c(capture_starts, em$cap_start)
      capture_lengths <- c(capture_lengths, em$cap_length)
    }
  }

  # Sort all by position
  ord <- order(match_starts)
  match_starts <- match_starts[ord]
  match_lengths <- match_lengths[ord]
  capture_starts <- capture_starts[ord]
  capture_lengths <- capture_lengths[ord]

  speakers <- character()
  remarks <- character()
  valid_indices <- integer()

  for (i in seq_along(match_starts)) {
    # Extract speaker name from capture group
    speaker <- substr(full_text, capture_starts[i], capture_starts[i] + capture_lengths[i] - 1)
    speaker <- trimws(speaker)

    # Clean up OCR artifacts:
    # 1. Remove HEMMELIG/HEMMELIC prefix (may have newlines/whitespace)
    speaker <- gsub("^HEMMELI[GC]\\s*", "", speaker)

    # 2. Remove margin signs at start of name (with or without parens, may have newlines)
    margin_pattern <- paste0("^\\(?(?:", paste(margin_signs, collapse = "|"), ")\\)?\\s*")
    speaker <- gsub(margin_pattern, "", speaker)

    speaker <- trimws(speaker)

    # Skip if more than 1 regular lowercase letter (likely false positive)
    # Exception: speakers in the exceptions list bypass this filter
    is_exception <- speaker %in% exceptions
    if (!is_exception && count_lowercase(speaker) > 1) {
      next
    }

    # Skip if in blocklist (case-insensitive) or matches blocklist pattern
    if (toupper(speaker) %in% toupper(blocklist)) {
      next
    }
    if (any(sapply(blocklist_patterns, function(p) grepl(p, speaker, perl = TRUE)))) {
      next
    }

    valid_indices <- c(valid_indices, i)
    speakers <- c(speakers, speaker)
  }

  # Extract remarks using valid indices only
  for (j in seq_along(valid_indices)) {
    i <- valid_indices[j]

    # Remark starts after the full match
    remark_start <- match_starts[i] + match_lengths[i]

    # Remark ends at the next valid match (or end of text)
    if (j < length(valid_indices)) {
      next_i <- valid_indices[j + 1]
      remark_end <- match_starts[next_i] - 1
    } else {
      remark_end <- nchar(full_text)
    }

    remark <- substr(full_text, remark_start, remark_end)
    remarks <- c(remarks, remark)
  }

  # Collapse all whitespace in remarks
  remarks <- gsub("\\s+", " ", remarks)
  remarks <- trimws(remarks)

  # Return dataframe
  data.frame(
    speaker_verbatim = speakers,
    remark = remarks,
    stringsAsFactors = FALSE
  )
}

df <- data.frame(
  filename = character(),
  index = integer(), 
  speaker_verbatim = character(), 
  remark = character()
)

for (i in seq_along(txts)) {
  message("Processing txt file ", i, " of ", length(txts), " ..")
  df_txt <- parse_transcript(txts[i])
  if (nrow(df_txt) > 0) {
    df_txt$filename <- str_replace(basename(txts[i]), "txt", "pdf")
    df_txt$index <- as.integer(rownames(df_txt))
    df_txt <- df_txt |>
      select(filename, index, speaker_verbatim, remark)
    df <- rbind(df, df_txt)
  }
}

write_csv(df, "temp/csv/missing_19921201.csv")
