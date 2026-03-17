# ----------------------------------------
# Segment speaker turns in pre-1976 transcripts
#
# Combines OCR JSON with bold-word JSON to identify
# speaker markers, then extracts segmented remarks
# to out/remarks.csv.
# ----------------------------------------

library(tidyverse)
library(fs)
library(jsonlite)

ocr_jsons <- dir_ls("data/json/ocr")
ocr_jsons <- ocr_jsons[!grepl("data/json/ocr/19", ocr_jsons)]
bold_jsons <- dir_ls("data/json/bold")

# Parse Norwegian parliamentary transcripts with bold speaker names
# Uses OCR JSON + bold word detection JSON to identify speakers
#
# Approach:
#   1. Build document-wide dictionary of all bold words (ignoring page numbers)
#   2. Apply LaTeX mappings for known OCR artifacts (e.g., "$\varnothing$" -> "Ø")
#   3. Find all patterns at start of line followed by colon (handles #, ##, [number] from OCR)
#   4. Check if those EXACTLY match bold words in the dictionary
#   5. Exception: words in prefix_exceptions can match as prefix (for "Formannen (Hambro)" etc.)
#   6. Exception: OCR errors in names corrected via ocr_name_corrections (e.g., "Tranmal" -> "Tranmæl")
#   7. Keep matches as speaker attributions
#   8. Hard rule: "\nFormannen:" always treated as speaker
#
# To add exceptions: edit the dictionaries near the top of parse_bold_transcript()
#   - prefix_exceptions: bold words that can match as prefix
#   - latex_mappings: LaTeX OCR artifacts -> actual text
#   - ocr_name_corrections: OCR speaker name errors -> correct names
#
# Returns: list with two dataframes
#   $remarks: columns: filename, index, speaker_verbatim, remark
#   $missing: columns: filename, miss1, miss2, ... (unmatched bold words)

parse_bold_transcript <- function(
  ocr_json_path,
  bold_json_path,
  verbose = TRUE,
  debug = FALSE
) {
  # Exceptions dictionaries ----------------------------------------

  # Bold words that can match as PREFIX of the full speaker name
  # e.g., "Formannen" in bold matches "Formannen (Hambro):"
  prefix_exceptions <- c(
    "Formannen",
    "Henrik Svensen",
    "Konrad Nordahl"
  )

  # LaTeX OCR artifacts -> actual text mappings
  # For cases where OCR outputs LaTeX notation instead of characters
  latex_mappings <- c(
    "$\\varnothing \\mathbf{e n :}$" = "\u00D8en:"
    # Add more here as needed, e.g.:
    # , "$\\AA$" = "Å"
  )

  # OCR name corrections: OCR text -> bold dictionary name
  # For cases where OCR produces wrong characters in speaker names
  # Format: "OCR_version" = "correct_version_in_bold_dict"
  ocr_name_corrections <- c(
    "Tranmal" = "Tranm\u00e6l",
    "oen" = "\u00D8en",
    "Ashj\u00f8rn Haugstvedt" = "Asbj\u00f8rn Haugstvedt",
    "Der Borten" = "Per Borten"
  )
  # End exceptions dictionaries ----------------------------------------

  # Load JSON files
  ocr_data <- fromJSON(ocr_json_path, simplifyVector = FALSE)
  bold_data <- fromJSON(bold_json_path, simplifyVector = FALSE)

  # Build document-wide dictionary of all bold words (ignoring page numbers)
  all_bold_words <- character()
  for (page_info in bold_data$pages) {
    words <- page_info$bold_words
    if (length(words) > 0) {
      all_bold_words <- c(all_bold_words, unlist(words))
    }
  }
  all_bold_words <- unique(all_bold_words)

  # Filter out metadata patterns
  metadata_patterns <- c(
    "^Den utvidede",
    "^m\u00F8te", # møte
    "^M\u00F8te", # Møte
    "^kl\\.",
    "^[Vv]\\s*o\\s*t\\s*e\\s*r\\s*i\\s*n\\s*g", # V o t e r i n g (spaced)
    "^[Dd]\\s*a\\s*g\\s*s\\s*o\\s*r\\s*d\\s*e\\s*n", # D a g s o r d e n (spaced)
    "^Votering",
    "^Dagsorden",
    "^[Ff]ra [A-Z\u00C6\u00D8\u00C5]", # "fra Utenriksdepartementet", "Fra De Norske..." etc.
    "^F\u00F8lgende" # Følgende
  )

  # Filter out metadata patterns (before cleanup)
  is_metadata <- sapply(all_bold_words, function(w) {
    any(sapply(metadata_patterns, function(p) grepl(p, w, ignore.case = FALSE)))
  })
  bold_dict <- all_bold_words[!is_metadata]
  skipped_metadata <- all_bold_words[is_metadata]

  # Clean up bold dictionary - remove any leading hashes or [number] (OCR artifacts)
  bold_dict <- gsub("^#+\\s*", "", bold_dict)
  bold_dict <- gsub("^\\[\\d{1,3}\\]\\s*", "", bold_dict)
  bold_dict <- trimws(bold_dict)
  bold_dict <- unique(bold_dict[bold_dict != ""])

  if (verbose) {
    cat("Bold dictionary size:", length(bold_dict), "words\n")
    cat("Skipped metadata:", length(skipped_metadata), "words\n")
  }

  # Combine all OCR text
  all_text <- ""
  for (page in ocr_data$pages) {
    page_text <- if (length(page$markdown) > 0) page$markdown[[1]] else ""
    all_text <- paste0(all_text, page_text, "\n\n")
  }

  # Apply LaTeX mappings (OCR artifacts)
  for (i in seq_along(latex_mappings)) {
    all_text <- gsub(
      names(latex_mappings)[i],
      latex_mappings[i],
      all_text,
      fixed = TRUE
    )
  }

  # Find all potential speaker patterns: start of line, optional hashes/brackets, text, colon
  # Pattern: newline (or start), optional hashes, optional [number], optional whitespace, captured text, colon, whitespace
  potential_pattern <- "(^|\\n)#*\\s*(?:\\[\\d{1,3}\\]\\s*)?([^\\n:]+):\\s*"
  matches <- gregexpr(potential_pattern, all_text, perl = TRUE)[[1]]

  if (matches[1] == -1) {
    if (verbose) {
      cat("No potential speakers found\n")
    }
    return(list(
      remarks = data.frame(
        filename = character(),
        index = integer(),
        speaker_verbatim = character(),
        remark = character(),
        stringsAsFactors = FALSE
      ),
      missing = data.frame(filename = character(), stringsAsFactors = FALSE)
    ))
  }

  # Extract match info
  match_starts <- as.integer(matches)
  match_lengths <- attr(matches, "match.length")
  cap_starts <- attr(matches, "capture.start")[, 2]
  cap_lengths <- attr(matches, "capture.length")[, 2]

  # Check each potential speaker against bold dictionary
  speaker_markers <- list()
  matched_bold <- character()

  for (i in seq_along(match_starts)) {
    potential_speaker_raw <- substr(
      all_text,
      cap_starts[i],
      cap_starts[i] + cap_lengths[i] - 1
    )
    potential_speaker <- trimws(potential_speaker_raw)

    # Remove any leading hashes that got captured (OCR artifacts)
    potential_speaker <- gsub("^#+\\s*", "", potential_speaker)
    # Remove any leading [number] that got captured
    potential_speaker <- gsub("^\\[\\d{1,3}\\]\\s*", "", potential_speaker)
    potential_speaker <- trimws(potential_speaker)

    # Skip "Formann" if it's preamble metadata (early in doc + short content after colon)
    # Preamble: "Formann : Hambro\n" vs Speaker: "Formann: Er det fleire som..."
    if (potential_speaker == "Formann") {
      # Check what comes after the colon until next newline
      after_colon_start <- match_starts[i] + match_lengths[i]
      after_colon_text <- substr(
        all_text,
        after_colon_start,
        min(after_colon_start + 100, nchar(all_text))
      )
      first_line <- strsplit(after_colon_text, "\n")[[1]][1]

      # If early in document (first 500 chars) AND short content after colon (< 40 chars), it's preamble
      is_early <- match_starts[i] < 500
      is_short_after <- nchar(trimws(first_line)) < 40

      if (is_early && is_short_after) {
        if (debug) {
          cat(
            "BLOCKED preamble Formann at position",
            match_starts[i],
            "- content after:",
            first_line,
            "\n"
          )
        }
        next
      }
    }

    # Skip if this looks like metadata (not a speaker)
    is_metadata_speaker <- any(sapply(metadata_patterns, function(p) {
      grepl(p, potential_speaker, ignore.case = FALSE)
    }))
    if (is_metadata_speaker) {
      if (debug) {
        cat("BLOCKED potential speaker:", potential_speaker, "\n")
      }
      next
    }

    if (debug) {
      cat("Checking potential speaker:", potential_speaker, "\n")
    }

    # Check if this matches any bold word
    # Require exact match, or prefix match for words in prefix_exceptions
    is_bold <- FALSE
    matched_word <- NULL

    for (bold_word in bold_dict) {
      # Exact match
      if (potential_speaker == bold_word) {
        is_bold <- TRUE
        matched_word <- bold_word
        break
      }
      # Prefix match for words in exceptions dictionary
      # e.g., "Formannen" matches "Formannen (Hambro)"
      # ONLY if the extension is a parenthetical, not a full sentence
      if (
        bold_word %in%
          prefix_exceptions &&
          startsWith(potential_speaker, bold_word) &&
          nchar(potential_speaker) > nchar(bold_word)
      ) {
        # Get the part after the bold word
        extension <- substr(
          potential_speaker,
          nchar(bold_word) + 1,
          nchar(potential_speaker)
        )
        extension <- trimws(extension)
        # Only match if extension is a parenthetical like "(Hambro)" or "(ikke gjennomsett av taleren)"
        if (grepl("^\\([^)]+\\)$", extension)) {
          is_bold <- TRUE
          matched_word <- bold_word
          break
        }
      }
    }

    # Check OCR name corrections if no match yet
    if (!is_bold && potential_speaker %in% names(ocr_name_corrections)) {
      correct_name <- ocr_name_corrections[potential_speaker]
      if (correct_name %in% bold_dict) {
        is_bold <- TRUE
        matched_word <- correct_name
        # Use the correct name as speaker (not the OCR error)
        potential_speaker <- correct_name
      }
    }

    if (is_bold) {
      if (debug) {
        cat(
          "MATCHED speaker:",
          potential_speaker,
          "via bold word:",
          matched_word,
          "\n"
        )
      }

      # Clean up speaker name - remove parenthetical content
      # e.g., "Formannen (Hambro)" -> "Formannen"
      clean_speaker <- gsub("\\s*\\([^)]*\\)\\s*$", "", potential_speaker)
      clean_speaker <- trimws(clean_speaker)

      speaker_markers[[length(speaker_markers) + 1]] <- list(
        position = match_starts[i] + match_lengths[i], # Position after colon
        speaker = clean_speaker,
        match_start = match_starts[i]
      )
      matched_bold <- c(matched_bold, matched_word)
      # Also mark the full speaker as matched if it's in bold_dict (for prefix matches)
      if (
        potential_speaker != matched_word && potential_speaker %in% bold_dict
      ) {
        matched_bold <- c(matched_bold, potential_speaker)
      }
    }
  }

  # Hard rule: "\n\nFormannen:" or "\nFormannen:" always treated as speaker
  # But ONLY if "Formannen" is the entire name (not start of a sentence)
  formannen_pattern <- "\\n+(Formannen):\\s*"
  formannen_matches <- gregexpr(formannen_pattern, all_text, perl = TRUE)[[1]]

  if (formannen_matches[1] != -1) {
    fm_lengths <- attr(formannen_matches, "match.length")
    fm_cap_starts <- attr(formannen_matches, "capture.start")[, 1]

    for (j in seq_along(formannen_matches)) {
      abs_position <- formannen_matches[j] + fm_lengths[j]
      abs_match_start <- formannen_matches[j]

      # Check if already captured
      already_exists <- FALSE
      if (length(speaker_markers) > 0) {
        already_exists <- any(sapply(speaker_markers, function(m) {
          abs(m$match_start - formannen_matches[j]) < 5
        }))
      }

      if (!already_exists) {
        speaker_markers[[length(speaker_markers) + 1]] <- list(
          position = abs_position,
          speaker = "Formannen",
          match_start = abs_match_start
        )
        matched_bold <- c(matched_bold, "Formannen")
      }
    }
  }

  # Sort markers by position
  if (length(speaker_markers) > 0) {
    positions <- sapply(speaker_markers, function(m) m$match_start)
    speaker_markers <- speaker_markers[order(positions)]
  }

  # Find unmatched bold words
  matched_bold <- unique(matched_bold)
  unmatched_bold <- setdiff(bold_dict, matched_bold)

  if (verbose) {
    cat("\n=== VALIDATION ===\n")
    cat("Matched bold words:", length(matched_bold), "\n")
    cat("Unmatched bold words:", length(unmatched_bold), "\n")
    if (length(unmatched_bold) > 0 && length(unmatched_bold) <= 20) {
      cat("Unmatched:", paste(unmatched_bold, collapse = ", "), "\n")
    }
    cat("Speaker markers found:", length(speaker_markers), "\n")
  }

  # Extract remarks
  speakers <- character()
  remarks <- character()

  for (i in seq_along(speaker_markers)) {
    marker <- speaker_markers[[i]]
    speakers <- c(speakers, marker$speaker)

    # Remark starts after the colon
    remark_start <- marker$position

    # Remark ends at the next speaker (or end of text)
    if (i < length(speaker_markers)) {
      remark_end <- speaker_markers[[i + 1]]$match_start - 1
    } else {
      remark_end <- nchar(all_text)
    }

    remark <- substr(all_text, remark_start, remark_end)
    remarks <- c(remarks, remark)
  }

  # Collapse whitespace in remarks
  remarks <- gsub("\\s+", " ", remarks)
  remarks <- trimws(remarks)

  # Build result dataframes
  filename <- sub("\\.json$", ".pdf", basename(ocr_json_path))

  remarks_df <- data.frame(
    filename = rep(filename, length(speakers)),
    index = seq_along(speakers),
    speaker_verbatim = speakers,
    remark = remarks,
    stringsAsFactors = FALSE
  )

  # Missing dataframe
  if (length(unmatched_bold) > 0) {
    missing_row <- as.list(unmatched_bold)
    names(missing_row) <- paste0("miss", seq_along(unmatched_bold))
    missing_row$filename <- filename
    missing_df <- as.data.frame(missing_row, stringsAsFactors = FALSE)
    # Reorder to put filename first
    missing_df <- missing_df[, c(
      "filename",
      setdiff(names(missing_df), "filename")
    )]
  } else {
    missing_df <- data.frame(filename = filename, stringsAsFactors = FALSE)
  }

  # Store validation info as attribute
  attr(remarks_df, "validation") <- list(
    matched = matched_bold,
    unmatched = unmatched_bold,
    skipped_metadata = skipped_metadata
  )

  list(
    remarks = remarks_df,
    missing = missing_df
  )
}


# Process all documents
all_remarks <- list()
all_missing <- list()

for (i in seq_along(ocr_jsons)) {
  result <- parse_bold_transcript(ocr_jsons[i], bold_jsons[i], verbose = FALSE)

  if (nrow(result$remarks) > 0) {
    all_remarks[[i]] <- result$remarks
  } else {
    cat("No remarks found in:", basename(ocr_jsons[i]), "\n")
  }

  all_missing[[i]] <- result$missing
}

# Remove NULLs and combine
all_remarks <- Filter(Negate(is.null), all_remarks)

remarks_df <- bind_rows(all_remarks)
missing_df <- bind_rows(all_missing)

write_csv(remarks_df, "temp/csv/speakers_pre76.csv")
write_csv(missing_df, "temp/csv/missing_bold.csv")

# compile
df1 <- read_csv("temp/csv/speakers_pre76.csv")
df2 <- read_csv("temp/csv/speakers_post75.csv")
df3 <- rbind(df1, df2)



# Add dates
df_doc <- read_csv("out/documents.csv")
df <- df3 |>
  left_join(df_doc[, c("filename", "date")], by = "filename") |>
  relocate(date, .before = index)

# save
#dir_create("out")
write_csv(df, "out/remarks.csv")
