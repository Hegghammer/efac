# ----------------------------------------
# Process later-released EFAC document additions
#
# Downloads newly released documents, runs OCR and
# segmentation, applies manual correction checkpoints,
# and merges additions into core output tables.
# ----------------------------------------

library(tidyverse)
library(fs)
library(httr2)
library(base64enc)
library(jsonlite)
library(ellmer)
library(rvest)
library(xml2)

add_dir <- "data/addition"
pdf_dir <- dir_create(file.path(add_dir, "pdf"))
ocr_out <- dir_create(file.path(add_dir, "json", "ocr"))
csv_dir <- dir_create(file.path(add_dir, "csv"))
pdfs <- dir_ls(pdf_dir)

# Download additions ----------------------------------------

landing_pages <- c(
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1924-1945/"
)

# download pdfs and store urls in the process
urls <- character()
for (i in seq_along(landing_pages)) {
  page <- read_html(landing_pages[i])
  links <- page |>
    html_nodes("a") |>
    html_attr("href")
  pdf_links <- grep(".pdf$", links, value = TRUE)
  pdf_urls <- paste0("https://www.stortinget.no", pdf_links)
  urls <- c(urls, pdf_urls)
}

urls <- urls[8:14]

for (j in seq_along(urls)) {
    download.file(urls[j], file.path(pdf_dir, basename(urls[j])), mode = "wb")
  }

# create and store df
df_files <- urls |>
  data.frame() |>
  rename(url = urls) |>
  mutate(filename = basename(url))

df_files_path <- file.path(csv_dir, "df_files_add.csv")

write_csv(df_files, df_files_path)

# OCR additions ----------------------------------------

mistral_api_key <- Sys.getenv("MISTRAL_API_KEY")

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

# Process with skip logic for already-completed files
process_pdfs <- function(pdfs, output_dir = "data/json/ocrs") {
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

process_pdfs(pdfs, output_dir = ocr_out)

ocr_jsons <- dir_ls(ocr_out)
txt_out <- dir_create(file.path(add_dir, "txt"))

for (i in seq_along(ocr_jsons)) {
  message("Processing json file ", i, " of ", length(ocr_jsons), " ...")
  json <- ocr_jsons[i]
  content <- fromJSON(json)
  text <- paste0(content$pages$markdown, collapse = "\n\n")
  path <- file.path(txt_out, str_replace(basename(json), "json", "txt"))
  write(text, path)
}

# Build metadata for additions ----------------------------------------

page <- read_html(landing_pages[1])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 42

link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- grep("\\d{1,2}\\. [A-Za-z]+ \\d{4}", link_texts, value = TRUE)
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
df_dates <- data.frame(raw = dates_raw, parsed = date) # check

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}$")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

title <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]") |>
  map_chr(~ {
    xml_remove(xml_find_first(.x, "./a[1]"))
    html_text2(.x)
  }) |>
  str_squish()

df1 <- data.frame(date, time, title)
df_meta <- df1[7:13,]
df_meta$title <- c(
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet",
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet",
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet",
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet",
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet",
  "Statsministerens redegjørelse om Grønlandsspørsmålet",
  "Redegjørelse fra utenriksministeren om Øst-Grønlands-spørsmålet"
)

df_files <- read_csv(df_files_path)

# Collate
df <- cbind(df_files, df_meta) |>
  select(filename, date, time, title, url)

# Add full document text ----------------------------------------

# get actual inventory
txt_files <- dir_ls(txt_out)

# get path vector from df (for right order)
txt_paths <- file.path(txt_out, str_replace(df$filename, "pdf", "txt"))

# check
setdiff(txt_files, txt_paths)

# add to df
df$text <- map_chr(txt_paths, read_file)
names(df)
# load existing
df_docs <- read_csv("out/documents.csv")
names(df_docs)
df_new <- rbind(df, df_docs) |>
  arrange(date, filename)

# Save final version
write_csv(df_new, "out/documents.csv")

# Get bold words from pre-1967 additions ----------------------------------------

type_page <- type_object(
  "Bold words found on a single page. Remove the colon at the end.",
  page_number = type_number("The page number"),
  bold_words = type_array(type_string(), "All words in bold on this page")
)

type_bold <- type_object(
  "Bold words found across all pages of the document",
  pages = type_array(type_page, "One entry per page in the document")
)

output_dir <- dir_create(file.path(add_dir, "json", "bold"))

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

# Collate pre-1976 additions ----------------------------------------

ocr_jsons <- dir_ls(ocr_out)
bold_jsons <- dir_ls(output_dir)

parse_bold_transcript <- function(
  ocr_json_path,
  bold_json_path,
  verbose = TRUE,
  debug = FALSE
) {
  # EXCEPTIONS DICTIONARIES - Edit these lists as needed
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
  # ============================================================

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

# Check missing
missing_df <- bind_rows(all_missing)

# Add dates
df_doc <- read_csv("out/documents.csv")
df2 <- remarks_df |>
  left_join(df_doc[, c("filename", "date")], by = "filename") |>
  relocate(date, .before = index)

# save
write_csv(df2, file.path(csv_dir, "remarks_add.csv"))

# Clean additions ----------------------------------------

# Define helper functions
remove_latex_notation <- function(text) {
  text |>
    # Fractions: $1 \frac{1}{2}$ → 1½
    str_replace_all(
      "\\$(\\d*)\\s*\\\\frac\\{1\\}\\{2\\}([^$]*)\\$",
      "\\1½\\2"
    ) |>
    str_replace_all(
      "\\$(\\d*)\\s*\\\\frac\\{1\\}\\{4\\}([^$]*)\\$",
      "\\1¼\\2"
    ) |>
    str_replace_all(
      "\\$(\\d*)\\s*\\\\frac\\{3\\}\\{4\\}([^$]*)\\$",
      "\\1¾\\2"
    ) |>
    # Fractions: $21 / 2$ → 2½
    str_replace_all("\\$(\\d*)1\\s*/\\s*2\\$", "\\1½") |>
    str_replace_all("\\$(\\d*)1\\s*/\\s*4\\$", "\\1¼") |>
    str_replace_all("\\$(\\d*)3\\s*/\\s*4\\$", "\\1¾") |>
    # Fraction + degrees: $681 / 2^{\circ}$ → 68½°
    str_replace_all("\\$(\\d*)1\\s*/\\s*2\\^\\{\\\\circ\\}\\$", "\\1½°") |>
    # Paragraph sign: $\S 34$ → § 34
    str_replace_all("\\$\\\\S\\s*(\\d+)\\$", "§ \\1") |>
    # Units with superscript: $48000 \mathrm{~km}^{2}$ → 48000 km²
    str_replace_all(
      "\\$([\\d\\.,\\-]+)\\s*\\\\mathrm\\{~?(\\w+)\\}\\^\\{2\\}\\$?",
      "\\1 \\2²"
    ) |>
    str_replace_all(
      "\\$([\\d\\.,\\-]+)\\s*\\\\mathrm\\{~?(\\w+)\\}\\^\\{3\\}\\$?",
      "\\1 \\2³"
    ) |>
    # Units without superscript (must come after superscript patterns)
    str_replace_all(
      "\\$([\\d\\.,\\-]+)\\s*\\\\mathrm\\{~?(\\w+)\\}\\$?",
      "\\1 \\2"
    ) |>
    # Degrees, minutes, seconds
    str_replace_all(
      "\\$([\\d\\.,]+)\\^\\{\\\\circ\\}\\s*([\\d\\.,]+)\\^\\{\\\\prime\\}\\s*([\\d\\.,]+)\\^\\{\\\\prime\\s*\\\\prime\\}\\$",
      "\\1° \\2′ \\3″"
    ) |>
    # Coordinates with sign and minutes
    str_replace_all(
      "\\$([+\\-]|\\\\div\\s*)([\\d\\.,]+)\\^\\{\\\\circ\\}\\s*([\\d\\.,]+)\\^\\{\\\\prime\\}\\$",
      "\\1\\2° \\3′"
    ) |>
    # Coordinates
    str_replace_all(
      "\\$([\\d\\.,]+)\\^\\{\\\\circ\\}\\s*([\\d\\.,]+)\\^\\{\\\\prime\\}\\$",
      "\\1° \\2′"
    ) |>
    # Degrees with cardinal direction
    str_replace_all(
      "\\$([\\d\\.,]+)\\^\\{\\\\circ\\}\\s*\\\\mathrm\\{([NSEW])\\}\\$",
      "\\1° \\2"
    )
}

remove_superscripts <- function(text) {
  # Match ${ }^{N}$ NOT preceded by "[^0] [^0]: "
  pattern <- "(?<!\\[\\^0\\] \\[\\^0\\]: )\\$\\{ \\}\\^\\{\\d+\\}\\$"
  str_replace_all(text, pattern, "")
}

remove_stenographer_initials <- function(text) {
  margin_signs <- c(
    "8W",
    "KL",
    "IS",
    "ME",
    "AGO",
    "SL",
    "BW",
    "SA",
    "LR",
    "IL",
    "EL",
    "EK",
    "KML",
    "GSR",
    "LM",
    "AGK",
    "RML"
  )
  signs_pattern <- str_c(margin_signs, collapse = "|")

  text |>
    str_replace_all(str_c("\\(\\s+(", signs_pattern, ")\\s*\\)"), "") |>
    str_replace_all(
      str_c("(?<![A-Za-z])(", signs_pattern, ")(?![A-Za-z])"),
      ""
    ) |>
    str_replace_all("\\(\\)", "")
}

# Load data -
df <- read_csv(file.path(csv_dir, "remarks_add.csv"))

# Clean remarks -
df <- df |>
  mutate(
    remark_clean = remark |>
      str_replace_all(c(
        "ö" = "ø",
        "Ö" = "Ø",
        "# HEMMELIG" = "",
        "# HEMMELIC" = "",
        " Møtet hevet kl[ \\.0-9]*" = "",
        " Møtet slutt [a-zA-Z0-9\\.]*" = "",
        " KAPITEL .*" = "",
        "\\. Sak nr[\\. ].*" = ".",
        "\\- Sak nr[\\. ].*" = "",
        "\\. Saknr.*" = ".",
        "\\$\\\\emptyset\\$ " = "ø",
        "\\$\\\\AA\\$" = "Å",
        "\\( KL\\$\\)\\$" = "",
        "\\( EK\\$\\)\\$" = "",
        "\\( KML\\$\\)\\$" = "",
        "\\[\\d{1,3}\\]" = "",
        "Stein Ørnhøi tok opp de vanskelige høreforholdene i det møtelokalet som blir benyttet av den ut videde utenrikskomite, og det utspant seg en livlig meningsutveksling om dette\\. " = "",
        "\\. PER HYSING-DAHL frafalt ordet\\." = "",
        "\\$\\\\cdot\\$" = " ",
        "\\$\\\\varnothing\\$ " = "ø",
        " \\$\\\\boldsymbol\\{j\\}\\$ " = "ø",
        " \\$\\\\phi\\$ " = "ø",
        " \\$\\\\emptyset \\\\mathrm\\{k\\}\\$" = "øk",
        "\\$\\$ \\\\begin\\{aligned\\} \\& \\\\text \\{ E v.*" = "eventuelt.",
        "\\$\\$ \\\\begin\\{aligned\\}.*?\\\\end\\{aligned\\} \\$\\$" = "",
        "\\$50-50\\$" = "50-50",
        "\\$1\\^\\{1\\}\\$" = "14",
        "\\$6\\^\\{1\\)\\}\\$" = "6",
        "\\#" = ""
      )) |>
      remove_latex_notation() |>
      remove_superscripts() |>
      remove_stenographer_initials() |>
      str_replace_all(" \\-$", "") |>
      str_squish(),
    speaker_verbatim = speaker_verbatim |>
      str_replace_all(c(
        "^\\(" = "",
        "\\[97b\\] " = "",
        " ALY " = " ALV ",
        "ASBJERN" = "ASBJØRN",
        "ASBJÜRN SJÜTHUN" = "ASBJØRN SJØTHUN",
        "^SPEDISJONSSJEF" = "EKSPEDISJONSSJEF",
        "FJELDVER" = "FJELDVÆR",
        "FORMAENEN|FÖRMANNEN|FORMANIER|FORMANHEN|FORMANNES" = "FORMANNEN",
        "Formarmen" = "Formannen",
        "FORSYARSMINISTER" = "FORSVARSMINISTER",
        "FRYDENLIIND|FRYDERLUND" = "FRYDENLUND",
        "KNUITFRYDENLUND" = "KNUT FRYDENLUND",
        "EUNG\\.|FUNG," = "FUNG.",
        "GRÜNDAHL" = "GRØNDAHL",
        "GUTTOHM|GUTTOBM|GUTTGRM" = "GUTTORM",
        "HANNA-KVANMO" = "HANNA KVANMO",
        "HANG HAMMOND" = "HANS HAMMOND",
        "HAUGSTYEDT" = "HAUGSTVEDT",
        "JAKOB AKNO" = "JAKOB AANO",
        "JAKOBSES" = "JAKOBSEN",
        "JENS SVENSEN" = "JENS EVENSEN",
        "JOBENKOW" = "JO BENKOW",
        "^JOANN " = "JOHAN ",
        "KJELLBJDRG" = "KJELLBJØRG",
        "ENUT FRYDENLUND" = "KNUT FRYDENLUND",
        "KORYALD" = "KORVALD",
        "MORR EIDEM" = "MØRK-EIDEM",
        "PER-KRISTIAN FOSS" = "PER KRISTIAN FOSS",
        "SJEPDIREKTØR" = "SJEFDIREKTØR",
        "SEATSMINISTER" = "STATSMINISTER",
        "^TSRAAD" = "STATSRAAD",
        "^ETATSRÅD" = "STATSRÅD",
        "STATSRÅD\\. ELDRID" = "STATSRÅD ELDRID",
        "STATSSEKRETAR|STATSSEKRETER" = "STATSSEKRETÆR",
        "STEERBERG" = "STEENBERG",
        "STEINER KVALÜ" = "STEINER KVALØ",
        "STRXY" = "STRAY",
        "SYENN" = "SVENN",
        "TXNNING" = "TYNNING",
        "^UNIHER|цітENRIKSMINISTER|UTERRIKSMINISTER|UITENRIKSMINISTER|UTENRIKSHINISTER|UTTENRIKSMINISTER|UTENRTKSMINISTER|ULENRIKSMINISTER|UTERRIKSMINISTER|UTMNRIKSMINISTER|UENRIKSMINISTER" = "UTENRIKSMINISTER ",
        "VERNÜ|VERNÕ" = "VERNØ",
        "ORNHÖI|ERNHØI|ÜRNHÖI|ORNHOI|ÜRNHÜI" = "ØRNHØI",
        "ÜSTBY|ÖSTRY" = "ØSTBY",
        "ö" = "ø",
        "Ö" = "Ø"
      ))
  ) |>
  # Fix indices and finalize
  group_by(filename) |>
  mutate(index = row_number()) |>
  ungroup() |>
  mutate(remark = remark_clean) |>
  select(-remark_clean)

# Write to file --
write_csv(df, file.path(csv_dir, "remarks_add.csv"))
# NB: After this, footnotes -- identified with "[^0] [^0]" -- are manually removed.

# Create expanded version for editing
df2 <- df |>
  mutate(meeting_id = paste0(str_replace_all(date, "\\-", ""), "0")) |>
  relocate(meeting_id, .after = filename)

df2 <- df2 |>
  mutate(
    lastname = NA,
    firstname = NA,
    organization = NA,
    person_id = NA,
    pid = NA
  ) |>
  select(
    filename, meeting_id, date, index, speaker_verbatim, lastname, firstname, organization, person_id, pid, remark
)
# write_csv(df2, file.path(csv_dir, "remarks_add_edited.csv"))
# Now edit manually

df_remarks_ed <- read_csv(file.path(csv_dir, "remarks_add_edited.csv"))

# merge in
df_all <- read_csv("out/remarks_final.csv") |>
  relocate(organization, .before = "person_id") |>
  arrange(meeting_id, index)

df_all_new <- rbind(df_all, df_remarks_ed) |>
  arrange(meeting_id, index)

write_csv(df_all_new, "out/remarks_final.csv")

# Build attendance for additions ----------------------------------------

df_spoke <- read_csv(file.path(csv_dir, "remarks_add_edited.csv"))

system_prompt <- "
Du er en ekspert på å analysere referater fra Stortingets utvidede utenriks- og konstitusjonskomité.

## OPPGAVE
Identifiser alle personer som var TIL STEDE på møtet, basert på preambelen.

## REGLER FOR INKLUDERING
INKLUDER personer som:
- Er listet i navneopprop ('Til stede var: Hambro, Madsen, Støstad')
- Er nevnt som varamenn ('Smitt Ingebretsen for Hambro' → inkluder Smitt Ingebretsen)
- Er eksplisitt nevnt som til stede ('Utenriksminister Lange var til stede')
- Er regjeringsmedlemmer til stede
- Er embetsmenn som fikk adgang
- Er innkalte eksperter
- Ledet møtet ('Formann: Terje Wold' → inkluder Terje Wold)

- Noter personens rolle og organisasjon hvis nevnt (f.eks. 'Fra Utenriksdepartementet: ekspedisjonssjef Vibe' → role = 'ekspedisjonssjef', organization = 'Utenriksdepartementet')

## REGLER FOR EKSKLUDERING
IKKE INKLUDER:
- Personer nevnt som FRAVÆRENDE ('Fraværende var: Handberg')
- Personer noen møter FOR ('Smitt Ingebretsen for Hambro' → IKKE inkluder Hambro)
- Personer som har meldt forfall
- Navn fra diskusjonen ETTER preambelen

## HVOR SLUTTER PREAMBELEN?
Preambelen slutter når diskusjonen begynner:
- Et navn/tittel etterfulgt av kolon, deretter fulle setninger med innhold
- F.eks. 'Formannen: Jeg føler at vi kanskje...'
"

attendee_type <- type_array(
  items = type_object(
    attendee_verbatim = type_string("Navnet slik det står i teksten, inkludert tittel"),
    lastname = type_string("Etternavn"),
    firstname = type_string("Fornavn hvis oppgitt, ellers tom streng"),
    role = type_enum("Rolle", values = c("committee_member", "substitute", "minister", "official", "expert", "chair")),
    organization = type_string("Organisasjon eller departement hvis nevnt, ellers tom streng")
  )
)

output_file <- "data/addition/csv/df_preamble_add.csv"
log_file <- "data/addition/csv/processing_log.txt"

# Load existing results if any
if (file.exists(output_file)) {
  df_preamble <- read_csv(output_file, show_col_types = FALSE)
  completed <- unique(df_preamble$meeting_id)
  message(paste("Resuming. Already processed:", length(completed), "meetings"))
} else {
  df_preamble <- tibble()
  completed <- character(0)
}

extract_attendance <- function(meeting_id, filename) {
  txt_path <- filename |> str_replace("\\.pdf$", ".txt")
  txt_path <- file.path("data/addition/txt", txt_path)

  if (!file.exists(txt_path)) {
    return(list(success = FALSE, error = "File not found", data = NULL))
  }

  preamble <- read_file(txt_path) |> str_sub(1, 2000)

  user_prompt <- paste0(
    "Identifiser alle personer som var til stede basert på preambelen.\n\n",
    "Referat:\n", preamble
  )

  chat <- chat_anthropic(
    # model = "claude-sonnet-4-20250514", 
    model = "claude-haiku-4-5-20251001", 
    system_prompt = system_prompt
  )

  attendees <- tryCatch(
    chat$chat_structured(user_prompt, type = attendee_type),
    error = function(e) {
      return(list(success = FALSE, error = e$message, data = NULL))
    }
  )

  if (is.list(attendees) && !is.null(attendees$error)) {
    return(attendees)
  }

  if (is.null(attendees) || nrow(attendees) == 0) {
    return(list(success = TRUE, error = NA, data = NULL))
  }

  attendees$meeting_id <- meeting_id
  list(success = TRUE, error = NA, data = attendees)
}

# Get meetings still to process
meetings <- df_spoke |>
  distinct(meeting_id, filename) |>
  filter(!meeting_id %in% completed)

message(paste("Meetings to process:", nrow(meetings)))

# Process one at a time, saving as we go
for (i in seq_len(nrow(meetings))) {
  meeting_id <- meetings$meeting_id[i]
  filename <- meetings$filename[i]

  message(paste0("[", i, "/", nrow(meetings), "] Processing: ", meeting_id))

  result <- extract_attendance(meeting_id, filename)

  # Log result
  log_entry <- paste(
    Sys.time(),
    meeting_id,
    ifelse(result$success, "OK", "FAILED"),
    ifelse(is.na(result$error), "", result$error),
    sep = "\t"
  )
  write_lines(log_entry, log_file, append = TRUE)

  # Save data if any
  if (result$success && !is.null(result$data)) {
    df_preamble <- bind_rows(df_preamble, result$data)
    write_csv(df_preamble, output_file)
  }

  Sys.sleep(0.5) # rate limiting
}

# Merge addition attendance with existing structure ----------------------------------------

df_spoke <- df_spoke |>
  select(filename, meeting_id, speaker_verbatim, lastname, firstname, person_id, organization) |>
  distinct()

df_attend <- read_csv("data/addition/csv/df_preamble_add.csv") |>
  relocate(meeting_id, .before = "attendee_verbatim")

# Correct typos manually (optional) ----------------------------------------
df_distinct <- df_attend |>
  distinct(attendee_verbatim) |>
  arrange(attendee_verbatim) |>
  mutate(corrected = "")

# write_csv(df_distinct, "data/addition/csv/attendee_verbatim_add_corrected_empty.csv")
# Manual editing here
df_corrected <- read_csv("data/addition/csv/attendee_verbatim_add_corrected.csv")

df_attend_correct <- df_attend |>
  left_join(df_corrected, by = "attendee_verbatim") |>
  mutate(attendee_verbatim = if_else(!is.na(corrected) & corrected != "", corrected, attendee_verbatim)) |>
  select(-corrected) |>
  mutate(
    attendee_verbatim = str_replace_all(attendee_verbatim, "ö", "ø"),
    attendee_verbatim = str_replace_all(attendee_verbatim, "Ö", "Ø"),
    lastname = str_replace_all(lastname, "ö", "ø"),
    lastname = str_replace_all(lastname, "Ö", "Ø"),
    firstname = str_replace_all(firstname, "ö", "ø"),
    firstname = str_replace_all(firstname, "Ö", "Ø")
    )

# Merge sources ----------------------------------------

# Add source column to track origin
df_spoke <- df_spoke |>
  mutate(source = "spoke")

df_attend_correct <- df_attend_correct |>
  mutate(source = "preamble")

df_all <- bind_rows(df_spoke, df_attend_correct) |>
  arrange(meeting_id, lastname, desc(source == "spoke")) |>
  distinct(meeting_id, lastname, .keep_all = TRUE) |>
  mutate(spoke = if_else(source == "spoke", 1, 0)) |>
  select(meeting_id, speaker_verbatim, attendee_verbatim, lastname, firstname, person_id, role, organization, spoke)

# Deduplicate preamble/speaker overlaps ----------------------------------------
# This removes any preamble entry (spoke == 0) where the attendee_verbatim matches a speaker_verbatim from the same meeting—regardless of what's in the lastname field.

# Find speaker titles per meeting (from spoke rows)
speaker_titles <- df_all |>
  filter(spoke == 1, !is.na(speaker_verbatim)) |>
  transmute(meeting_id, title = str_to_lower(speaker_verbatim))

# Find preamble rows where attendee_verbatim matches a speaker title
rows_to_remove <- df_all |>
  mutate(row_id = row_number()) |>
  filter(spoke == 0, !is.na(attendee_verbatim)) |>
  mutate(title = str_to_lower(attendee_verbatim)) |>
  semi_join(speaker_titles, by = c("meeting_id", "title")) |>
  pull(row_id)

# Remove them
df_all <- df_all |>
  mutate(row_id = row_number()) |>
  filter(!row_id %in% rows_to_remove) |>
  select(-row_id)

# Normalize names/titles ----------------------------------------
# df_distinct <- df_all |>
#   distinct(attendee_verbatim, lastname, firstname) |>
#   mutate(title = "") |>
#   relocate(title, .before = lastname) |>
#   arrange(attendee_verbatim)

# write_csv(df_distinct, "data/addition/csv/attendee_verbatim_add_expanded_empty.csv")

# df_expanded <- read_csv("data/addition/csv/attendee_verbatim_add_expanded.csv") |>
#   filter(!is.na(attendee_verbatim)) |>
#   distinct(attendee_verbatim, .keep_all = TRUE)

# df_all <- df_all |>
#   left_join(
#     df_expanded |>
#       distinct(attendee_verbatim, .keep_all = TRUE) |>
#       select(
#         attendee_verbatim,
#         lastname_new = lastname,
#         firstname_new = firstname,
#         title
#       ),
#     by = "attendee_verbatim"
#   ) |>
#   mutate(
#     lastname = if_else(
#       is.na(speaker_verbatim) & !is.na(lastname_new),
#       lastname_new,
#       lastname
#     ),
#     firstname = if_else(
#       is.na(speaker_verbatim) & !is.na(firstname_new),
#       firstname_new,
#       firstname
#     )
#   ) |>
#   select(meeting_id, speaker_verbatim, attendee_verbatim, title, lastname, firstname, organization, person_id, spoke)

df_all2 <- df_all |>
  rename(title = role) |>
  mutate(pid = NA) |> 
  select(meeting_id, speaker_verbatim, attendee_verbatim, title, lastname, firstname, organization, spoke, person_id, pid)

write_csv(df_all2, "data/addition/csv/attendance_add_edited.csv")

# Manual editing of CSV here

# Fill missing identifiers ----------------------------------------

df <- read_csv("data/addition/csv/attendance_add_edited.csv")

df_unique <- df |>
  select(lastname, firstname, organization, person_id, pid) |>
  distinct(lastname, firstname, person_id, pid) |>
  arrange(lastname, firstname) |> 
  write_csv("data/addition/csv/attendance_unique.csv")

sort(unique(df_unique$person_id))

length(sort(unique(df_unique$person_id)))
length(na.omit(df_unique$person_id))

duplicate_ids <- df_unique |>
  group_by(person_id) |>
  filter(n() > 1) |>
  distinct(person_id) |>
  pull(person_id)
print(duplicate_ids)

df_unique |>
  filter(person_id == 91008)

df_fiva <- read_csv("temp/csv/fiva_ext.csv") |>
  select(candidatename_ed, lastname, firstname, pid)
names(df_fiva)

# look up pids
# dd a row ID to df2 before joining
df2 <- df %>%
  mutate(
    row_id = row_number(),
    fullname = paste(firstname, lastname)
  )

# Do the join
df2_matched <- df2 %>%
  left_join(
    df_fiva %>% rename(pid_fiva = pid, firstname_fiva = firstname),
    by = "lastname",
    relationship = "many-to-many"
  ) %>%
  mutate(
    name_match = !is.na(candidatename_ed) & (
      str_detect(fullname, paste0("^", str_escape(candidatename_ed), "(\\s|$|-|,)")) |
        str_detect(candidatename_ed, paste0("^", str_escape(fullname), "(\\s|$|-)"))
    )
  )

# Now collapse using row_id
df3 <- df2_matched %>%
  group_by(row_id) %>%
  summarise(
    pid_fiva = first(pid_fiva[name_match]),
    .groups = "drop"
  )

# Join back to original df2
df4 <- df2 %>%
  left_join(df3, by = "row_id") %>%
  mutate(pid = if_else(is.na(pid), pid_fiva, pid)) %>%
  select(-pid_fiva, -row_id, -fullname) |>
  arrange(meeting_id, lastname, firstname)

write_csv(df4, "data/addition/csv/attendance_add_all_ids.csv")

df_att <- read_csv("temp/csv/attendance_with_early_years.csv")
df_att_final <- rbind(df_att, df4) |>
  arrange(meeting_id, lastname, firstname)

write_csv(df_att_final, "out/attendance_final_edited.csv")

df <- read_csv("out/attendance_final_edited.csv")

max(df$person_id, na.rm = T)
