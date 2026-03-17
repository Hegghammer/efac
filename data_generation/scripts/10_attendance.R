# ----------------------------------------
# Build meeting attendance from transcript preambles
#
# Extracts attendees from preambles (LLM-assisted),
# merges with speaker-based attendance, and assigns
# person identifiers and party metadata.
# ----------------------------------------

library(tidyverse)
library(ellmer)
library(stringdist)

# Extract attendance from preambles ----------------------------------------

df_spoke <- read_csv("out/remarks.csv")

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

output_file <- "temp/csv/df_preamble.csv"
log_file <- "temp/csv/processing_log.txt"

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
  txt_path <- file.path("data/txt", txt_path)

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

# Merge with speaker-based attendance ----------------------------------------

df <- read_csv("out/remarks_final.csv")

df_spoke <- df |>
  select(filename, meeting_id, speaker_verbatim, lastname, firstname, person_id, organization) |>
  distinct()

df_attend <- read_csv("temp/csv/df_preamble.csv")

# Correct typos manually (optional) ----------------------------------------
# df_distinct <- df_attend |>
#   distinct(attendee_verbatim) |>
#   arrange(attendee_verbatim) |>
#   mutate(corrected = "")

# write_csv(df_distinct, "temp/csv/attendee_verbatim_corrected_empty.csv")
# Manual editing here
df_corrected <- read_csv("temp/csv/attendee_verbatim_corrected.csv")

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

# write_csv(df_distinct, "temp/csv/attendee_verbatim_expanded_empty.csv")

df_expanded <- read_csv("temp/csv/attendee_verbatim_expanded.csv") |>
  filter(!is.na(attendee_verbatim)) |> 
  distinct(attendee_verbatim, .keep_all = TRUE)

df_all <- df_all |>
  left_join(
    df_expanded |>
      distinct(attendee_verbatim, .keep_all = TRUE) |>
      select(
        attendee_verbatim,
        lastname_new = lastname,
        firstname_new = firstname,
        title
      ),
    by = "attendee_verbatim"
  ) |>
  mutate(
    lastname = if_else(
      is.na(speaker_verbatim) & !is.na(lastname_new),
      lastname_new,
      lastname
    ),
    firstname = if_else(
      is.na(speaker_verbatim) & !is.na(firstname_new),
      firstname_new,
      firstname
    )
  ) |>
  select(meeting_id, speaker_verbatim, attendee_verbatim, title, lastname, firstname, organization, person_id, spoke)

write_csv(df_all, "temp/csv/attendance_full_edited_empty.csv")

# Manual editing of CSV here

# Fill missing identifiers ----------------------------------------

df <- read_csv("temp/csv/attendance_full_edited.csv")

df_unique <- df |>
  select(lastname, firstname, organization, person_id, pid) |>
  distinct(lastname, firstname, person_id, pid) |>
  arrange(lastname, firstname) |> 
  write_csv("temp/csv/attendance_unique.csv")

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
  filter(person_id == 90473)

unused <- setdiff(90001:90474, unique(df_unique$person_id))
length(unused)

df_noid <- df_unique |>
  filter(is.na(person_id))
nrow(df_noid) # 543
n_toadd <- nrow(df_noid) - length(unused) # 529

end <- max(df_unique$person_id, na.rm = T) + n_toadd
start <- max(df_unique$person_id, na.rm = T) + 1

df_noid$person_id <- c(unused, start:end)

df_unique2 <- df_unique |>
  left_join(df_noid, by = c("lastname", "firstname"), suffix = c("", "_new")) |>
  mutate(person_id = coalesce(person_id, person_id_new)) |>
  select(-c(person_id_new, pid_new))

df_unique2$person_id[is.na(df_unique2$lastname)] <- NA
write_csv(df_unique2, "temp/csv/attendance_unique_person_ids.csv")

df_unique2 <- read_csv("temp/csv/attendance_unique_person_ids.csv")

# merge back 
df2 <- df %>%
  select(-person_id, -pid) %>%
  left_join(df_unique2, by = c("lastname", "firstname"))

write_csv(df2, "temp/csv/attendance_full_edited_person_ids.csv")

df_fiva <- read_csv("temp/csv/fiva_ext.csv") |>
  select(lastname, firstname, pid)
names(df_fiva)

# look up pids
# dd a row ID to df2 before joining
df2 <- df2 %>%
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


write_csv(df4, "temp/csv/attendance_full_edited_all_ids.csv")

# Add party variable ----------------------------------------

df <- read_csv("out/attendance_final_edited.csv") |>
  arrange(meeting_id, lastname, firstname) |>
  select(-party)

df_fiva <- read_csv("temp/csv/fiva_ext.csv") 
names(df_fiva)

df_fiva_party <- df_fiva |>
  select(pid, party)

df2 <- df |>
  left_join(df_fiva_party, by = "pid")

df_remain <- df2 |>
  filter(category %in% c("Regjering", "Storting")) |>
  filter(is.na(party)) |>
  distinct(lastname, firstname) |>
  arrange(lastname) |>
  mutate(party = NA)

#write_csv(df_remain, "temp/csv/party_to_complete.csv")
# manual editing here
df_remain_added <- read_csv("temp/csv/party_to_complete.csv")

df3 <- df2 |>
  left_join(df_remain_added, by = c("lastname", "firstname")) |>
  mutate(party = coalesce(party.x, party.y)) |>
  select(-party.x, -party.y) |>
  select(
    meeting_id,
    lastname,
    firstname,
    title,
    organization,
    category,
    party,
    gender,
    person_id,
    pid,
    spoke,
    speaker_verbatim,
    attendee_verbatim
  ) |>
  arrange(meeting_id, lastname, firstname)

write_csv(df3, "out/attendance_final_edited.csv")

# Update remarks using final attendance ----------------------------------------

df_attend <- read_csv("out/attendance_final_edited.csv")
df_remark <- read_csv("out/remarks_final.csv")

# Helper functions ----------------------------------------

compare_firstnames <- function(fn1, fn2) {
  if (is.na(fn1) || is.na(fn2)) {
    return(1)
  }
  if (fn1 == "" || fn2 == "") {
    return(1)
  }

  parts1 <- str_split(fn1, "\\s+")[[1]]
  parts2 <- str_split(fn2, "\\s+")[[1]]

  first1 <- parts1[1]
  first2 <- parts2[1]

  first_name_dist <- stringdist(first1, first2, method = "jw")

  if (first_name_dist <= 0.15) {
    return(first_name_dist)
  }

  if (length(parts1) == 1) {
    min_dist <- min(sapply(parts2, function(p) stringdist(first1, p, method = "jw")))
    if (min_dist <= 0.15) {
      return(min_dist + 0.1)
    }
  }

  if (length(parts2) == 1) {
    min_dist <- min(sapply(parts1, function(p) stringdist(first2, p, method = "jw")))
    if (min_dist <= 0.15) {
      return(min_dist + 0.1)
    }
  }

  return(first_name_dist)
}

# Step 0: Separate matchable from unmatchable rows ----------------------------------------

# Rows without lastname cannot be reliably matched
df_remark_unmatchable <- df_remark %>%
  filter(is.na(lastname))

df_remark_matchable <- df_remark %>%
  filter(!is.na(lastname))

cat("df_remark total rows:      ", nrow(df_remark), "\n")
cat("  - Matchable (has lastname):", nrow(df_remark_matchable), "\n")
cat("  - Unmatchable (no lastname):", nrow(df_remark_unmatchable), "\n\n")

# Same for df_attend - we can only use entries with lastnames as reference
df_attend_matchable <- df_attend %>%
  filter(!is.na(lastname))

cat("df_attend total rows:      ", nrow(df_attend), "\n")
cat("  - Usable (has lastname):   ", nrow(df_attend_matchable), "\n")
cat("  - Unusable (no lastname):  ", nrow(df_attend) - nrow(df_attend_matchable), "\n\n")

# Step 1: Prepare matchable data ----------------------------------------

df_remark_matchable <- df_remark_matchable %>%
  mutate(
    lastname_clean = str_to_lower(str_trim(lastname)),
    firstname_clean = str_to_lower(str_trim(firstname)),
    firstname_clean = replace_na(firstname_clean, ""), # Some might have lastname but no firstname
    row_id = row_number()
  )

df_attend_matchable <- df_attend_matchable %>%
  mutate(
    lastname_clean = str_to_lower(str_trim(lastname)),
    firstname_clean = str_to_lower(str_trim(firstname)),
    firstname_clean = replace_na(firstname_clean, "")
  )

# Step 2: Exact matching on meeting_id + lastname + firstname ----------------------------------------

exact_match <- df_remark_matchable %>%
  inner_join(
    df_attend_matchable %>%
      select(meeting_id, lastname_clean, firstname_clean,
        lastname_attend = lastname,
        firstname_attend = firstname,
        person_id_attend = person_id
      ),
    by = c("meeting_id", "lastname_clean", "firstname_clean"),
    relationship = "many-to-one"
  ) %>%
  mutate(match_type = "exact")

cat("Exact matches:", nrow(exact_match), "of", nrow(df_remark_matchable), "\n")

unmatched_rows <- df_remark_matchable %>%
  filter(!row_id %in% exact_match$row_id)

# Step 3: Match on meeting_id + lastname only (unique lastname in meeting) ----------------------------------------

lastname_only_match <- unmatched_rows %>%
  inner_join(
    df_attend_matchable %>%
      group_by(meeting_id, lastname_clean) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      select(meeting_id, lastname_clean,
        lastname_attend = lastname,
        firstname_attend = firstname,
        person_id_attend = person_id
      ),
    by = c("meeting_id", "lastname_clean"),
    relationship = "many-to-one"
  ) %>%
  mutate(match_type = "lastname_unique")

cat("Lastname-unique matches:", nrow(lastname_only_match), "\n")

unmatched_rows <- unmatched_rows %>%
  filter(!row_id %in% lastname_only_match$row_id)

# Step 4: Match on meeting_id + lastname + first component of firstname ----------------------------------------

if (nrow(unmatched_rows) > 0) {
  unmatched_rows <- unmatched_rows %>%
    mutate(firstname_first = sapply(str_split(firstname_clean, "\\s+"), `[`, 1))

  df_attend_first <- df_attend_matchable %>%
    mutate(firstname_first = sapply(str_split(firstname_clean, "\\s+"), `[`, 1))

  firstname_component_match <- unmatched_rows %>%
    inner_join(
      df_attend_first %>%
        group_by(meeting_id, lastname_clean, firstname_first) %>%
        filter(n() == 1) %>%
        ungroup() %>%
        select(meeting_id, lastname_clean, firstname_first,
          lastname_attend = lastname,
          firstname_attend = firstname,
          person_id_attend = person_id
        ),
      by = c("meeting_id", "lastname_clean", "firstname_first"),
      relationship = "many-to-one"
    ) %>%
    mutate(match_type = "firstname_component")

  cat("Firstname-component matches:", nrow(firstname_component_match), "\n")

  unmatched_rows <- unmatched_rows %>%
    filter(!row_id %in% firstname_component_match$row_id)
} else {
  firstname_component_match <- tibble(
    row_id = integer(),
    lastname_attend = character(),
    firstname_attend = character(),
    person_id_attend = character(), # adjust type if needed (integer, etc.)
    match_type = character()
  )
  cat("Firstname-component matches: 0 (no unmatched rows)\n")
}

# Step 5: Fuzzy matching within each meeting ----------------------------------------

if (nrow(unmatched_rows) > 0) {
  cat("Running fuzzy matching...\n")
  fuzzy_matches <- list()
  meeting_ids <- unique(unmatched_rows$meeting_id)

  for (i in seq_along(meeting_ids)) {
    mid <- meeting_ids[i]
    remark_subset <- unmatched_rows %>% filter(meeting_id == mid)
    attend_subset <- df_attend_matchable %>% filter(meeting_id == mid)

    for (j in 1:nrow(remark_subset)) {
      match_result <- find_fuzzy_match(remark_subset[j, ], attend_subset)

      if (!is.null(match_result)) {
        fuzzy_matches[[length(fuzzy_matches) + 1]] <- tibble(
          row_id = remark_subset$row_id[j],
          meeting_id = mid,
          lastname_remark = remark_subset$lastname[j],
          firstname_remark = remark_subset$firstname[j],
          lastname_attend = match_result$lastname,
          firstname_attend = match_result$firstname,
          person_id_attend = match_result$person_id,
          lastname_dist = match_result$lastname_dist,
          firstname_dist = match_result$firstname_dist,
          combined_dist = match_result$combined_dist
        )
      }
    }

    if (i %% 100 == 0) cat("  Processed", i, "of", length(meeting_ids), "meetings\n")
  }

  fuzzy_match_df <- bind_rows(fuzzy_matches) %>%
    mutate(match_type = "fuzzy")

  cat("Fuzzy matches:", nrow(fuzzy_match_df), "\n")
} else {
  fuzzy_match_df <- tibble(
    row_id = integer(),
    lastname_attend = character(),
    firstname_attend = character(),
    person_id_attend = character(),
    match_type = character()
  )
  cat("Fuzzy matches: 0 (no unmatched rows)\n")
}

# Step 6: Create review files ----------------------------------------

if (nrow(fuzzy_match_df) > 0) {
  fuzzy_review <- fuzzy_match_df %>%
    select(
      row_id, meeting_id,
      original_lastname = lastname_remark,
      original_firstname = firstname_remark,
      matched_lastname = lastname_attend,
      matched_firstname = firstname_attend,
      person_id_attend,
      lastname_dist, firstname_dist, combined_dist
    ) %>%
    arrange(desc(combined_dist))

  write_csv(fuzzy_review, "fuzzy_matches_for_review.csv")
  cat("Saved fuzzy matches for review to 'fuzzy_matches_for_review.csv'\n")
} else {
  cat("No fuzzy matches to review\n")
}

all_matched_ids <- c(
  exact_match$row_id,
  lastname_only_match$row_id,
  firstname_component_match$row_id,
  fuzzy_match_df$row_id
)

completely_unmatched <- df_remark_matchable %>%
  filter(!row_id %in% all_matched_ids) %>%
  select(row_id, meeting_id, lastname, firstname, person_id)

if (nrow(completely_unmatched) > 0) {
  write_csv(completely_unmatched, "unmatched_remarks_for_review.csv")
  cat("Unmatched rows:", nrow(completely_unmatched), "\n")
  cat("Saved to 'unmatched_remarks_for_review.csv'\n")
} else {
  cat("No unmatched rows - all remarks matched successfully!\n")
}

# Step 7: Combine matches and apply updates ----------------------------------------

standardize_match_df <- function(df) {
  df %>%
    select(row_id, lastname_attend, firstname_attend, person_id_attend, match_type) %>%
    mutate(
      row_id = as.integer(row_id),
      lastname_attend = as.character(lastname_attend),
      firstname_attend = as.character(firstname_attend),
      person_id_attend = as.double(person_id_attend), # adjust to your type
      match_type = as.character(match_type)
    )
}

all_matches <- bind_rows(
  standardize_match_df(exact_match),
  standardize_match_df(lastname_only_match),
  standardize_match_df(firstname_component_match),
  standardize_match_df(fuzzy_match_df)
)

df_remark_matched <- df_remark_matchable %>%
  left_join(all_matches, by = "row_id") %>%
  mutate(
    lastname_new = coalesce(lastname_attend, lastname),
    firstname_new = coalesce(firstname_attend, firstname),
    person_id_new = coalesce(person_id_attend, person_id)
  )

# Summary ----------------------------------------

cat("\n========================================\n")
cat("MATCHING SUMMARY\n")
cat("========================================\n")
cat("Total rows in df_remark:     ", nrow(df_remark), "\n")
cat("  Unmatchable (no lastname): ", nrow(df_remark_unmatchable), "\n")
cat("  Matchable:                 ", nrow(df_remark_matchable), "\n")
cat("----------------------------------------\n")
cat("Exact matches:               ", sum(all_matches$match_type == "exact"), "\n")
cat("Lastname-unique matches:     ", sum(all_matches$match_type == "lastname_unique"), "\n")
cat("Firstname-component matches: ", sum(all_matches$match_type == "firstname_component"), "\n")
cat("Fuzzy matches:               ", sum(all_matches$match_type == "fuzzy"), "\n")
cat("----------------------------------------\n")
cat("Total matched:               ", nrow(all_matches), "\n")
cat("Unmatched (with lastname):   ", nrow(completely_unmatched), "\n")
cat("========================================\n")

# Step 8: Create final dataframe with unmatchable rows added back ----------------------------------------

# Get the original column names from df_remark
original_cols <- names(df_remark)

df_remark_clean <- df_remark_matched %>%
  mutate(
    lastname = lastname_new,
    firstname = firstname_new,
    person_id = person_id_new
  ) %>%
  select(all_of(original_cols))

# Add back unmatchable rows (they keep their original values)
df_remark_final <- bind_rows(
  df_remark_clean,
  df_remark_unmatchable
) |>
  arrange(meeting_id, index)

# Verify we haven't lost any rows
cat("\nFINAL VERIFICATION\n")
cat("Original df_remark rows:", nrow(df_remark), "\n")
cat("Final df_remark rows:   ", nrow(df_remark_final), "\n")
cat("Row counts match:       ", nrow(df_remark_final) == nrow(df_remark), "\n")

# Save outputs ----------------------------------------

write_csv(df_remark_final, "out/remarks.csv")

df_docs <- read_csv("out/documents.csv")
names(df_docs)
