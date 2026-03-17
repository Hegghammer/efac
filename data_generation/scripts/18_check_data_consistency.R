# ----------------------------------------
# Validate consistency across core dataset tables
#
# Runs schema, key, referential-integrity, value,
# and plausibility checks across persons, attendees,
# documents, and remarks. Exits non-zero on failures.
# ----------------------------------------

library(tidyverse)
library(cli)

# Configuration ----------------------------------------
data_dir <- Sys.getenv("EFAC_CORE_CSV_DIR", "../article/csv")

# Load data ----------------------------------------
cli_h1("Loading data")
persons   <- read_csv(file.path(data_dir, "persons.csv"),   show_col_types = FALSE)
attendees <- read_csv(file.path(data_dir, "attendees.csv"), show_col_types = FALSE)
documents <- read_csv(file.path(data_dir, "documents.csv"), show_col_types = FALSE)
remarks   <- read_csv(file.path(data_dir, "remarks.csv"),   show_col_types = FALSE)

cli_alert_info("persons:   {nrow(persons)} rows")
cli_alert_info("attendees: {nrow(attendees)} rows")
cli_alert_info("documents: {nrow(documents)} rows")
cli_alert_info("remarks:   {nrow(remarks)} rows")

# Helper ----------------------------------------
# Each check appends to a results tibble. At the end we summarise.
results <- tibble(
  check   = character(),
  status  = character(),
  n_issues = integer(),
  details = character()
)

add_result <- function(check, ok, n_issues = 0L, details = "") {
  status <- if (ok) "PASS" else "FAIL"
  if (ok) {
    cli_alert_success("{check}")
  } else {
    cli_alert_danger("{check}  [{n_issues} issue{?s}]")
    if (nchar(details) > 0) cli_text("    {.val {details}}")
  }
  results <<- bind_rows(results, tibble(check, status, n_issues, details))
}

# ----------------------------------------
cli_h1("Schema checks")
# ----------------------------------------

# 1. Expected columns ----------------------------------------
expected_cols <- list(
  persons   = c("person_id", "lastname", "firstname", "gender", "pid"),
  attendees = c("meeting_id", "person_id", "chairman", "title",
                "organization", "category", "party", "spoke"),
  documents = c("filename", "meeting_id", "date", "doctitle", "url", "text"),
  remarks   = c("meeting_id", "person_id", "index", "remark", "lang")
)

for (tbl_name in names(expected_cols)) {
  tbl <- get(tbl_name)
  missing <- setdiff(expected_cols[[tbl_name]], names(tbl))
  extra   <- setdiff(names(tbl), expected_cols[[tbl_name]])
  ok <- length(missing) == 0 && length(extra) == 0
  detail_parts <- c()
  if (length(missing) > 0) detail_parts <- c(detail_parts, paste("missing:", paste(missing, collapse = ", ")))
  if (length(extra) > 0)   detail_parts <- c(detail_parts, paste("extra:", paste(extra, collapse = ", ")))
  add_result(
    glue::glue("{tbl_name}: expected columns present"),
    ok,
    n_issues = length(missing) + length(extra),
    details  = paste(detail_parts, collapse = "; ")
  )
}

# ----------------------------------------
cli_h1("Primary key checks")
# ----------------------------------------

# 2. persons.person_id is unique ----------------------------------------
dup_person_ids <- persons |> filter(duplicated(person_id)) |> pull(person_id)
add_result(
  "persons: person_id is unique",
  length(dup_person_ids) == 0,
  n_issues = length(dup_person_ids),
  details  = paste(head(dup_person_ids, 10), collapse = ", ")
)

# 3. persons.person_id has no NAs ----------------------------------------
n_na <- sum(is.na(persons$person_id))
add_result("persons: person_id has no NAs", n_na == 0, n_issues = n_na)

# 4. documents: filename is the primary key (unique, no NAs) ----------------------------------------
# Note: meeting_id is NOT unique in documents.csv because two meetings
# (197910030, 198211190) are split across two PDF files each.
dup_fn_docs <- documents |> filter(duplicated(filename)) |> pull(filename)
add_result(
  "documents: filename is unique",
  length(dup_fn_docs) == 0,
  n_issues = length(dup_fn_docs),
  details  = paste(head(dup_fn_docs, 10), collapse = ", ")
)

# 5. remarks: meeting_id + index is unique ----------------------------------------
dup_remark_key <- remarks |>
  group_by(meeting_id, index) |>
  filter(n() > 1)
add_result(
  "remarks: (meeting_id, index) is unique",
  nrow(dup_remark_key) == 0,
  n_issues = nrow(dup_remark_key)
)

# 6. attendees: meeting_id + person_id is unique (excl NA person_id) ----------------------------------------
# NAs are excluded because some meetings have multiple unidentified attendees
dup_attend_key <- attendees |>
  filter(!is.na(person_id)) |>
  group_by(meeting_id, person_id) |>
  filter(n() > 1)
n_dup <- nrow(dup_attend_key)
detail <- ""
if (n_dup > 0) {
  detail <- dup_attend_key |>
    distinct(meeting_id, person_id) |>
    mutate(label = paste0(meeting_id, "/", person_id)) |>
    pull(label) |>
    head(10) |>
    paste(collapse = ", ")
}
add_result(
  "attendees: (meeting_id, person_id) is unique",
  n_dup == 0,
  n_issues = n_dup,
  details  = detail
)

# ----------------------------------------
cli_h1("Referential integrity: meeting_id")
# ----------------------------------------

doc_meeting_ids <- documents |> filter(!is.na(meeting_id)) |> pull(meeting_id)

# 7. All meeting_ids in remarks exist in documents ----------------------------------------
orphan_mid_remarks <- setdiff(remarks$meeting_id, doc_meeting_ids)
add_result(
  "remarks -> documents: all meeting_ids exist",
  length(orphan_mid_remarks) == 0,
  n_issues = length(orphan_mid_remarks),
  details  = paste(head(orphan_mid_remarks, 10), collapse = ", ")
)

# 8. All meeting_ids in attendees exist in documents ----------------------------------------
orphan_mid_attend <- setdiff(attendees$meeting_id, doc_meeting_ids)
add_result(
  "attendees -> documents: all meeting_ids exist",
  length(orphan_mid_attend) == 0,
  n_issues = length(orphan_mid_attend),
  details  = paste(head(orphan_mid_attend, 10), collapse = ", ")
)

# 9. All meeting_ids in documents appear in remarks ----------------------------------------
# (every document with a meeting_id should have at least one remark)
# Some fully redacted documents may have attendees but no remarks.
mid_no_remarks <- setdiff(doc_meeting_ids, remarks$meeting_id)
if (length(mid_no_remarks) > 0) {
  no_remark_files <- documents |>
    filter(meeting_id %in% mid_no_remarks) |>
    pull(filename) |>
    paste(collapse = ", ")
  cli_alert_warning(
    "documents -> remarks: {length(mid_no_remarks)} meeting(s) have no remarks (possibly redacted): {no_remark_files}"
  )
} else {
  add_result("documents -> remarks: all meeting_ids have remarks", TRUE)
}

# 10. All meeting_ids in documents appear in attendees ----------------------------------------
mid_no_attend <- setdiff(doc_meeting_ids, attendees$meeting_id)
add_result(
  "documents -> attendees: all meeting_ids have attendees",
  length(mid_no_attend) == 0,
  n_issues = length(mid_no_attend),
  details  = paste(head(mid_no_attend, 10), collapse = ", ")
)

# ----------------------------------------
cli_h1("Referential integrity: person_id")
# ----------------------------------------

person_ids <- persons$person_id

# 11. All person_ids in remarks exist in persons (excl NA) ----------------------------------------
orphan_pid_remarks <- remarks |>
  filter(!is.na(person_id), !person_id %in% person_ids) |>
  distinct(person_id) |>
  pull(person_id)
add_result(
  "remarks -> persons: all person_ids exist (excl NA)",
  length(orphan_pid_remarks) == 0,
  n_issues = length(orphan_pid_remarks),
  details  = paste(head(orphan_pid_remarks, 10), collapse = ", ")
)

# 12. All person_ids in attendees exist in persons (excl NA) ----------------------------------------
orphan_pid_attend <- attendees |>
  filter(!is.na(person_id), !person_id %in% person_ids) |>
  distinct(person_id) |>
  pull(person_id)
add_result(
  "attendees -> persons: all person_ids exist (excl NA)",
  length(orphan_pid_attend) == 0,
  n_issues = length(orphan_pid_attend),
  details  = paste(head(orphan_pid_attend, 10), collapse = ", ")
)

# 13. All person_ids in persons appear in at least one other table ----------------------------------------
used_pids <- union(
  attendees |> filter(!is.na(person_id)) |> pull(person_id),
  remarks   |> filter(!is.na(person_id)) |> pull(person_id)
)
unused_pids <- setdiff(person_ids, used_pids)
add_result(
  "persons: all person_ids referenced in attendees or remarks",
  length(unused_pids) == 0,
  n_issues = length(unused_pids),
  details  = paste(head(unused_pids, 10), collapse = ", ")
)

# ----------------------------------------
cli_h1("Cross-table consistency: spoke flag")
# ----------------------------------------

speakers_in_remarks <- remarks |>
  filter(!is.na(person_id)) |>
  distinct(meeting_id, person_id)

# 14. spoke=1 in attendees -> person appears in remarks ----------------------------------------
spoke_not_in_remarks <- attendees |>
  filter(spoke == 1, !is.na(person_id)) |>
  anti_join(speakers_in_remarks, by = c("meeting_id", "person_id"))
n <- nrow(spoke_not_in_remarks)
detail <- ""
if (n > 0) {
  detail <- spoke_not_in_remarks |>
    mutate(label = paste0(meeting_id, "/", person_id)) |>
    pull(label) |>
    head(10) |>
    paste(collapse = ", ")
}
add_result(
  "attendees spoke=1 -> person appears in remarks",
  n == 0,
  n_issues = n,
  details  = detail
)

# 15. Person in remarks -> spoke=1 in attendees ----------------------------------------
in_remarks_not_spoke <- speakers_in_remarks |>
  left_join(
    attendees |> select(meeting_id, person_id, spoke),
    by = c("meeting_id", "person_id")
  ) |>
  filter(is.na(spoke) | spoke != 1)
n <- nrow(in_remarks_not_spoke)
detail <- ""
if (n > 0) {
  detail <- in_remarks_not_spoke |>
    mutate(label = paste0(meeting_id, "/", person_id)) |>
    pull(label) |>
    head(10) |>
    paste(collapse = ", ")
}
add_result(
  "remarks speaker -> attendees spoke=1",
  n == 0,
  n_issues = n,
  details  = detail
)

# 16. Every meeting has exactly one chairman ----------------------------------------
chairman_counts <- attendees |>
  group_by(meeting_id) |>
  summarise(n_chair = sum(chairman == 1, na.rm = TRUE), .groups = "drop")

no_chair  <- chairman_counts |> filter(n_chair == 0) |> pull(meeting_id)
multi_chair <- chairman_counts |> filter(n_chair > 1) |> pull(meeting_id)
n_issues <- length(no_chair) + length(multi_chair)
detail_parts <- c()
if (length(no_chair) > 0)    detail_parts <- c(detail_parts, paste("no chairman:", paste(head(no_chair, 5), collapse = ", ")))
if (length(multi_chair) > 0) detail_parts <- c(detail_parts, paste("multiple chairmen:", paste(head(multi_chair, 5), collapse = ", ")))
add_result(
  "attendees: each meeting has exactly one chairman",
  n_issues == 0,
  n_issues = n_issues,
  details  = paste(detail_parts, collapse = "; ")
)

# ----------------------------------------
cli_h1("Value validity checks")
# ----------------------------------------

# 17. meeting_id format: 9 digits, YYYYMMDD + index ----------------------------------------
all_mids <- c(
  documents$meeting_id,
  attendees$meeting_id,
  remarks$meeting_id
) |> na.omit() |> unique()

bad_format <- all_mids[!str_detect(as.character(all_mids), "^\\d{9}$")]
add_result(
  "meeting_id: all are 9-digit integers",
  length(bad_format) == 0,
  n_issues = length(bad_format),
  details  = paste(head(bad_format, 10), collapse = ", ")
)

# 18. meeting_id date part encodes a valid date ----------------------------------------
mid_dates <- tibble(meeting_id = all_mids) |>
  mutate(
    mid_str  = as.character(meeting_id),
    date_str = substr(mid_str, 1, 8),
    parsed   = as.Date(date_str, format = "%Y%m%d")
  ) |>
  filter(is.na(parsed))
add_result(
  "meeting_id: date portion encodes a valid date",
  nrow(mid_dates) == 0,
  n_issues = nrow(mid_dates),
  details  = paste(head(mid_dates$meeting_id, 10), collapse = ", ")
)

# 19. meeting_id date matches documents.date ----------------------------------------
date_mismatch <- documents |>
  filter(!is.na(meeting_id)) |>
  mutate(
    mid_str    = as.character(meeting_id),
    mid_date   = as.Date(substr(mid_str, 1, 8), format = "%Y%m%d"),
    doc_date   = as.Date(date),
    mismatch   = mid_date != doc_date
  ) |>
  filter(mismatch)
n <- nrow(date_mismatch)
detail <- ""
if (n > 0) {
  detail <- date_mismatch |>
    mutate(label = paste0(meeting_id, ": mid=", mid_date, " doc=", doc_date)) |>
    pull(label) |>
    head(10) |>
    paste(collapse = "; ")
}
add_result(
  "documents: meeting_id date matches date column",
  n == 0,
  n_issues = n,
  details  = detail
)

# 20. documents.date is within expected range ----------------------------------------
doc_dates <- documents |>
  filter(!is.na(date)) |>
  mutate(d = as.Date(date))
out_of_range <- doc_dates |>
  filter(d < as.Date("1924-01-01") | d > as.Date("1992-12-31"))
add_result(
  "documents: dates within 1924-1992",
  nrow(out_of_range) == 0,
  n_issues = nrow(out_of_range),
  details  = paste(head(out_of_range$date, 10), collapse = ", ")
)

# 21. person_id format: 5-digit integer >= 90000 ----------------------------------------
all_pids <- persons$person_id
bad_pid <- all_pids[all_pids < 90000 | all_pids > 99999]
add_result(
  "persons: person_id is 5-digit integer in 90000-99999",
  length(bad_pid) == 0,
  n_issues = length(bad_pid),
  details  = paste(head(bad_pid, 10), collapse = ", ")
)

# 22. gender values ----------------------------------------
valid_genders <- c("kvinne", "mann")
bad_gender <- persons |> filter(!gender %in% valid_genders)
add_result(
  "persons: gender is 'kvinne' or 'mann'",
  nrow(bad_gender) == 0,
  n_issues = nrow(bad_gender),
  details  = paste(unique(bad_gender$gender), collapse = ", ")
)

# 23. chairman is 0 or 1 ----------------------------------------
bad_chair <- attendees |> filter(!chairman %in% c(0, 1))
add_result(
  "attendees: chairman is 0 or 1",
  nrow(bad_chair) == 0,
  n_issues = nrow(bad_chair)
)

# 24. spoke is 0 or 1 ----------------------------------------
bad_spoke <- attendees |> filter(!spoke %in% c(0, 1))
add_result(
  "attendees: spoke is 0 or 1",
  nrow(bad_spoke) == 0,
  n_issues = nrow(bad_spoke)
)

# 25. category values ----------------------------------------
valid_categories <- c("Regjering", "Storting", "Embetsverk",
                      "Næringsliv", "Akademia",
                      "Rettsvesen", "Stortingssekretariat")
bad_cat <- attendees |>
  filter(!is.na(category), !category %in% valid_categories) |>
  distinct(category) |>
  pull(category)
add_result(
  "attendees: category in expected set",
  length(bad_cat) == 0,
  n_issues = length(bad_cat),
  details  = paste(bad_cat, collapse = ", ")
)

# 26. lang values ----------------------------------------
valid_langs <- c("nb", "nn")
bad_lang <- remarks |>
  filter(!is.na(lang), !lang %in% valid_langs) |>
  distinct(lang) |>
  pull(lang)
add_result(
  "remarks: lang is 'nb' or 'nn'",
  length(bad_lang) == 0,
  n_issues = length(bad_lang),
  details  = paste(bad_lang, collapse = ", ")
)

# 27. index is positive integer ----------------------------------------
bad_index <- remarks |> filter(index < 1 | index != as.integer(index))
add_result(
  "remarks: index is positive integer",
  nrow(bad_index) == 0,
  n_issues = nrow(bad_index)
)

# 28. index is sequential within each meeting (1, 2, 3, ...) ----------------------------------------
non_seq <- remarks |>
  group_by(meeting_id) |>
  arrange(index) |>
  mutate(expected = row_number(), gap = index != expected) |>
  filter(gap) |>
  ungroup()
n <- n_distinct(non_seq$meeting_id)
add_result(
  "remarks: index is sequential within each meeting",
  n == 0,
  n_issues = n,
  details = if (n > 0) paste(head(unique(non_seq$meeting_id), 5), collapse = ", ") else ""
)

# ----------------------------------------
cli_h1("Content plausibility checks")
# ----------------------------------------

# 29. No empty remarks ----------------------------------------
empty_remarks <- remarks |>
  filter(is.na(remark) | str_squish(remark) == "")
add_result(
  "remarks: no empty remark text",
  nrow(empty_remarks) == 0,
  n_issues = nrow(empty_remarks)
)

# 30. No extremely long remarks (> 20000 words, likely parse error) ----------------------------------------
word_counts <- remarks |>
  mutate(wc = str_count(remark, "\\S+")) |>
  filter(wc > 20000)
add_result(
  "remarks: no remarks > 20,000 words (possible merge error)",
  nrow(word_counts) == 0,
  n_issues = nrow(word_counts),
  details  = if (nrow(word_counts) > 0) {
    word_counts |>
      mutate(label = paste0(meeting_id, "/idx=", index, " (", wc, " words)")) |>
      pull(label) |> paste(collapse = ", ")
  } else ""
)

# 31. (Moved to primary key checks above) ----------------------------------------

# 32. documents.text is not empty (excl partially redacted) ----------------------------------------
empty_text <- documents |>
  filter(!is.na(meeting_id)) |>
  filter(is.na(text) | str_squish(text) == "")
add_result(
  "documents: text is non-empty for docs with meeting_id",
  nrow(empty_text) == 0,
  n_issues = nrow(empty_text),
  details  = paste(head(empty_text$filename, 5), collapse = ", ")
)

# 33. documents.url is a valid URL ----------------------------------------
bad_url <- documents |>
  filter(!is.na(url), !str_detect(url, "^https?://"))
add_result(
  "documents: url starts with http(s)://",
  nrow(bad_url) == 0,
  n_issues = nrow(bad_url),
  details  = paste(head(bad_url$url, 5), collapse = ", ")
)

# 34. documents.filename ends in .pdf ----------------------------------------
bad_ext <- documents |> filter(!str_detect(filename, "\\.pdf$"))
add_result(
  "documents: filename ends in .pdf",
  nrow(bad_ext) == 0,
  n_issues = nrow(bad_ext),
  details  = paste(head(bad_ext$filename, 5), collapse = ", ")
)

# 35. persons: no duplicate (lastname, firstname) pairs ----------------------------------------
dup_names <- persons |>
  group_by(lastname, firstname) |>
  filter(n() > 1) |>
  ungroup()
n <- n_distinct(dup_names$person_id)
add_result(
  "persons: no duplicate (lastname, firstname) pairs",
  n == 0,
  n_issues = n,
  details  = if (n > 0) {
    dup_names |>
      mutate(label = paste0(firstname, " ", lastname, " [", person_id, "]")) |>
      pull(label) |> head(10) |> paste(collapse = ", ")
  } else ""
)

# 36. persons.pid is unique where non-NA ----------------------------------------
dup_pids <- persons |>
  filter(!is.na(pid)) |>
  filter(duplicated(pid)) |>
  pull(pid)
add_result(
  "persons: pid is unique (where non-NA)",
  length(dup_pids) == 0,
  n_issues = length(dup_pids),
  details  = paste(head(dup_pids, 10), collapse = ", ")
)

# 37. party values are plausible (no stray values) ----------------------------------------
known_parties <- c(
  "a", "dna", "frp", "h", "h_fv", "krf", "nkp", "sf", "sp",
  "sv", "v", "fv", "bl", "bp", "oth", NA
)
bad_party <- attendees |>
  filter(!is.na(party), !party %in% known_parties) |>
  distinct(party) |>
  pull(party)
# Report rather than fail -- just flag unexpected values
if (length(bad_party) > 0) {
  add_result(
    "attendees: party values are in expected set",
    FALSE,
    n_issues = length(bad_party),
    details  = paste(bad_party, collapse = ", ")
  )
} else {
  add_result("attendees: party values are in expected set", TRUE)
}

# 38. Attendees with spoke=1 should be > 0 per meeting ----------------------------------------
# Exclude meetings with no remarks (e.g. fully redacted documents)
meetings_with_remarks <- remarks |> distinct(meeting_id) |> pull(meeting_id)
meetings_no_speakers <- attendees |>
  filter(meeting_id %in% meetings_with_remarks) |>
  group_by(meeting_id) |>
  summarise(n_spoke = sum(spoke == 1, na.rm = TRUE), .groups = "drop") |>
  filter(n_spoke == 0)
add_result(
  "attendees: every meeting with remarks has at least one speaker",
  nrow(meetings_no_speakers) == 0,
  n_issues = nrow(meetings_no_speakers),
  details  = paste(head(meetings_no_speakers$meeting_id, 5), collapse = ", ")
)

# 39. Remarks per meeting > 0 ----------------------------------------
# Partially redacted meetings may have very few remarks.
remarks_per_meeting <- remarks |>
  count(meeting_id, name = "n_remarks")
single_remark <- remarks_per_meeting |> filter(n_remarks < 2)
if (nrow(single_remark) > 0) {
  sr_files <- documents |>
    filter(meeting_id %in% single_remark$meeting_id) |>
    pull(filename) |>
    paste(collapse = ", ")
  cli_alert_warning(
    "remarks: {nrow(single_remark)} meeting(s) have fewer than 2 remarks (possibly redacted): {sr_files}"
  )
} else {
  add_result("remarks: every meeting has at least 2 remarks", TRUE)
}

# 40. No stray whitespace in person names ----------------------------------------
bad_ws <- persons |>
  filter(
    str_detect(lastname, "^\\s|\\s$") |
    (!is.na(firstname) & str_detect(firstname, "^\\s|\\s$"))
  )
add_result(
  "persons: no leading/trailing whitespace in names",
  nrow(bad_ws) == 0,
  n_issues = nrow(bad_ws),
  details  = if (nrow(bad_ws) > 0) {
    bad_ws |>
      mutate(label = paste0("'", firstname, " ", lastname, "' [", person_id, "]")) |>
      pull(label) |> head(5) |> paste(collapse = ", ")
  } else ""
)

# ----------------------------------------
cli_h1("Summary")
# ----------------------------------------

n_pass <- sum(results$status == "PASS")
n_fail <- sum(results$status == "FAIL")
n_total <- nrow(results)

cli_rule()
cli_alert_info("Total checks: {n_total}")
cli_alert_success("Passed: {n_pass}")
if (n_fail > 0) {
  cli_alert_danger("Failed: {n_fail}")
  cli_h2("Failed checks")
  failed <- results |> filter(status == "FAIL")
  for (i in seq_len(nrow(failed))) {
    row <- failed[i, ]
    cli_alert_danger("{row$check}  [{row$n_issues} issue(s)]")
    if (nchar(row$details) > 0) cli_text("    {row$details}")
  }
} else {
  cli_alert_success("All checks passed!")
}
cli_rule()

# Exit with appropriate code
if (n_fail > 0) quit(status = 1, save = "no")
