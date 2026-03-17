# ----------------------------------------
# Consolidate final release CSV tables
#
# Rebuilds normalized documents/persons/
# attendees/remarks tables, runs integrity checks,
# and writes final outputs to out/.
# ----------------------------------------

library(tidyverse)

df_attend <- read_csv("out/attendees.csv")
df_remark_final <- read_csv("out/remarks.csv")
df_documents <- read_csv("out/documents.csv")

# Step 1: Create persons.csv ----------------------------------------

# Extract unique persons from attendees (only those with person_id)
persons <- df_attend %>%
  filter(!is.na(person_id)) %>%
  group_by(person_id) %>%
  summarise(
    lastname = first(na.omit(lastname)),
    firstname = first(na.omit(firstname)),
    gender = first(na.omit(gender)),
    pid = first(na.omit(pid)),
    .groups = "drop"
  )

cat("Unique identified persons:", nrow(persons), "\n")

# Check how many unidentified attendees we have
unidentified_attendees <- df_attend %>%
  filter(is.na(person_id))

cat("Unidentified attendee records:", nrow(unidentified_attendees), "\n")

# Check for any person_ids in remarks that aren't in attendees
remark_person_ids <- df_remark_final %>%
  filter(!is.na(person_id)) %>%
  distinct(person_id)

missing_persons <- remark_person_ids %>%
  anti_join(persons, by = "person_id")

if (nrow(missing_persons) > 0) {
  cat("WARNING:", nrow(missing_persons), "person_ids in remarks not found in attendees\n")

  persons_from_remarks <- df_remark_final %>%
    filter(person_id %in% missing_persons$person_id) %>%
    group_by(person_id) %>%
    summarise(
      lastname = first(na.omit(lastname)),
      firstname = first(na.omit(firstname)),
      gender = NA_character_,
      pid = NA_character_,
      .groups = "drop"
    )

  persons <- bind_rows(persons, persons_from_remarks)
  cat("Added missing persons. Total persons:", nrow(persons), "\n")
}

# Step 2: Clean up documents.csv (one row per file) ----------------------------------------

# Get meeting_id to filename mapping
meeting_id_lookup <- df_remark_final %>%
  distinct(meeting_id, filename)

# Join meeting_id to documents, keep one row per file
documents <- df_documents %>%
  left_join(meeting_id_lookup, by = "filename") %>%
  select(
    meeting_id,
    filename,
    date,
    time,
    title,
    url,
    text
  )

documents <- documents |>
  rename(doctitle = title)

documents <- documents |>
  relocate(meeting_id, .after = "filename") |>
  arrange(meeting_id)

# Step 3: Clean up attendees.csv ----------------------------------------

# For identified attendees: use person_id, drop name columns
attendees_identified <- df_attend %>%
  filter(!is.na(person_id)) %>%
  select(
    meeting_id,
    person_id,
    title,
    organization,
    category,
    party,
    spoke,
    attendee_verbatim,
    speaker_verbatim
  )

# For unidentified attendees: keep description columns, person_id will be NA
attendees_unidentified <- df_attend %>%
  filter(is.na(person_id)) %>%
  select(
    meeting_id,
    person_id,
    title,
    organization,
    category,
    party,
    spoke,
    attendee_verbatim,
    speaker_verbatim
  )

attendees <- bind_rows(attendees_identified, attendees_unidentified)

cat("Attendee records:", nrow(attendees), "\n")
cat("  - Identified:  ", nrow(attendees_identified), "\n")
cat("  - Unidentified:", nrow(attendees_unidentified), "\n")

# Step 4: Clean up remarks.csv ----------------------------------------

remarks <- df_remark_final %>%
  select(
    meeting_id,
    person_id,
    index,
    remark
  )

cat("Remark records:", nrow(remarks), "\n")

# Check for unidentified speakers
unidentified_remarks <- remarks %>% filter(is.na(person_id))
cat("  - With person_id:   ", nrow(remarks) - nrow(unidentified_remarks), "\n")
cat("  - Without person_id:", nrow(unidentified_remarks), "\n")

# Step 5: Validation checks ----------------------------------------

cat("\n========================================\n")
cat("VALIDATION CHECKS\n")
cat("========================================\n")

# Check all meeting_ids in remarks exist in documents
remarks_meeting_ids <- remarks %>% distinct(meeting_id)
docs_meeting_ids <- documents %>% distinct(meeting_id)

orphan_remarks <- anti_join(remarks_meeting_ids, docs_meeting_ids, by = "meeting_id")
if (nrow(orphan_remarks) > 0) {
  cat("WARNING:", nrow(orphan_remarks), "meeting_ids in remarks not found in documents\n")
} else {
  cat("OK: All remark meeting_ids exist in documents\n")
}

# Check all meeting_ids in attendees exist in documents
attendees_meeting_ids <- attendees %>% distinct(meeting_id)

orphan_attendees <- anti_join(attendees_meeting_ids, docs_meeting_ids, by = "meeting_id")
if (nrow(orphan_attendees) > 0) {
  cat("WARNING:", nrow(orphan_attendees), "meeting_ids in attendees not found in documents\n")
} else {
  cat("OK: All attendee meeting_ids exist in documents\n")
}

# Check all person_ids in remarks exist in persons (excluding NAs)
remarks_with_person <- remarks %>%
  filter(!is.na(person_id)) %>%
  distinct(person_id)
orphan_remark_persons <- anti_join(remarks_with_person, persons, by = "person_id")
if (nrow(orphan_remark_persons) > 0) {
  cat("WARNING:", nrow(orphan_remark_persons), "person_ids in remarks not found in persons\n")
} else {
  cat("OK: All identified remark person_ids exist in persons\n")
}

# Check all person_ids in attendees exist in persons (excluding NAs)
attendees_with_person <- attendees %>%
  filter(!is.na(person_id)) %>%
  distinct(person_id)
orphan_attendee_persons <- anti_join(attendees_with_person, persons, by = "person_id")
if (nrow(orphan_attendee_persons) > 0) {
  cat("WARNING:", nrow(orphan_attendee_persons), "person_ids in attendees not found in persons\n")
} else {
  cat("OK: All identified attendee person_ids exist in persons\n")
}

# Step 6: Inspect unidentified attendees ----------------------------------------

cat("\n========================================\n")
cat("UNIDENTIFIED ATTENDEES\n")
cat("========================================\n")

# Show what information we have for unidentified attendees
unidentified_summary <- attendees_unidentified %>%
  count(title, organization, category, sort = TRUE)

cat("\nTop unidentified attendee types:\n")
print(unidentified_summary %>% head(15))

# Step 7: Summary statistics ----------------------------------------

cat("\n========================================\n")
cat("FINAL SUMMARY\n")
cat("========================================\n")
cat("documents.csv: ", nrow(documents), "rows,", ncol(documents), "columns\n")
cat("persons.csv:   ", nrow(persons), "rows,", ncol(persons), "columns\n")
cat("attendees.csv: ", nrow(attendees), "rows,", ncol(attendees), "columns\n")
cat("remarks.csv:   ", nrow(remarks), "rows,", ncol(remarks), "columns\n")

cat("\nColumn names:\n")
cat("  documents:", paste(names(documents), collapse = ", "), "\n")
cat("  persons:  ", paste(names(persons), collapse = ", "), "\n")
cat("  attendees:", paste(names(attendees), collapse = ", "), "\n")
cat("  remarks:  ", paste(names(remarks), collapse = ", "), "\n")

# Step 8: Save to CSV ----------------------------------------

write_csv(documents, "out/documents.csv")
write_csv(persons, "out/persons.csv")
write_csv(attendees, "out/attendees.csv")
write_csv(remarks, "out/remarks.csv")

cat("\nFiles saved successfully!\n")
