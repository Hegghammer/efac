# ----------------------------------------
# Identify and enrich speakers in remarks
#
# Matches speaker labels to names and IDs using
# prior lookup tables and the Fiva parliamentary
# dataset, then writes enriched remarks to out/remarks.csv.
# ----------------------------------------

library(tidyverse)
library(fs)
library(haven)

# Build Fiva lookup ----------------------------------------
url <- "https://www.jon.fiva.no/data/FivaSmith2025.zip"
file <- basename(url)
destdir <- dir_create("temp/misc/fiva")
path <- file.path(destdir, file)
download.file(
  url,
  destfile = path,
  method = "libcurl",
  mode = "wb",
  headers = c(
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/120.0"
  )
)
unzip(path, exdir = destdir)
data_file <- dir_ls(destdir, glob = "*.dta")
df_mps <- read_dta(data_file)
names(df_mps)

election_years <- sort(as.integer(unique(df_mps$year)))
election_years <- election_years[election_years %in% 1924:1992]
df_relevant <- df_mps |>
  # filter(elected == 1, year %in% election_years) |>
  filter(year %in% election_years) |>
  filter(elected == 1 | deputy == 1) |> 
  select(year, elected, deputy, candidatename_ed, lastname, firstname, pid, party) |>
  arrange(lastname) |>
  distinct(candidatename_ed, .keep_all = TRUE) |>
  mutate(
    firstname = str_replace_all(firstname, "\\d", ""), # there are numbers in some names
    lastname = str_replace_all(lastname, "\\d", ""),
    candidatename_ed = str_replace_all(candidatename_ed, "\\d", "")
  )
  
write_csv(df_relevant, "temp/csv/fiva.csv")
write_csv(df_relevant, "temp/csv/fiva_ext.csv")

file.remove(data_file) # it's over 100MB

# Load source data ----------------------------------------

df <- read_csv("out/remarks.csv")
df_mp <- read_csv("temp/csv/fiva.csv")
df_first <- read_csv("temp/csv/duufk_speakers_first_try.csv") |>
  select(-remark) |>
  arrange(date) |>
  filter(!filename == "1977_1229.pdf")

# Helper functions ----------------------------------------
# Function to normalize Norwegian characters for fuzzy matching
normalize_norwegian <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("Ø", "O") %>%
    str_replace_all("Å", "A") %>%
    str_replace_all("Æ", "AE") %>%
    str_replace_all("ø", "O") %>%
    str_replace_all("å", "A") %>%
    str_replace_all("æ", "AE")
}

# Part 1: Merge speaker metadata from df_first ----------------------------------------

# Step 1: Create file-level lookup from df_first (metadata only)
file_lookup <- df_first %>%
  select(filename, speaker_verbatim, lastname, firstname, mp_id, organization) %>%
  distinct() %>%
  group_by(filename, speaker_verbatim) %>%
  slice(1) %>%
  ungroup()

# Step 2: Join to df (exact match on filename + speaker_verbatim)
df_merged <- df %>%
  select(filename, date, index, speaker_verbatim, remark) %>%
  left_join(file_lookup, by = c("filename", "speaker_verbatim"))

n_matched <- sum(!is.na(df_merged$lastname))
print(paste(
  "After exact match:", n_matched, "/", nrow(df_merged),
  "(", round(100 * n_matched / nrow(df_merged), 1), "%)"
))

# Step 3: Fallback with normalized matching for OCR errors
file_lookup_norm <- df_first %>%
  select(filename, speaker_verbatim, lastname, firstname, mp_id, organization) %>%
  distinct() %>%
  mutate(speaker_norm = normalize_norwegian(speaker_verbatim)) %>%
  group_by(filename, speaker_norm) %>%
  slice(1) %>%
  ungroup() %>%
  select(filename, speaker_norm,
    lastname_norm = lastname,
    firstname_norm = firstname, mp_id_norm = mp_id,
    organization_norm = organization
  )

df_merged <- df_merged %>%
  mutate(speaker_norm = normalize_norwegian(speaker_verbatim)) %>%
  left_join(file_lookup_norm, by = c("filename", "speaker_norm")) %>%
  mutate(
    lastname = coalesce(lastname, lastname_norm),
    firstname = coalesce(firstname, firstname_norm),
    mp_id = coalesce(mp_id, mp_id_norm),
    organization = coalesce(organization, organization_norm)
  ) %>%
  select(-ends_with("_norm"), -speaker_norm)

# Step 4: Add matched indicator
df_merged <- df_merged %>%
  mutate(matched = ifelse(is.na(lastname), 0, 1))

print(table(df_merged$matched))
print(paste("Match rate:", round(100 * mean(df_merged$matched), 1), "%"))

# Part 2: Add pid from df_mp ----------------------------------------

# Step 5: Prepare df_merged for pid matching
df_merged <- df_merged %>%
  mutate(
    firstname_first = word(firstname, 1),
    lastname_upper = str_to_upper(lastname),
    firstname_first_upper = str_to_upper(firstname_first),
    year = as.numeric(str_sub(date, 1, 4))
  )

# Step 6: Prepare mp_lookup
mp_lookup <- df_mp %>%
  mutate(
    firstname_first = word(firstname, 1),
    lastname_upper = str_to_upper(lastname),
    firstname_first_upper = str_to_upper(firstname_first)
  ) %>%
  select(lastname_upper, firstname_first_upper, pid, mp_start_year = year) %>%
  distinct()

# Step 7: Function to find best pid match
get_best_pid <- function(ln, fn, yr) {
  if (is.na(ln) | is.na(fn)) {
    return(NA_real_)
  }

  candidates <- mp_lookup %>%
    filter(lastname_upper == ln, firstname_first_upper == fn)

  if (nrow(candidates) == 0) {
    return(NA_real_)
  }

  # If only one candidate, return it regardless of year
  if (nrow(candidates) == 1) {
    return(candidates$pid)
  }

  # Multiple candidates: prefer one whose start year is <= meeting year
  valid <- candidates %>% filter(mp_start_year <= yr)

  if (nrow(valid) > 0) {
    return(valid %>% filter(mp_start_year == max(mp_start_year)) %>% pull(pid) %>% first())
  }

  # Fallback: return the earliest one
  candidates %>%
    filter(mp_start_year == min(mp_start_year)) %>%
    pull(pid) %>%
    first()
}

# Step 8: Apply pid matching (this may take a moment)
df_merged <- df_merged %>%
  rowwise() %>%
  mutate(pid = get_best_pid(lastname_upper, firstname_first_upper, year)) %>%
  ungroup()

# Step 9: Clean up helper columns
df_merged <- df_merged %>%
  select(-firstname_first, -lastname_upper, -firstname_first_upper, -year)

# Step 10: Final check
print(paste("Rows:", nrow(df_merged)))
n_with_pid <- sum(!is.na(df_merged$pid))
n_with_lastname <- sum(!is.na(df_merged$lastname))
print(paste(
  "Rows with pid:", n_with_pid, "/", n_with_lastname, "matched rows",
  "(", round(100 * n_with_pid / n_with_lastname, 1), "%)"
))


df_merged_final <- df_merged %>%
  select(matched, filename, date, index, speaker_verbatim, lastname, firstname, pid, organization, remark) |>
  arrange(date, filename, index)

write_csv(df_merged_final, "out/remarks.csv")

# Clean up typos in speaker names ----------------------------------------

df <- read_csv("out/remarks.csv")

df_wrong <- df |>
  filter(matched == 0)

write_csv(df, "out/remarks_edited.csv")

