# ----------------------------------------
# Expand attendance for early-year meetings
#
# Uses committee tenure intervals to add likely
# attendees for 1924-1953 meetings where transcripts
# list absences rather than full roll calls.
# ----------------------------------------

library(tidyverse)

# convert dates
df <- read_csv("temp/csv/tenures.csv") |> 
  mutate(
    startdate = as.Date(startdate),
    enddate = as.Date(enddate)
  )

# function to get members on given date
members_on_date <- function(data, date) {
  target_date <- as.Date(date)
  data |>
    filter(startdate <= target_date & enddate >= target_date) |>
    arrange(lastname) |>
    mutate(
      meeting_id = NA,
      speaker_verbatim = NA,
      attendee_verbatim = NA,
      title = NA,
      organization = "Stortinget",
      spoke = 0,
      person_id = NA,
      pid = NA
    ) |>
    select(
      meeting_id,
      speaker_verbatim,
      attendee_verbatim,
      title, 
      lastname, 
      firstname, 
      organization,
      spoke, 
      person_id,
      pid
    )
}

# Test
members_on_date(df, "1924-06-04")

# Expand pre-1954 meetings ----------------------------------------

# Load attendance data
df2 <- read_csv("temp/csv/attendance_full_edited_all_ids.csv")

# Load tenures data
tenures <- read_csv("temp/csv/tenures.csv") |>
  mutate(
    startdate = as.Date(startdate),
    enddate = as.Date(enddate)
  )

# Get unique pre-1950 meeting_ids
pre1954_meetings <- df2 %>%
  filter(str_sub(meeting_id, 1, 4) < "1954") %>%
  distinct(meeting_id) %>%
  pull(meeting_id)

# Function to extract date from meeting_id
meeting_to_date <- function(mid) {
  date_str <- str_sub(mid, 1, 8)
  as.Date(date_str, format = "%Y%m%d")
}

# Function to get missing members for one meeting
get_missing_members <- function(mid, df2, tenures) {
  meeting_date <- meeting_to_date(mid)

  existing_lastnames <- df2 %>%
    filter(meeting_id == mid) %>%
    pull(lastname) %>%
    unique()

  members <- tenures %>%
    filter(startdate <= meeting_date & enddate >= meeting_date)

  missing <- members %>%
    filter(!lastname %in% existing_lastnames) %>%
    mutate(
      meeting_id = as.numeric(mid),
      speaker_verbatim = NA_character_,
      attendee_verbatim = NA_character_,
      title = NA_character_,
      organization = "Stortinget",
      spoke = 0,
      person_id = NA_real_,
      pid = NA_real_
    ) %>%
    select(
      meeting_id,
      speaker_verbatim,
      attendee_verbatim,
      title,
      lastname,
      firstname,
      organization,
      spoke,
      person_id,
      pid
    )

  return(missing)
}

# Loop through all pre-1950 meetings and collect missing members
missing_rows <- map_dfr(pre1954_meetings, ~ get_missing_members(.x, df2, tenures))

# Check what we're adding
nrow(missing_rows)

# Add to df2
df2_expanded <- bind_rows(df2, missing_rows) %>%
  arrange(meeting_id, lastname)

# Verify
nrow(df2_expanded) # Should be 16319 + nrow(missing_rows)

write_csv(df2_expanded, "temp/csv/attendance_with_early_years.csv")
