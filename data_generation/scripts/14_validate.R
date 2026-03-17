# ----------------------------------------
# Build human-validation sample files
#
# Draws a reproducible random sample of meetings,
# exports joined remark/person/document rows for
# annotation, and copies the corresponding PDFs.
# ----------------------------------------

library(tidyverse)
library(fs)

# Load source data ----------------------------------------
core_csv_dir <- Sys.getenv("EFAC_CORE_CSV_DIR", "../article/csv")
df_remark <- read_csv(file.path(core_csv_dir, "remarks.csv"))
df_person <- read_csv(file.path(core_csv_dir, "persons.csv"))
df_docs <- read_csv(file.path(core_csv_dir, "documents.csv"))

# Sample meetings ----------------------------------------
all_meeting_ids <- unique(df_remark$meeting_id)

set.seed(123)
sampled_meeting_ids <- sample(all_meeting_ids, 10)

df_annot <- df_remark |>
  filter(meeting_id %in% sampled_meeting_ids)

# Build annotation table ----------------------------------------
df <- df_annot |>
  left_join(df_person |> select(person_id, lastname, firstname), by = "person_id") |>
  left_join(df_docs |> select(meeting_id, filename), by = "meeting_id") |>
  select(
    meeting_id,
    filename,
    index,
    lastname,
    firstname,
    filename,
    remark
  )

# Write validation CSVs ----------------------------------------
dir_create("validate")
write_csv(df, "validate/validation_human.csv")
write_csv(df, "validate/validation_original.csv")

# Copy source PDFs for annotators ----------------------------------------
pdf_basenames <- unique(df$filename)
pdf_sourcedir <- Sys.getenv("EFAC_PDF_SOURCE_DIR", "data/pdf")
pdf_sourcepaths <- file.path(pdf_sourcedir, pdf_basenames)
pdf_destdir <- Sys.getenv("EFAC_VALIDATE_DEST_DIR", "validate")
dir_create(pdf_destdir)
pdf_destpaths <- file.path(pdf_destdir, pdf_basenames)
file_copy(pdf_sourcepaths, pdf_destpaths)
