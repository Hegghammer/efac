# ----------------------------------------
# Verify OCR/parsing quality with similarity diagnostics
#
# Compares reconstructed remark text to original
# OCR text by document and writes review metrics.
# ----------------------------------------

library(tidyverse)
library(fs)
library(textreuse)
library(tokenizers)

# Load source data ----------------------------------------
df_remark <- read_csv("out/remarks.csv")
df_doc <- read_csv("out/documents.csv")

# Compare source and reconstructed text ----------------------------------------

# check pdf situation
length(unique(df_remark$filename))
length(unique(df_doc$filename))
missing <- setdiff(df_doc$filename, df_remark$filename)
missing # pdfs containing nothing, hence correct

# get docs vector
docs <- df_remark |>
  arrange(date) |>
  select(filename) |>
  unique() |>
  pull(filename)

# destdir for compiled remarks
dest <- "temp/verify/remarks_compiled"
dir_create(dest)

# Prepare score dataframe ----------------------------------------
df_scores <- data.frame(
  doc = character(),
  sim = numeric(),
  bagsim = numeric(),
  prop_orig_in_remarks = numeric(),
  prop_remarks_in_orig = numeric()
)

# Build similarity diagnostics ----------------------------------------
for (i in seq_along(docs)) {
  message("Processing ", docs[i], " ..")

  # subset df
  df_remarks <- df_remark |>
    filter(filename == docs[i]) |>
    arrange(index)

  # compile text from remarks
  text_remarks <- character()
  for (j in 1:nrow(df_remarks)) {
    entry <- paste0(df_remarks$speaker_verbatim[j], ": ", df_remarks$remark[j])
    text_remarks <- paste(text_remarks, entry, "\n\n")
  }

  # store for later inspection
  txtstem <- str_replace(docs[i], "pdf", "txt")
  write(text_remarks, file.path(dest, txtstem))

  # get original from file
  text_orig <- read_file(file.path("data/txt", txtstem))

  # tokenize
  tokenize <- function(text) tokenize_words(text)[[1]]
  a <- tokenize(text_remarks)
  b <- tokenize(text_orig)

  # calculate
  sim <- round(jaccard_similarity(a, b), 3) # should be close to 1
  bagsim <- round(jaccard_bag_similarity(a, b), 3) # should be close to 0.5
  prop_orig_in_remarks <- round(ratio_of_matches(a, b), 3) # should be close to 1
  prop_remarks_in_orig <- round(ratio_of_matches(b, a), 3) # should be 1

  # add to df
  row <- data.frame(
    doc = docs[i],
    sim = sim,
    bagsim = bagsim,
    prop_orig_in_remarks = prop_orig_in_remarks,
    prop_remarks_in_orig = prop_remarks_in_orig
  )

  df_scores <- rbind(df_scores, row)
}

# Add context variables ----------------------------------------
df_present <- df_doc |>
  filter(!filename %in% missing)

df_scores2 <- df_scores |>
  mutate(wordcount = count_words(df_present$text)) |>
  relocate(wordcount, .after = doc) |>
  rename(filename = doc)

df_missing_bold <- read_csv("temp/csv/missing_bold.csv")

df_scores3 <- merge(df_scores2, df_missing_bold, by = "filename", all.x = TRUE)

write_csv(df_scores3, "temp/csv/df_scores.csv")

# inspect
# hist(df_scores$sim)
# min(df_scores$sim)
# max(df_scores$sim)

# hist(df_scores$bagsim)
# min(df_scores$bagsim)
# max(df_scores$bagsim)

# hist(df_scores$prop_orig_in_remarks)
# min(df_scores$prop_orig_in_remarks)
# max(df_scores$prop_orig_in_remarks)

# hist(df_scores$prop_remarks_in_orig)
# min(df_scores$prop_remarks_in_orig)
# max(df_scores$prop_remarks_in_orig)

# Create ranked file for manual review ----------------------------------------
df_review <- df_scores3 |>
  arrange(sim)

write_csv(df_review, "temp/csv/ranked_for_review.csv")
