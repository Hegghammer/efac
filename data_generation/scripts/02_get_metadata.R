# ----------------------------------------
# Build EFAC document metadata table
#
# Extracts date, time, title, URL, and full OCR text
# for each meeting document and writes out/documents.csv.
# ----------------------------------------

library(tidyverse)
library(rvest)
library(xml2)

landing_pages <- c(
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1924-1945/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1946-1965/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/Utvidet-utenriks-1966-1970/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/Utvidet-utenriks-1971-1979/", # sic
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1976-1981/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1981-1985/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomiteen-1986-1995/"
)

# 1924-1945 ----------------------------------------
page <- read_html(landing_pages[1])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 42

# get dates
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

# 1946-1965 ----------------------------------------
page <- read_html(landing_pages[2])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 323 sic

# get dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- link_texts[3:length(link_texts)]
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
date[31] <- "1949-10-11"
df_dates <- data.frame(raw = dates_raw, parsed = date) # check

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}$")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

# descriptions
title <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]") |>
  map_chr(~ {
    xml_remove(xml_find_first(.x, "./a[1]"))
    html_text2(.x)
  }) |>
  str_squish()

df2 <- data.frame(date, time, title)

# 1966-1970 ----------------------------------------
page <- read_html(landing_pages[3])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 35

# get dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- link_texts[2:length(link_texts)]
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

# get descriptions
p_nodes <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]")
 
title <- p_nodes |> map_chr(function(p) {
  full_p_text <- xml_text(p, trim = TRUE)
  date_text <- xml_text(xml_find_first(p, "./a[1]"))
  p_text <- full_p_text |>
    str_remove(fixed(date_text)) |>
    str_squish()
  ol_node <- xml_find_first(p, "following-sibling::*[1][self::ol]")
  if (!is.na(ol_node)) {
    items <- ol_node |>
      xml_find_all("./li") |>
      xml_text(trim = TRUE)
    items_combined <- paste(items, collapse = "; ")
    if (str_detect(p_text, "[:–—-]\\s*$")) {
      combined <- paste0(p_text, " ", items_combined)
    } else if (p_text == "") {
      combined <- items_combined
    } else {
      combined <- paste0(p_text, " — ", items_combined)
    }
  } else {
    combined <- p_text
  }
  str_squish(combined)
})

df3 <- data.frame(date, time, title)

# 1971-1975 ----------------------------------------
page <- read_html(landing_pages[4])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 81

# get dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- link_texts[2:length(link_texts)]
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
date[25] <- "1972-03-09"
date[67] <- "1974-11-22"
df_dates <- data.frame(raw = dates_raw, parsed = date) # check

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

# descriptions
title <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]") |>
  map_chr(~ {
    xml_remove(xml_find_first(.x, "./a[1]"))
    html_text2(.x)
  }) |>
  str_squish()

title[54] <- str_replace(title[54], "2. november 1973 \\(andre del\\)", "")
title[67] <- str_replace(title[67], "5. desember 1974 \\(andre del\\)", "")

part1 <- title[1:53]
part2 <- title[54:66]
part3 <- title[67:length(title)]
title <- c(part1, part2[1], part2, part3[1], part3)

# title[54] <- paste(title[54], "(andre del)")
# title[68] <- paste(title[68], "(andre del)")
title[54] <- paste(title[54], "(første del)")
title[55] <- paste(title[55], "(andre del)")
title[68] <- paste(title[68], "(første del)")
title[69] <- paste(title[69], "(andre del)")

df4 <- data.frame(date, time, title)

# 1976-1980 ----------------------------------------
page <- read_html(landing_pages[5])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 46

# get dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- link_texts[2:length(link_texts)]
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
df_dates <- data.frame(raw = dates_raw, parsed = date) # check

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

# descriptions
title <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]") |>
  map_chr(~ {
    xml_remove(xml_find_first(.x, "./a[1]"))
    html_text2(.x)
  }) |>
  str_squish()

title[37] <- str_replace(title[37], "3. oktober 1979 \\(andre del\\)", "")

part1 <- title[1:36]
part2 <- title[37:length(title)]
title <- c(part1, part2[1], part2)
title[37] <- paste(title[37], "(første del)")
title[38] <- paste(title[38], "(andre del)")

df5 <- data.frame(date, time, title)

# 1981-1985 ----------------------------------------
page <- read_html(landing_pages[6])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
length(pdf_links) # 31

# get dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text()

datetimes_raw <- link_texts[2:length(link_texts)]
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
df_dates <- data.frame(raw = dates_raw, parsed = date) # check
date[15] <- "1982-11-19"
date[16] <- "1982-11-19"

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

# descriptions
title <- page |>
  html_nodes(xpath = "//p[node()[1][self::a]]") |>
  map_chr(~ {
    xml_remove(xml_find_first(.x, "./a[1]"))
    html_text2(.x)
  }) |>
  str_squish()

title[15] <- "Redegjørelse av utenriksminister Svenn Stray om 1. Henvendelser vedrørende bevilgninger til NATOs infrastrukturprogram. 2. Undertegnelse av Havrettstraktaten (første del)"
title[16] <- "Redegjørelse av utenriksminister Svenn Stray om 1. Henvendelser vedrørende bevilgninger til NATOs infrastrukturprogram. 2. Undertegnelse av Havrettstraktaten (andre del)"

df6 <- data.frame(date, time, title)

# 1986-1992 ----------------------------------------
page <- read_html(landing_pages[7])

# Check number of pdfs
links <- page |>
  html_nodes("a") |>
  html_attr("href")
pdf_links <- grep(".pdf$", links, value = TRUE)
pdf_links <- pdf_links[!grepl("innst", pdf_links)]
pdf_links <- pdf_links[!grepl("vedlegg", pdf_links)]
length(pdf_links) # 82

# dates
link_texts <- page |>
  html_nodes("p") |>
  html_nodes("a") |>
  html_text() |>
  str_trim()

datetimes_raw <- link_texts[2:length(link_texts)]
dates_raw <- str_extract(datetimes_raw, ".*\\d{4}")

old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "nb_NO.UTF-8")
date <- as.Date(dates_raw, format = "%d. %B %Y")
Sys.setlocale("LC_TIME", old_locale)
df_dates <- data.frame(raw = dates_raw, parsed = date)

# get times
times_raw <- str_extract(datetimes_raw, "\\d{1,2}[\\.:]\\d{2}")
all_colons <- gsub("\\.", ":", times_raw)
time <- format(strptime(all_colons, "%H:%M"), "%H:%M")

# descriptions
title <- page |>
  html_nodes("ol") |>
  html_text() |>
  str_replace("Eventuelt", "") |>
  str_replace("Vedlegg: Utkast til erklæring som skal sendes etter toppmøtet i EFTA\\.", "") |>
  str_replace("Vedlegg: Orientering om GATT Uruguay-runden for utenrikskomiteen fra handelsministeren", "") |>
  str_replace("\n\\.+\n", "") |>
  str_replace("\\.$", "") |>
  str_squish()

df7 <- data.frame(date, time, title)

# Compile metadata ----------------------------------------

df_files <- read_csv("temp/csv/df_files.csv")

# Collate
df_meta <- rbind(df1, df2, df3, df4, df5, df6, df7)


df <- cbind(df_files, df_meta) |>
  select(filename, date, time, title, url)

# Add meeting_id
df <- df |>
  mutate(meeting_id = paste0(str_replace_all(date, "\\-", ""), "0")) |>
  relocate(meeting_id, .after = filename)
# This variable will be manually edited later for two-meeting days

# Store
write_csv(df, "temp/csv/df_meta.csv")

# Add full document text ----------------------------------------

# get actual inventory
txt_files <- dir_ls("data/txt")

# get path vector from df (for right order)
txt_paths <- file.path("data/txt", str_replace(df$filename, "pdf", "txt"))

# check
setdiff(txt_files, txt_paths)

# add to df
df$text <- map_chr(txt_paths, read_file)

# Save final version
write_csv(df, "out/documents.csv")
