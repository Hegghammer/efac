# ----------------------------------------
# Download EFAC PDFs and source URL index
#
# Pulls PDF files from the Storting archive pages,
# removes known non-target files, and writes a
# filename-url lookup to temp/csv/df_files.csv.
# ----------------------------------------

library(tidyverse)
library(rvest)
library(qpdf)

# Get PDFs ----------------------------------------

# get landing pages
landing_pages <- c(
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1924-1945/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1946-1965/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/Utvidet-utenriks-1966-1970/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/Utvidet-utenriks-1971-1979/", # sic
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1976-1981/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomite-1981-1985/",
  "https://www.stortinget.no/no/Stortinget-og-demokratiet/Historikk/Lukkede-moter/den-utvidede-utenriks--og-konstitusjonskomiteen-1986-1995/"
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
  for (j in seq_along(pdf_urls)) {
    download.file(pdf_urls[j], file.path("data/pdf", basename(pdf_urls[j])), mode = "wb")
  }
}

# remove unwanted pdf
file.remove(c(
  "data/pdf/innst.-s.-nr.-124-1994-95.pdf",
  "data/pdf/innst.-s.-nr.-295-1995-96.pdf",
  "data/pdf/1989_0308-vedlegg-utkast-til-erklaring-som-skal-sendes-ut-etter-toppmotet-i-efta.pdf",
  "data/pdf/1992_0116-vedlegg-orientering-om-gatt-uruguay-runden-for-utenrikskomiteen-fra-handelsministeren.pdf"
))

# crop excess pages in 1990_0822-ss.-401-616_sladda_.pdf
pdf_subset("data/pdf/1990_0822-ss.-401-616_sladda_.pdf", pages = 1:49)

# create and store df
df_files <- urls |>
  data.frame() |>
  rename(url = urls) |>
  filter(!(grepl("innst.-s.-nr.-124-1994-95.pdf|innst.-s.-nr.-295-1995-96.pdf", url))) |>
  mutate(filename = basename(url)) |>
  filter(!(grepl("vedlegg", filename)))

write_csv(df_files, "temp/csv/df_files.csv")
