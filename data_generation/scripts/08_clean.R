# ----------------------------------------
# Clean and normalize EFAC remarks text
#
# Applies regex-based cleanup for OCR artifacts,
# normalizes speaker strings, and rewrites row
# indices per file before export.
# ----------------------------------------

# Load packages ----------------------------------------
library(tidyverse)

# Define helper functions ----------------------------------------

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

# Load data ----------------------------------------
df <- read_csv("out/remarks.csv")


# Clean remarks ----------------------------------------
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

# Write to file ----------------------------------------
write_csv(df, "temp/csv/missing_19921201.csv")

#write_csv(df, "out/remarks.csv")
# NB: After this, footnotes -- identified with "[^0] [^0]" -- were manually removed. 

# Appendix: inspection snippets used during development ----------------------------------------

# Function for inspecting
# get_context <- function(text, term, before = 5, after = 10) {
#   matches <- gregexpr(term, text)[[1]]
#   contexts <- sapply(matches, function(match) {
#     start_pos <- pmax(1, match - before)
#     end_pos <- pmin(nchar(text), match + nchar(term) + after - 1)
#     substr(text, start_pos, end_pos)
#   })
#   return(contexts)
# }

# View original ----------------------------------------

# term <- "SEARCH TERM"

# How many instances
# length(grep(term, df$remark, value = TRUE))

# Show single exaple
# grep(term, df$remark, value = TRUE)[1]

# View expressions in context
# for (i in 1:10) {
#   con <- get_context(grep(term, df$remark, value = TRUE)[i], term)
#   lst <- paste(con, "\n")
#   cat(lst)
# }

# Check after cleaning ----------------------------------------

# length(grep(term, df$remark_clean, value = TRUE))
# grep(term, df$remark_clean, value = TRUE)[1]

# for (i in 1:1) {
#   con <- get_context(grep(term, df$remark_clean, value = TRUE)[i], term)
#   lst <- paste(con, "\n")
#   cat(lst)
# }
