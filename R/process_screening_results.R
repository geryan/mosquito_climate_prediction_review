# Read and tidy the abstract screening results, and summarise them for plotting.
#
# Creates:
#   screening      - one row per study retained after abstract screening
#   journal_counts - number of studies per journal, for journals with more
#                    than one study, plus "Other"

library(readxl)
library(dplyr)
library(stringr)

# inputs ------------------------------------------------------------------

# Covidence export of abstract screening results
screening_file <- "data/review_823741_select_csv_20261012173842.csv.xlsx"

# journals with fewer studies than this are pooled into "Other"
min_studies <- 2

# read and tidy -----------------------------------------------------------

# Journal names are exported in upper case; convert to title case for plotting,
# keeping acronyms upper case and minor words lower case
tidy_journal_name <- function(x) {
  x |>
    str_to_title() |>
    str_replace_all("\\b(Plos|Bmc)\\b", toupper) |>
    str_replace_all("(?<=\\s)(And|Of|The|In|De)\\b", tolower)
}

# read everything as text: Excel has mangled some unused columns (e.g. an
# issue number stored as a date), which otherwise throws parsing warnings
screening <- read_excel(screening_file, col_types = "text") |>
  transmute(
    study = Study,
    year = as.integer(`Published Year`),
    journal = tidy_journal_name(Journal)
  )

# studies by journal ------------------------------------------------------

# Journals shown individually, in descending order of count, with ties in
# alphabetical order
top_journals <- screening |>
  count(journal) |>
  filter(n >= min_studies) |>
  arrange(desc(n), str_to_lower(journal)) |>
  pull(journal)

journal_counts <- screening |>
  mutate(
    journal = if_else(journal %in% top_journals, journal, "Other"),
    # order by descending count, with "Other" last
    journal = factor(journal, levels = c(top_journals, "Other"))
  ) |>
  count(journal)
