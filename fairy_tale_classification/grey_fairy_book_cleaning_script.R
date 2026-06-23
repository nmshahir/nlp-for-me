################################################################
# Name: grey_fairy_book_cleaning_script.R
# Purpose: Pulling Lang's Fairy Books from Project Gutenberg and
#          and labeling them by story (i.e. chapter) within them
#          Removes extraneous information such as Prefaces.
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: Project Gutenberg
# Data Obtained: 2026.06.12
# Date Created: 2026.06.12
# Last Update: 2026.06.13
#################################################################

install.packages("tidytext")
# install.packages("pak")
pak::pak("ropensci/gutenbergr")

library(tidytext)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gutenbergr)
library(RColorBrewer)

gutenberg_get_mirror()


grey_fairy_book <- gutenberg_download(6746)

# Cleaning The Grey Fairy Book
grey_chapter_titles <- grey_fairy_book |>
  slice(32:64) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |> # trimming excess characters
  # Fix spelling: TOC says "Dachemila" but body heading says "Dschemila"
  str_replace("DACHEMILA", "DSCHEMILA") |>
  # Fix missing hyphen: TOC says "Unlooked For" but body heading says "Unlooked-for"
  str_replace("UNLOOKED FOR PRINCE", "UNLOOKED-FOR PRINCE") |>
  # Split two combined TOC rows into individual story titles
  str_replace(
    "THE JACKAL AND THE SPRING THE BEAR",
    "THE JACKAL AND THE SPRING|THE BEAR"
  ) |>
  str_replace(
    "THE SUNCHILD THE DAUGHTER OF BUK ETTEMSUCH",
    "THE SUNCHILD|THE DAUGHTER OF BUK ETTEMSUCH"
  ) |>
  str_split("\\|") |>
  unlist()

# Allow optional trailing period to match body headings like
# "The Partnership of the Thief and the Liar."
grey_regex <- grey_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\.?\\s*$"))()

grey_fairy_book_stories <- grey_fairy_book |>
  gutenberg_add_sections(
    pattern = grey_regex,
    section_col = "story",
    ignore_case = TRUE,
    # Strip whitespace and any trailing period so labels are clean
    format_fn = \(x) str_remove(str_trim(x), "\\.$")
  )

grey_fairy_stories_only <- grey_fairy_book_stories |>
  slice(70:10717)

grey_fairy_stories_only |>
  count(story) |>
  print(n = 80)

saveRDS(grey_fairy_stories_only, "cleaned_grey_fairy_book.rds")
