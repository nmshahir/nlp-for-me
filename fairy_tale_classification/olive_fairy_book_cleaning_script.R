################################################################
# Name: olive_fairy_book_cleaning_script.R
# Purpose: Pulling Lang's Fairy Books from Project Gutenberg and
#          and labeling them by story (i.e. chapter) within them
#          Removes extraneous information such as Prefaces.
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: Project Gutenberg
# Data Obtained: 2026.06.14
# Date Created: 2026.06.14
# Last Update: 2026.06.14
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

olive_fairy_book <- gutenberg_download(27826)

# Cleaning The Olive Fairy Book
olive_chapter_titles <- olive_fairy_book |>
  slice(212:268) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |> # trimming excess characters
  str_remove("\\s+\\d+$") |> # remove trailing page numbers
  str_trim() |> # trim again after removals
  str_subset("^.+$") # drop empty strings

olive_regex <- olive_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

olive_fairy_book_stories <- olive_fairy_book |>
  #change _THE FIVE WISE WORDS OF THE GURU_[4] to _THE FIVE WISE WORDS OF THE GURU_
  mutate(
    text = str_replace(
      text,
      "_THE FIVE WISE WORDS OF THE GURU_\\[4\\]",
      "_THE FIVE WISE WORDS OF THE GURU_"
    )
  ) |> #Note: According to posit assistant an alternative approach is to use the "fixed" function to deal with the square brackets
  # example fixed("_THE FIVE WISE WORDS OF THE GURU_[4]")
  gutenberg_add_sections(
    pattern = olive_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

olive_fairy_stories_only <- olive_fairy_book_stories |>
  slice(393:10316)

olive_fairy_stories_only |>
  count(story) |>
  print(n = 80)
