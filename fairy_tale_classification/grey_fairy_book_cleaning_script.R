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
# Last Update: 2026.06.12
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
  str_trim() # trimming excess characters

grey_regex <- grey_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

grey_fairy_book_stories <- grey_fairy_book |>
  gutenberg_add_sections(
    pattern = grey_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

grey_fairy_stories_only <- grey_fairy_book_stories |>
  slice(70:10717)
