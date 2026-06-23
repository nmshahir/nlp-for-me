################################################################
# Name: red_fairy_book_cleaning_script.R
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

red_fairy_book <- gutenberg_download(540)

# Cleaning The Red Fairy Book
red_chapter_titles <- red_fairy_book |>
  slice(18:54) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

red_chapter_titles <- red_fairy_book |>
  slice(18:54) |>
  mutate(text = toupper(text)) |>
  pull(text) |>
  str_trim() |>
  str_replace("LITTLE GOLDENHOOD", "LITTLE GOLDEN HOOD") # fix TOC vs. body mismatch

red_regex <- red_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

red_fairy_book_stories <- red_fairy_book |>
  gutenberg_add_sections(
    pattern = red_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

red_fairy_stories_only <- red_fairy_book_stories |>
  slice(86:14327)

#Sanity check by counting the number of lines associated with each story
red_fairy_stories_only |>
  count(story) |>
  print(n = 80)

saveRDS(red_fairy_stories_only, "cleaned_red_fairy_book.rds")
