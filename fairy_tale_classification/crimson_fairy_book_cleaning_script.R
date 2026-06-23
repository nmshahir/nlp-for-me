################################################################
# Name: crimson_fairy_book_cleaning_script.R
# Purpose: Pulling Lang's Fairy Books from Project Gutenberg and
#          and labeling them by story (i.e. chapter) within them
#          Removes extraneous information such as Prefaces.
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: Project Gutenberg
# Data Obtained: 2026.06.12
# Date Created: 2026.06.12
# Last Update: 2026.06.23
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

crimson_fairy_book <- gutenberg_download(2435)

# Cleaning The Crimson Fairy Book
crimson_chapter_titles <- crimson_fairy_book |>
  slice(71:106) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

crimson_regex <- crimson_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

crimson_fairy_book_stories <- crimson_fairy_book |>
  gutenberg_add_sections(
    pattern = crimson_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

crimson_fairy_stories_only <- crimson_fairy_book_stories |>
  slice(111:10061)

crimson_fairy_stories_only |>
  count(story) |>
  print(n = 80)

##WHOOO NO POSIT ASSISTANT NEEDED
saveRDS(crimson_fairy_stories_only, "cleaned_crimson_fairy_book.rds")
