################################################################
# Name: blue_fairy_book_cleaning_script.R
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

# We want to get the Blue Fairy Book
gutenberg_works(title == "The Blue Fairy Book")

blue_fairy_book <- gutenberg_download(503)

# Cleaning The Blue Fairy Book
blue_chapter_titles <- blue_fairy_book |>
  slice(12:48) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

# This does not work properly because the punctuation in the TOC does not align with the headers in the body of text!!!
blue_regex <- blue_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

blue_fairy_book_stories <- blue_fairy_book |>
  gutenberg_add_sections(
    pattern = blue_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )


# So we kick it to Claude (Sonnet, I am cheap)
blue_chapter_titles_fixed <- blue_fairy_book |>
  slice(12:48) |>
  mutate(text = toupper(text)) |>
  pull(text) |>
  str_trim() |>
  str_replace("LITTLE RED RIDING-HOOD", "LITTLE RED RIDING HOOD") |>
  str_replace(
    "CINDERELLA; OR, THE LITTLE GLASS SLIPPER",
    "CINDERELLA, OR THE LITTLE GLASS SLIPPER"
  )

blue_regex_fixed <- blue_chapter_titles_fixed |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

blue_fairy_book_stories_fixed <- blue_fairy_book |>
  gutenberg_add_sections(
    pattern = blue_regex_fixed,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )


blue_fairy_stories_only <- blue_fairy_book_stories_fixed |>
  slice(53:13532)

#Sanity check by counting the number of lines associated with each story
blue_fairy_stories_only |>
  count(story) |>
  print(n = 80)

saveRDS(blue_fairy_stories_only, "cleaned_blue_fairy_book.rds")
