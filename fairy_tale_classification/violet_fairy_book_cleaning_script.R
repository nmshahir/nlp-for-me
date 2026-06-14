################################################################
# Name: violet_fairy_book_cleaning_script.R
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


violet_fairy_book <- gutenberg_download(641)

# Cleaning The Violet Fairy Book
violet_chapter_titles <- violet_fairy_book |>
  slice(76:110) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |>
  str_replace(
    regex("VIRGILILUS THE SORCERER", ignore_case = TRUE),
    "VIRGILIUS THE SORCERER" # replace with whatever the book text actually uses
  )

violet_regex <- violet_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

violet_fairy_book_stories <- violet_fairy_book |>
  gutenberg_add_sections(
    pattern = violet_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

violet_fairy_stories_only <- violet_fairy_book_stories |>
  slice(115:11102)

violet_fairy_stories_only |>
  count(story) |>
  print(n = 80)

# Checking if there's differences between chapter titles vs actual labels
setdiff(violet_chapter_titles, violet_fairy_stories_only$story)
#two stories are missing "THE THREE PRINCES AND THEIR BEASTS" and "HOW A FISH SWAM IN THE AIR AND A HARE IN THE WATER"

#Posit Assistant fixes
violet_chapter_titles <- violet_fairy_book |>
  slice(76:110) |>
  mutate(text = toupper(text)) |>
  pull(text) |>
  str_trim() |>
  str_replace(
    regex("VIRGILILUS THE SORCERER", ignore_case = TRUE),
    "VIRGILIUS THE SORCERER"
  ) |>
  # Fix titles to match actual book headings
  str_replace(
    "THE THREE PRINCES AND THEIR BEASTS",
    "THE THREE PRINCES AND THEIR BEASTS (LITHUANIAN FAIRY TALE)"
  ) |>
  str_replace(
    "HOW A FISH SWAM IN THE AIR AND A HARE IN THE WATER",
    "HOW A FISH SWAM IN THE AIR AND A HARE IN THE WATER."
  )

violet_regex <- violet_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

violet_fairy_book_stories <- violet_fairy_book |>
  gutenberg_add_sections(
    pattern = violet_regex,
    section_col = "story",
    ignore_case = TRUE,
    # Normalize labels: trim whitespace, strip trailing punctuation, remove subtitles in parens
    format_fn = function(x) {
      x |>
        str_trim() |>
        str_remove("[.!?]$") |>
        str_remove("\\s*\\([^)]*\\)$")
    }
  )

violet_fairy_stories_only <- violet_fairy_book_stories |>
  slice(115:11102)

violet_fairy_stories_only |>
  count(story) |>
  print(n = 80)


setdiff(
  c(
    "THE THREE PRINCES AND THEIR BEASTS",
    "HOW A FISH SWAM IN THE AIR AND A HARE IN THE WATER"
  ),
  violet_fairy_book_stories$story
)
