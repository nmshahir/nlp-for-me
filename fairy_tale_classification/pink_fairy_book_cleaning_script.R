################################################################
# Name: pink_fairy_book_cleaning_script.R
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

pink_fairy_book <- gutenberg_download(5615)

# Cleaning The Pink Fairy Book
#Original version
# pink_chapter_titles <- pink_fairy_book |>
#   slice(55:95) |>
#   mutate(text = toupper(str_remove_all(text, "\\."))) |>
#   pull(text) |>
#   str_trim()

# pink_regex <- pink_chapter_titles |>
#   str_escape() |>
#   str_c(collapse = "|") |>
#   (\(x) str_c("^\\s*(", x, ")\\s*$"))()

# Asked positron to fix it
# pink_chapter_titles <- pink_fairy_book |>
#   slice(55:95) |>
#   mutate(text = toupper(str_trim(text))) |>
#   mutate(text = str_remove(text, "\\.$")) |> # strip trailing period (e.g. "The Cat's Elopement.")
#   pull(text) |>
#   str_replace("THE SNOW MAN", "THE SNOW.?MAN") # match "Snow Man" and "Snow-man"

# pink_regex <- pink_chapter_titles |>
#   str_escape() |>
#   str_c(collapse = "|") |>
#   str_c("^\\s*(", ... = _, ")\\s*\\.?$")

# pink_regex <- regex(pink_regex, ignore_case = TRUE)

#Try number 2
pink_chapter_titles <- pink_fairy_book |>
  slice(55:95) |>
  mutate(text = toupper(str_trim(text))) |>
  mutate(text = str_remove(text, "\\.$")) |>
  pull(text)

pink_regex <- pink_chapter_titles |>
  str_escape() |>
  str_replace("THE SNOW MAN", "THE SNOW.?MAN") |> # after escaping, so .? stays as regex
  str_c(collapse = "|") |>
  str_c("^\\s*(", ... = _, ")\\s*\\.?$")

pink_regex <- regex(pink_regex, ignore_case = TRUE)

#this is different because the periods are inconsistent for this book
pink_fairy_book_stories <- pink_fairy_book |>
  mutate(text = str_remove_all(text, "\\.")) |>
  gutenberg_add_sections(
    pattern = pink_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

pink_fairy_stories_only <- pink_fairy_book_stories |>
  slice(100:10615)

pink_fairy_stories_only |>
  count(story) |>
  print(n = 80)
