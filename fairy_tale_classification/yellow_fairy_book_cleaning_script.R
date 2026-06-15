################################################################
# Name: yellow_fairy_book_cleaning_script.R
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

yellow_fairy_book <- gutenberg_download(640)

# Cleaning The Yellow Fairy Book
yellow_chapter_titles <- yellow_fairy_book |>
  slice(126:173) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

yellow_regex <- yellow_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

# yellow_fairy_book_stories <- yellow_fairy_book |>
#   mutate(
#     text = str_replace(
#       text,
#       "THE DRAGON OF THE NORTH\\(2\\)",
#       "THE DRAGON OF THE NORTH"
#     )
#   ) |>
#   mutate(
#     text = str_replace(
#       text,
#       "STORY OF THE EMPEROR'S NEW CLOTHES\\(4\\)",
#       "STORY OF THE EMPEROR'S NEW CLOTHES"
#     )
#   ) |>
#   mutate(
#     text = str_replace(text, "THE GOLDEN CRAB\\(5\\)", "THE GOLDEN CRAB")
#   ) |>
#   mutate(text = str_replace(text, "THE IRON STOVE\\(7\\)", "THE IRON STOVE")) |>
#   mutate(
#     text = str_replace(
#       text,
#       "THE LITTLE GREEN FROG\\(8\\)",
#       "THE LITTLE GREEN FROG"
#     )
#   ) |>
#   gutenberg_add_sections(
#     pattern = yellow_regex,
#     section_col = "story",
#     ignore_case = TRUE,
#     format_fn = str_trim
#   )

#The above code uh is very very tedious but I know the issue is the footnote numbers attached to some of the titles! So threw it into posit assistant
# to determine a more efficient way of handling the issue as seen below
yellow_fairy_book_stories <- yellow_fairy_book |>
  mutate(
    text = str_remove(text, "\\s*\\(\\d+\\)$"), # strip all trailing footnote numbers
    text = str_replace(text, "BLOCKHEAD-HANS", "BLOCKHEAD HANS") # match ToC spelling
  ) |>
  gutenberg_add_sections(
    pattern = yellow_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )


yellow_fairy_stories_only <- yellow_fairy_book_stories |>
  slice(184:11312)

yellow_fairy_stories_only |>
  count(story) |>
  print(n = 80)
