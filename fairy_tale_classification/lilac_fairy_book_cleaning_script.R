################################################################
# Name: lilac_fairy_book_cleaning_script.R
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

lilac_fairy_book <- gutenberg_download(3454)

# Cleaning The Lilac Fairy Book
lilac_chapter_titles <- lilac_fairy_book |>
  slice(138:170) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |> # trimming excess characters
  # "THE BELIEVING. HUSBANDS" in the table of contents is actually "THE BELIEVING HUSBANDS" in the body so lets fix that
  str_replace("THE BELIEVING. HUSBANDS", "THE BELIEVING HUSBANDS") |>
  # "THE HOODIE-CROW" is listed as "THE HOODIE-CROW." in the body
  str_replace("THE HOODIE-CROW", "THE HOODIE-CROW.") |>
  # THE RING OF THE WATERFALLS is actually "THE KING OF THE WATERFALLS"
  str_replace("THE RING OF THE WATERFALLS", "THE KING OF THE WATERFALLS") |>
  str_replace("THE WONDERFUL TUNE", "THE WONDERFUL TUNE.") |>
  # Yet Another Ring in the table of contents vs King in the body
  str_replace("THE SEA RING'S GIFT", "THE SEA KING'S GIFT") |>
  str_replace("THE LADY OF THE FOUNTAIN", "THE LADY OF THE FOUNTAIN.")

lilac_regex <- lilac_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

lilac_fairy_book_stories <- lilac_fairy_book |>
  gutenberg_add_sections(
    pattern = lilac_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

lilac_fairy_stories_only <- lilac_fairy_book_stories |>
  slice(175:10626)

lilac_fairy_stories_only |>
  count(story) |>
  print(n = 80)
