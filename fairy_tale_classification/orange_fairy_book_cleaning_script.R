################################################################
# Name: orange_fairy_book_cleaning_script.R
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

orange_fairy_book <- gutenberg_download(36532)

# Cleaning The Orange Fairy Book
orange_chapter_titles <- orange_fairy_book |>
  slice(212:276) |>
  mutate(text = toupper(text)) |>
  pull(text) |>
  str_trim() |>
  str_remove("\\s+\\d+$") |> # remove trailing page numbers
  str_trim() |> # trim again after removals
  str_subset("^.+$") # drop empty strings

orange_regex <- orange_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

orange_fairy_book_stories <- orange_fairy_book |>
  gutenberg_add_sections(
    pattern = orange_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

orange_fairy_stories_only <- orange_fairy_book_stories |>
  slice(426:11344) |>
  mutate(story = str_replace_all(story, "_", "")) |> #Removes the underscores from story label
  filter(!str_detect(text, "\\[Illustration:")) #removes the Illustration lines because evidently this version came with pictures??

orange_fairy_stories_only |>
  count(story) |>
  print(n = 80)

saveRDS(orange_fairy_stories_only, "cleaned_orange_fairy_book.rds")
