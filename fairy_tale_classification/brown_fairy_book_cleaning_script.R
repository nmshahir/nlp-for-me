################################################################
# Name: brown_fairy_book_cleaning_script.R
# Purpose: Pulling Lang's Fairy Books from Project Gutenberg and
#          and labeling them by story (i.e. chapter) within them
#          Removes extraneous information such as Prefaces.
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: Project Gutenberg
# Data Obtained: 2026.06.14
# Date Created: 2026.06.14
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

brown_fairy_book <- gutenberg_download(3282)

# Cleaning The Brown Fairy Book
brown_chapter_titles <- brown_fairy_book |>
    slice(77:109) |> # getting the range of titles
    mutate(text = toupper(text)) |> # I convert them to uppercase
    pull(text) |> # tell R I just want the text column
    str_trim() |> # trimming excess characters
    str_subset(pattern = "^FOOTNOTES$", negate = TRUE) # one of the books has footnotes and thus not relevant for this project and thus being removed

brown_regex <- brown_chapter_titles |>
    str_escape() |>
    str_c(collapse = "|") |>
    (\(x) str_c("^\\s*(", x, ")\\s*$"))()

brown_fairy_book_stories <- brown_fairy_book |>
    slice(-c(1308:1350)) |>
    mutate(text = str_remove(text, "\\[\\d+\\]")) |>
    gutenberg_add_sections(
        pattern = brown_regex,
        section_col = "story",
        ignore_case = TRUE,
        format_fn = str_trim
    )

brown_fairy_stories_only <- brown_fairy_book_stories |>
    slice(119:10051)

brown_fairy_stories_only |>
    count(story) |>
    print(n = 80)

# NO POSIT ASSISTANT NEEDED

saveRDS(brown_fairy_stories_only, "cleaned_brown_fairy_book.rds")
