################################################################
# Name: green_fairy_book_cleaning_script.R
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

# We want to get the Green Fairy Book
gutenberg_works(title == "The Green Fairy Book")

green_fairy_book <- gutenberg_download(7277)

# Problem number 1 - I want the chapters to respond to the Story title! But they aren't formatted like "chapter/story:" etc

#I know that this isn't exact enough and regex are a nightmare
green_fairy_book2 <- green_fairy_book |>
  gutenberg_add_sections(
    pattern = "^[A-Z]+(?:\\s[A-Z]+)*$",
    section_col = "chapter"
  )


#I asked positron assistant! I then opened a repo and it got rid of my previous chat and I do not remember my original prompt,
#I am reminded how much I hate LLMs
# Let try take two though

#I roughly remember that it involved getting the chapter titles into it's own dataset
chapter_titles <- green_fairy_book |>
  slice(99:140) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

#I asked Posit Assistant to fix the regex issues

chapter_regex_fixed <- chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")(\\s*\\(.*\\))?\\s*$"))()

green_fairy_book_stories_fixed <- green_fairy_book |>
  gutenberg_add_sections(
    pattern = chapter_regex_fixed,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

green_fairy_book_stories_fixed |>
  count(story) |>
  print(n = 90)

# I don't care about the preface so lets just remove that (and the table of contents)
#Bonus from claude: One remaining cosmetic issue: the section label itself now includes the parenthetical (e.g., KING KOJATA (From the Russian)). If you want clean labels, you can strip it after the fact:
green_fairy_stories_only <- green_fairy_book_stories_fixed |>
  slice(147:12130) |>
  mutate(story = str_remove(story, "\\s*\\(.*\\)$"))
