################################################################
# Name: fairy_tale_story_labeling.R
# Purpose: Pulling Lang's Fairy Books from Project Gutenberg and
#          and labeling them by story (i.e. chapter) within them
#          Removes extraneous information such as Prefaces.
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: Project Gutenberg
# Data Obtained: 2026.03.10
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

# I told Posit!Claude to create a regex for me; this is different from the regex they made previously but I forgot that one!
# so alas here we are
chapter_regex <- chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

# and Voila we have titled things mostly
green_fairy_book_stories <- green_fairy_book |>
  gutenberg_add_sections(
    pattern = chapter_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )


# I don't care about the preface so lets just remove that (and the table of contents)
green_fairy_stories_only <- green_fairy_book_stories |>
  slice(147:12130)

# Okay lets get a few more fairy tales
gutenberg_works(title == "The Blue Fairy Book")
gutenberg_works(title == "The Red Fairy Book")
gutenberg_works(title == "The Pink Fairy Book")
gutenberg_works(title == "The Grey Fairy Book")
gutenberg_works(title == "The Violet Fairy Book")
gutenberg_works(title == "The Crimson Fairy Book")
gutenberg_works(title == "The Brown Fairy Book")
gutenberg_works(title == "The Orange Fairy Book")
gutenberg_works(title == "The Olive Fairy Book")
gutenberg_works(title == "The Lilac Fairy Book")

blue_fairy_book <- gutenberg_download(503)
red_fairy_book <- gutenberg_download(540)
pink_fairy_book <- gutenberg_download(5615)
grey_fairy_book <- gutenberg_download(6746)
violet_fairy_book <- gutenberg_download(641)
crimson_fairy_book <- gutenberg_download(2435)
brown_fairy_book <- gutenberg_download(3282)
#orange_fairy_book <- gutenberg_download(3027) The formatting is wonky in this one
orange_fairy_book <- gutenberg_download(36532)
olive_fairy_book <- gutenberg_download(27826)
lilac_fairy_book <- gutenberg_download(3454)

# Cleaning The Blue Fairy Book
blue_chapter_titles <- blue_fairy_book |>
  slice(12:48) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

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

blue_fairy_stories_only <- blue_fairy_book_stories |>
  slice(53:13532)

# Cleaning The Red Fairy Book
red_chapter_titles <- red_fairy_book |>
  slice(18:54) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

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

# Cleaning The Pink Fairy Book
pink_chapter_titles <- pink_fairy_book |>
  slice(55:95) |>
  mutate(text = toupper(str_remove_all(text, "\\."))) |>
  pull(text) |>
  str_trim()

pink_regex <- pink_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

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

# Cleaning The Grey Fairy Book
grey_chapter_titles <- grey_fairy_book |>
  slice(32:64) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

grey_regex <- grey_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

grey_fairy_book_stories <- grey_fairy_book |>
  gutenberg_add_sections(
    pattern = grey_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

grey_fairy_stories_only <- grey_fairy_book_stories |>
  slice(70:10717)

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

# Cleaning The Brown Fairy Book
brown_chapter_titles <- brown_fairy_book |>
  slice(77:109) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |> # trimming excess characters
  str_subset(pattern = "^FOOTNOTES$", negate = TRUE)

brown_regex <- brown_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

brown_fairy_book_stories <- brown_fairy_book |>
  mutate(text = str_remove(text, "\\[\\d+\\]")) |>
  gutenberg_add_sections(
    pattern = brown_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

brown_fairy_stories_only <- brown_fairy_book_stories |>
  slice(119:10051)

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
  slice(426:11344)

# Cleaning The Olive Fairy Book
olive_chapter_titles <- olive_fairy_book |>
  slice(212:268) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() |> # trimming excess characters
  str_remove("\\s+\\d+$") |> # remove trailing page numbers
  str_trim() |> # trim again after removals
  str_subset("^.+$") # drop empty strings

olive_regex <- olive_chapter_titles |>
  str_escape() |>
  str_c(collapse = "|") |>
  (\(x) str_c("^\\s*(", x, ")\\s*$"))()

olive_fairy_book_stories <- olive_fairy_book |>
  gutenberg_add_sections(
    pattern = olive_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )

olive_fairy_stories_only <- olive_fairy_book_stories |>
  slice(393:10316)

# Cleaning The Lilac Fairy Book
lilac_chapter_titles <- lilac_fairy_book |>
  slice(138:170) |> # getting the range of titles
  mutate(text = toupper(text)) |> # I convert them to uppercase
  pull(text) |> # tell R I just want the text column
  str_trim() # trimming excess characters

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

#Save all of the fairy tales into an Rdata file

saveRDS(blue_fairy_stories_only, "cleaned_blue_fairy_book.rds")
saveRDS(red_fairy_stories_only, "cleaned_red_fairy_book.rds")
saveRDS(green_fairy_stories_only, "cleaned_green_fairy_book.rds")
saveRDS(pink_fairy_stories_only, "cleaned_pink_fairy_book.rds")
saveRDS(grey_fairy_stories_only, "cleaned_grey_fairy_book.rds")
saveRDS(violet_fairy_stories_only, "cleaned_violet_fairy_book.rds")
saveRDS(crimson_fairy_stories_only, "cleaned_crimson_fairy_book.rds")
saveRDS(brown_fairy_stories_only, "cleaned_brown_fairy_book.rds")
saveRDS(orange_fairy_stories_only, "cleaned_orange_fairy_book.rds")
saveRDS(olive_fairy_stories_only, "cleaned_olive_fairy_book.rds")
saveRDS(lilac_fairy_stories_only, "cleaned_lilac_fairy_book.rds")
