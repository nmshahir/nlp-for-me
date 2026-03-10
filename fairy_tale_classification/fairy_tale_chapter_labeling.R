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

grn_fairy_book <- gutenberg_download(7277)

# Problem number 1 - I want the chapters to respond to the Story title! But they aren't formatted like "chapter/story:" etc

#I know that this isn't exact enough and regex are a nightmare
grn_fairy_book2 <- grn_fairy_book |>
  gutenberg_add_sections(
    pattern = "^[A-Z]+(?:\\s[A-Z]+)*$",
    section_col = "chapter"
  )

#I asked positron assistant! I then opened a repo and it got rid of my previous chat and I do not remember my original prompt,
#I am reminded how much I hate LLMs
# Let try take two though

#I roughly remember that it involved getting the chapter titles into it's own dataset
chapter_titles <- grn_fairy_book |>
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
grn_fairy_book_stories <- grn_fairy_book |>
  gutenberg_add_sections(
    pattern = chapter_regex,
    section_col = "story",
    ignore_case = TRUE,
    format_fn = str_trim
  )


# I don't carea about the preface so lets just remove that (and the table of contents)
grn_fairy_stories_only <- grn_fairy_book_stories |>
  slice(147:12130)

# Okay lets get a few more fairy tales
gutenberg_works(title == "The Blue Fairy Book")
gutenberg_works(title == "The Red Fairy Book")

blue_fairy_book <- gutenberg_download(503)
red_fairy_book <- gutenberg_download(540)

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

## TOKENIZATION or it's late o'clock and I want to have at least plotted something
green_words <- grn_fairy_stories_only |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

blue_words <- blue_fairy_stories_only |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

red_words <- red_fairy_stories_only |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

## Word Frequency Analysis

green_word_counts <- green_words |>
  count(word, sort = TRUE)

blue_word_counts <- blue_words |>
  count(word, sort = TRUE)

red_word_counts <- red_words |>
  count(word, sort = TRUE)


green_word_counts |>
  slice_max(n, n = 20) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Most Common Words in ",
      italic("The Green Fairy Book")
    )),
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Greens"))(20))

blue_word_counts |>
  slice_max(n, n = 20) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Most Common Words in ",
      italic("The Blue Fairy Book")
    )),
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Blues"))(20))

red_word_counts |>
  slice_max(n, n = 20) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(x = n, y = word, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Most Common Words in ",
      italic("The Red Fairy Book")
    )),
    x = "Frequency",
    y = NULL
  ) +
  theme_minimal() +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Reds"))(21))

#Initial thoughts, interesting the top four words across all three books are "king", "prince", "princess", and "time". Interestingly, "father" is mentioned frequently in
# the blue and green books while "mother" is the term used more frequently in the Red book.

# Some light sentiment work
nrc_sentiments <- get_sentiments("nrc")

grn_word_sentiments <- green_words |>
  inner_join(nrc_sentiments, by = "word", relationship = "many-to-many") |>
  count(sentiment, sort = TRUE)

#Visualize the distribution of sentiments:

grn_word_sentiments |>
  mutate(sentiment = reorder(sentiment, n)) |>
  ggplot(aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Sentiment Distribution in ",
      italic("The Green Fairy Book")
    )),
    x = "Word Count",
    y = NULL
  ) +
  theme_minimal()

blue_word_sentiments <- blue_words |>
  inner_join(nrc_sentiments, by = "word", relationship = "many-to-many") |>
  count(sentiment, sort = TRUE)

#Visualize the distribution of sentiments:

blue_word_sentiments |>
  mutate(sentiment = reorder(sentiment, n)) |>
  ggplot(aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Sentiment Distribution in ",
      italic("The Blue Fairy Book")
    )),
    x = "Word Count",
    y = NULL
  ) +
  theme_minimal()

red_word_sentiments <- red_words |>
  inner_join(nrc_sentiments, by = "word", relationship = "many-to-many") |>
  count(sentiment, sort = TRUE)

#Visualize the distribution of sentiments:

red_word_sentiments |>
  mutate(sentiment = reorder(sentiment, n)) |>
  ggplot(aes(x = n, y = sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = expression(paste(
      "Sentiment Distribution in ",
      italic("The Red Fairy Book")
    )),
    x = "Word Count",
    y = NULL
  ) +
  theme_minimal()
