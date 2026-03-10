## TOKENIZATION or it's late o'clock and I want to have at least plotted something
green_words <- green_fairy_stories_only |>
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
