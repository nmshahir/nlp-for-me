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
