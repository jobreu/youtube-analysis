library(readr)

yt <- read_csv("INSERT_FILE_NAME_HERE") # Namen der entsprechenden Datei (inkl. Dateiendung) einfügen

names(yt)

library(dplyr)

glimpse(yt)

library(tubecleanR)

processed_yt <- parse_yt_comments(yt,
                                  package = "vosonSML") # Paket, mit dem die Daten gesammelt wurden (vosonSML oder tuber)

glimpse(processed_yt)

write_csv(processed_yt, "./processed_yt.csv")

library(ggplot2)

processed_yt |>
  filter(!is.na(Published)) |>
  mutate(date = as.Date(Published)) |>
  count(date) |>
  arrange(date) |>
  mutate(cumulative_comments = cumsum(n)) |>
  ggplot(aes(x = date, y = cumulative_comments)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Cumulative Number of YouTube Comments Over Time",
    x = "Date",
    y = "Cumulative Comments"
  ) +
  theme_minimal()

library(tidyr)

processed_yt |>
  unnest_longer(Emoji) |>
  filter(!is.na(Emoji)) |>
  count(Emoji, sort = TRUE) |>
  slice_max(n, n = 15) |>
  ggplot(aes(x = reorder(Emoji, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Top 15 Emoji", x = "Emoji", y = "Frequency") +
  theme_minimal()

library(quanteda)

yt_corpus <- processed_yt |>
  select(CommentID, CleanedText, 
         Published, LikeCount, Author, VideoID) |>
  corpus(docid_field = "CommentID",
         text_field = "CleanedText")

tokens_yt <- tokens(yt_corpus)

tokens_yt <- tokens_remove(tokens_yt,
                              stopwords("de")) |>
  tokens_remove("dass")

tokens_yt <- tokens_select(tokens_yt,
                           min_nchar = 3)

dfm_yt <- dfm(tokens_yt)

library(quanteda.textstats)

dfm_yt |>
  textstat_frequency(n = 20)

library(quanteda.textplots)

dfm_yt |>
dfm_trim(min_termfreq = 10) %>%
  textplot_wordcloud()

library(ggplot2)

tstat_freq <- dfm_yt %>% 
  textstat_frequency(n = 20)

ggplot(tstat_freq, aes(x = frequency, y = reorder(feature, frequency))) +
  geom_col() + 
  labs(x = "Frequency", y = "Feature") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))
