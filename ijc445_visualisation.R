R code

## ---- 0) Reset ----
rm(list = ls())

suppressPackageStartupMessages({
 library(tidyverse)
 library(lubridate)
 library(tidytext)
 library(textdata)
 library(scales)
 library(viridis)
 library(patchwork)
 library(ggplot2)


})

## ---- 1) Load raw ----
data_path <- "/Users/rahulsharma/Downloads/billboard_24years_lyrics_spotify 2.csv"
bb0 <- readr::read_csv(data_path, show_col_types = FALSE)

## ---- 2) Safe column names ----
safe_names <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  tolower(gsub("_+", "_", x))
}
names(bb0) <- safe_names(names(bb0))

## ---- 3) Minimal cleaning / standardisation ----
bb <- bb0 %>%
  mutate(
    rank  = as.numeric(ranking),   # lower rank = better
    title = song,
    artist = band_singer,
    year  = as.integer(year)
  ) %>%
  filter(!is.na(year), dplyr::between(year, 2000, 2023)) %>%
  filter(!is.na(title), !is.na(artist), !is.na(rank)) %>%
  mutate(
    song_id = tolower(paste0(trimws(title), " — ", trimws(artist))),
    success_score = 101 - rank,
    top10 = as.integer(rank <= 10)
  )

## ---- 4) cap outliers on numeric Spotify features (safe) ----
possible_feats <- c(
  "danceability","energy","valence","acousticness","instrumentalness",
  "liveness","speechiness","tempo","loudness","duration_ms","popularity",
  "key","mode","time_signature"
)
feat_cols <- intersect(possible_feats, names(bb))

cap_iqr <- function(x) {
  if (!is.numeric(x)) return(x)
  qs <- stats::quantile(x, c(.25,.75), na.rm = TRUE)
  iqr <- qs[2] - qs[1]
  lo <- qs[1] - 3*iqr; hi <- qs[2] + 3*iqr
  pmin(pmax(x, lo), hi)
}

if (length(feat_cols) > 0) {
  bb <- bb %>% mutate(across(all_of(feat_cols), cap_iqr))
}

## ---- 5) Export cleaned dataset ----
readr::write_csv(bb, "billboard_clean_2000_2023.csv")

cat("\nSaved:", "billboard_clean_2000_2023.csv",
    "\nRows:", nrow(bb),
    "\nYears:", min(bb$year, na.rm=TRUE), "to", max(bb$year, na.rm=TRUE), "\n")

data <- read_csv("/Users/rahulsharma/Downloads/billboard_clean_2000_2023.csv")

# lyrical emotional analysis

nrc <- textdata::lexicon_nrc() %>%
  distinct(word, sentiment)

emotion_year <- data %>%
  filter(!is.na(lyrics), !is.na(year)) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(nrc, by = "word", relationship = "many-to-many") %>%
  filter(sentiment %in% c("joy", "sadness", "anger", "fear")) %>%
  count(year, sentiment) %>%
  group_by(year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

v1 <- ggplot(emotion_year, aes(x = year, y = prop, colour = sentiment)) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Trends in Lyrical Emotion (2000–2023)",
    x = "Year",
    y = "Proportion of emotion words",
    colour = "Emotion"
  ) +
  theme_minimal(base_size = 12)

v1

#lyrical complexity(TTR)
complexity_year <- data %>%
  filter(!is.na(lyrics), !is.na(year)) %>%
  unnest_tokens(word, lyrics) %>%
  distinct(song_id, year, word) %>%
  count(song_id, year, name = "unique_words") %>%
  left_join(
    data %>%
      filter(!is.na(lyrics), !is.na(year)) %>%
      unnest_tokens(word, lyrics) %>%
      count(song_id, year, name = "total_words"),
    by = c("song_id", "year")
  ) %>%
  mutate(ttr = unique_words / total_words) %>%
  group_by(year) %>%
  summarise(median_ttr = median(ttr, na.rm = TRUE), .groups = "drop")

v2 <- ggplot(complexity_year, aes(x = year, y = median_ttr)) +
  geom_line(linewidth = 0.9) +
  geom_smooth(se = FALSE, linewidth = 0.9) +
  labs(
    title = "Lyrical Complexity Over Time",
    x = "Year",
    y = "Median Type–Token Ratio (proxy)"
  ) +
  theme_minimal(base_size = 12)

print(v2)
# Sentiment by success quartile
afinn <- get_sentiments("afinn")

sent_song <- data %>%
  filter(!is.na(lyrics), !is.na(success_score)) %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(afinn, by = "word") %>%
  group_by(song_id) %>%
  summarise(sentiment = mean(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    data %>% distinct(song_id, success_score),
    by = "song_id"
  ) %>%
  mutate(success_q = ntile(success_score, 4))

v3 <- ggplot(sent_song, aes(x = factor(success_q), y = sentiment)) +
  geom_violin(trim = FALSE, fill = "grey80") +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(
    title = "Sentiment Distribution Across Success Quartiles",
    x = "Success quartile (1 = lower, 4 = higher)",
    y = "Mean AFINN sentiment per song"
  ) +
  theme_minimal(base_size = 12)

print(v3)


#Audio features vs success
audio_subset <- data %>%
  filter(
    !is.na(danceability),
    !is.na(energy),
    !is.na(success_score)
  )

v4 <- ggplot(audio_subset,
             aes(x = danceability, y = energy, colour = success_score)) +
  geom_point(alpha = 0.6) +
  scale_colour_viridis_c() +
  labs(
    title = "Danceability vs Energy (Spotify-matched subset)",
    subtitle = paste0("n = ", nrow(audio_subset), " songs"),
    x = "Danceability",
    y = "Energy",
    colour = "Success score"
  ) +
  theme_minimal(base_size = 12)

print(v4)



#composite visualisation
final_plot <- (v1 | v2) / (v3 | v4)
final_plot

ggsave("composite_visual.png", final_plot, width = 14, height = 10, dpi = 300)
