library(tidyverse)
library(lubridate)
library(gganimate)
theme_set(theme_minimal())
options(scipen = 999)

# Import data
tuesdata <- tidytuesdayR::tt_load("2020-01-21")
spotify_songs <- tuesdata$spotify_songs

feature_names <- names(spotify_songs)[12:23]

# create new variable with year alone
spotify_songs$year <- year(ymd(spotify_songs$track_album_release_date))
spotify_songs$month <- month(ymd(spotify_songs$track_album_release_date))

# Average features per year
features_year <- 
  spotify_songs %>% 
  group_by(year) %>% 
  summarise(n = n(),
            danceability = mean(danceability),
            energy = mean(energy),
            key = mean(key),
            loudness = mean(loudness),
            mode = mean(mode),
            speechiness = mean(speechiness),
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness),
            liveness = mean(liveness),
            valence = mean(valence),
            tempo = mean(tempo),
            duration_ms = mean(duration_ms)) %>% 
  na.omit()

# arrange into tidy data format
features_year_long <- pivot_longer(features_year, cols = names(spotify_songs)[12:23])
colnames(features_year_long)[3] <- "Feature"

# plot individual feature
features_year_long %>% 
  ggplot(aes(x = year, y = value)) + geom_line() + facet_wrap(~Feature, scales = "free")

# Calculate difference values from one year to the next and normalized it
calc_diff <- function(data) {
  features <- data[, 3:14]
  features_diff <- apply(features, 2, diff)
  # function to normalize between -1 and 1
  normalize <- function(x) {
    2*((x - min(x)) / (max(x) - min(x))) - 1
  }
  # Apply to all columns
  features_diff <- apply(features_diff, 2, normalize)
  # Add first row
  features_diff <- rbind(c(rep(0, times = 12)), features_diff)
  # Re-add year and n columns
  features_diff <- as.data.frame(cbind(features_year$year, features_year$n, features_diff))
  colnames(features_diff)[1:2] <- c("year", "n")
  # Remove 1st row
  features_diff <- features_diff[-1, ]
  # Change to tidy format
  result <- pivot_longer(features_diff, cols = names(spotify_songs)[12:23])
  
  names(result)[3] <- "Feature"
  return(result)
}

features_diff_long <- calc_diff(features_year)
ten_sons_min <- filter(features_diff_long, n > 9)

# Animate
ggplot(ten_sons_min, aes(x = year, y = value, color = Feature)) + 
  geom_line(aes(group = 1)) + 
  geom_point() + 
  ylim(-1, 1) + 
  transition_reveal(year) + 
  theme(axis.text.x = element_text(angle = 270)) + 
  facet_wrap(. ~ Feature, scales = "free")

# Genre popularity over time
by_genre <- 
  spotify_songs %>% 
  group_by(year, playlist_genre) %>% 
  summarise(n = n()) %>% 
  na.omit()

# remove the nested loop into something more elegant
calc_propn <- function(data) {
  # Create new column to put proportions in
  data$Propn <- 0
  # Make vector of yers
  years <- na.omit(unique(by_genre$year))
  # Isolate one year at a time
  for(i in seq_along(years)){
    year_subset <- filter(data, year == years[i])
    # calc propn for each genre present
    genres <- na.omit(unique(year_subset$playlist_genre))
    for(j in seq_along(genres)) {
      propn <- year_subset$n[j]/sum(year_subset$n)
      data$Propn[data$year == years[i] & data$playlist_genre == genres[j]] <- propn
    }
  }
  return(data)
}
by_genre <- calc_propn(by_genre)

# Calc cumulative song propn in each year
by_year <- 
  spotify_songs %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(propn_total = cumsum(n/30947)) %>% 
  na.omit()

# add this info to genre dataframe
by_genre <- inner_join(by_genre, by_year, by = 'year')
# make sure year is an integer, for some reason this is an issue?
by_genre$year <- as.integer(by_genre$year)

ggplot(by_genre, aes(x = playlist_genre, y = Propn, fill = playlist_genre)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_hline(aes(yintercept = propn_total), alpha = .1, size = 1.5, col = "Black") + 
  transition_time(year) + 
  labs(title = "Year: {frame_time}") + 
  shadow_wake(.1)
