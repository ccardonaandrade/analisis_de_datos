##############################
# R Script para los datos
# de Spotify
##############################

# Cargamos los paquetes
library(tidyverse)


# Working directory
setwd(dir = "/Users/ccard/Dropbox/analitica_datos/2024-II/slides/lecture3/data")

# https://www.kaggle.com/code/lusfernandotorres/spotify-top-hits-2000-2019-eda/data
data <- read.csv("spotify_data.csv")

# Creamos la dummy para "latin"
data <- data %>%
  mutate(latin = ifelse(grepl("latin", genre), 1, 0))

average_popularity <- data %>%
  group_by(year, latin) %>%
  summarize(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
  ungroup()


# Plot the average popularity over time
ggplot(data = average_popularity, aes(x = year, y = avg_popularity, color = factor(latin))) +
  geom_line() +
  labs(title = "Average Popularity Over Time",
       x = "Year",
       y = "Average Popularity",
       color = "Latin") +
  theme_minimal()


filtered_data <- data %>%
  filter(year %in% c(2000, 2010, 2019))


ggplot(filtered_data, aes(x = danceability, color = factor(year), fill = factor(year))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Danceability Across Years",
       x = "Danceability",
       y = "Density",
       color = "Year",
       fill = "Year") +
  theme_minimal()


ggplot(data, aes(x = danceability, color = factor(explicit), fill = factor(explicit))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ year) +
  labs(title = "Density of Danceability by Explicit Status Across Years",
       x = "Danceability",
       y = "Density",
       color = "Explicit",
       fill = "Explicit") +
  theme_minimal()
