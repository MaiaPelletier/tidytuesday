# Load libraries
library(Cairo)
library(ggridges)
library(lubridate)
library(tidyverse)
library(tidytext)

horror_movies <- read_csv('horrormoviedata.csv') # Read data

# Tidying data (only ended using the genre data)
tidy_horror <- 
  horror_movies %>%
  mutate(movie_run_time = str_remove(movie_run_time, '\\s*min'),
         movie_run_time = as.numeric(movie_run_time)) %>%
  mutate(budget = str_remove(budget, '\\W'),
         budget = str_remove_all(budget, ','),
         budget = str_remove(budget, '[A-Z]{3}'),
         budget = as.numeric(budget))%>%
  separate(genres, into = c('genre1'), sep = '\\|', remove = FALSE)

# Filter genres (less than 40 titles or NA review ratings)
genre_ratings <- 
  tidy_horror %>%
  filter(!is.na(review_rating)) %>%
  group_by(genre1) %>%
  add_count(genre1) %>%
  filter(n > 40)

# Plot distribution of ratings by genre
genre_ratings %>%
  ggplot(aes(x = review_rating, y = fct_reorder(genre1, review_rating, .desc = TRUE), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.005) +
  labs(x = 'Movie review ratings (10 point scale)', y = 'Primary genre') +
  ggtitle('Distribution of horror movie ratings by primary genre') +
  scale_fill_gradientn(colours = c('#fda403', '#ff6c00', '#8a1253', 'black'), name = 'Rating') +
  theme_ridges()

# Save plot
ggsave('GenreRatingsHorrorMovies.png', device = 'png', type = 'cairo')