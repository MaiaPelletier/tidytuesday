
# 2020 Week 15: Tour de France --------------------------------------------

library(tidyverse)
library(lubridate)
library(Cairo)

theme_set(theme_maia())
pal <- pjocolors::pjo_palettes$LightningThief

# Get data
tdf_winners <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

# Explore -----------------------------------------------------------------

glimpse(tdf_winners)
skimr::skim(tdf_winners)

# Cyclists with more than one TDF win -----

tdf_winners %>%
  count(winner_name, sort = TRUE) %>%
  filter(n > 1) %>%
  summarise(
    num_of_racers = n_distinct(winner_name), 
    num_of_wins = sum(n)
    ) # 21 racers who account for 64/106 TDF wins

tdf_winners %>%
  count(winner_name, name = 'num_of_wins') %>%
  filter(num_of_wins > 1) %>%
  ggplot(aes(reorder(winner_name, num_of_wins), num_of_wins)) +
  geom_col(fill = pal[1], width = 0.6, alpha = 0.8) +
  labs(x = NULL, y = 'Tour de France wins') +
  coord_flip()

# Nicknames (for fun) -----

tdf_winners %>%
  filter(!is.na(nickname)) %>%
  distinct(winner_name, nickname)
# Apparently nicknaming is a tradition in the TDF/cycling
# https://www.bikeraceinfo.com/riderhistories/cyclist-nicknames.html

# Country wins -----

tdf_winners %>%
  count(nationality, sort = TRUE) %>%
  ggplot(aes(reorder(nationality, n), n)) +
  geom_col(fill = pal[2], alpha = 0.8, width = 0.6) +
  labs(x = NULL, y = 'Tour de France wins') +
  coord_flip()

# Race distance ----- 

ggplot(tdf_winners, aes(distance)) +
  geom_histogram(fill = pal[3], binwidth = 200, alpha = 0.8) +
  labs(x = 'Race distance (km)', y = 'Frequency')

min_max_dist <- bind_rows(
  
  tdf_winners %>%
    mutate(start_year = year(start_date)) %>%
    top_n(1, distance),
  
  max_dist <- tdf_winners %>%
    mutate(start_year = year(start_date)) %>%
    filter(start_year > 1905) %>%
    top_n(-1, distance)
)

tdf_winners %>%
  mutate(start_year = year(start_date)) %>%
  ggplot(aes(start_year, distance)) +
  geom_line(col = pal[4]) +
  geom_point(data = min_max_dist,
             aes(start_year, distance),
             col = 'grey35') +
  geom_text(data = min_max_dist,
            aes(label = start_year),
            size = 3,
            color = 'grey25',
            hjust = -0.25,
            family = 'Arial Narrow') +
  labs(x = 'Race year', y = 'Race distance (km)')
