library(tidyverse)
library(skimr)
library(corrplot)
library(Cairo)
library(gganimate)
library(glue)

theme_set(theme_minimal())

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

skim(nyc_squirrels)

# Tidy data (ish, its pretty good already)
tidy_squirrels <- 
  nyc_squirrels %>%
  mutate(date = lubridate::mdy(date))

# Correlations between squirrel actions
behaviors <- nyc_squirrels %>%
  select_if(is.logical) %>%
  mutate_each(as.integer)

behaviors_corrs <- cor(behaviors)

corrplot(
  behaviors_corrs,
  method = 'square',
  order = 'FPC', 
  cl.pos = 'b', 
  tl.col = 'grey20',
  tl.cex = 0.5,
  tl.pos = 'd',
  col = c('black', 'white'),
  bg = 'grey',
  diag = TRUE
  )


# animate -----------------------------------------------------------------

p <- tidy_squirrels %>%
  filter(age %in% c('Adult', 'Juvenile')) %>%
  ggplot(aes(long, lat)) +
  geom_point(aes(color = age), alpha = 0.6, size = 6, shape = 17) +
  ggtitle('test title')+
  labs(x = 'Longitude', y = 'Latititude') +
  ggthemes::scale_color_canva(palette = 'Summer sunflower') +
  theme_void() +
  theme(plot.title = element_text(size = 26, hjust = 0.5))
p

anim <- p + 
  transition_states(date, transition_length = 20, state_length = 10) +
  ease_aes('cubic-in-out') +
  ggtitle('Squirrel locations on {closest_state}')

animate(anim, fps = 5.5)

anim_save('SquirrelPositions.gif', anim, fps = 5.5)

