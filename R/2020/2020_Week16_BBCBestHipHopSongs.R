# Week 16: Hip hop rankings -----------------------------------------------

library(tidyverse)
library(Cairo)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# I was initially disappointed to see so little Kendrick at the top
# But then as I explored the data, I noticed that there was a lot of Kendrick
# on the list, which makes sense. Kendrick has so many songs that are worthy
# of the title of "Best Hip-Hop" i can't be boiled down to just one

rankings %>%
  count(artist, sort = TRUE) # Kendrick is no. 2 with 10 songs


# Create theme_damn -------------------------------------------------------

# Palette of album cover colours
damn_pal <-
  c(
    damn_red = '#F80101',
    damn_dark = '#372127',
    tshirt_light = '#E7E6EF',
    brick_dark = '#7F3547',
    brick_light = '#A36A7A',
    damn_brown = '#4E3535'
  )

# Create theme based on DAMN. album artwork
theme_damn <- function() {
  
  extrafont::loadfonts(device = 'win', quiet = TRUE)
  
  library(ggplot2)
  
  ggthemes::theme_foundation(
    base_size = 12, 
    base_family = 'Compacta BT' # Compacta is the Parental Advisory font
    ) +
    theme(
      rect = element_rect(fill = damn_pal['brick_light'],
                          linetype = 1,
                          color = NA),
      text = element_text(color = damn_pal['tshirt_light']),
      panel.grid.major = element_line(color = damn_pal['brick_dark']),
      panel.grid.minor = element_blank(),
      plot.title = element_text(family = 'Times New Roman', 
                                color = damn_pal['damn_red'], 
                                size = 50,
                                hjust = 1),
      plot.subtitle = element_text(color = damn_pal['damn_dark'],
                                   size = 15,
                                   hjust = -1.25),
      plot.caption = element_text(color = damn_pal['damn_dark'], 
                                  size = 8),
      axis.ticks = element_blank(),
      axis.title = element_text(color = damn_pal['brick_dark']),
      axis.text = element_text(family = 'Arial Narrow',
                               size = 10,
                               color = damn_pal['damn_dark'])
    )
  
}


# Create plot --------------------------------------------------------------------

rankings %>%
  filter(artist == 'Kendrick Lamar') %>%
  ggplot(aes(reorder(title, points), points)) +
  geom_col(fill = damn_pal['tshirt_light'], alpha = 0.9) +
  labs(title = 'BEST KENDRICK.', 
       subtitle = 'ACCORDING TO THE BBC\'S LIST OF GREATEST HIP-HOP SONGS',
       caption = '@MAIAPELLETIER | #TIDYTUESDAY | DATA SOURCED FROM THE BBC',
       y = 'SONG POINTS', 
       x = NULL) +
  coord_flip() +
  theme_damn() +
  theme(axis.title.x = element_text(size = 9))

ggsave('KendrickBestSongs.png', type = 'cairo')
