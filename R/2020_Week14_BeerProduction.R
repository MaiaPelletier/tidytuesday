# 2020 Week 14: Beer Production ------------------------------------------------

library(tidyverse)   # All the fun tidy functions!
library(pjocolors)   # My new colour palette package
library(GGally)      # Creates plot matrices for data exploration
library(gghighlight) # Highlights certain geoms on ggplots
library(rvest)       # To scrape state abbrevs & names (i'm literally so lazy)
library(Cairo)       # Anti-aliasing to combat shitty Windows graphics

# Pretty colors for plot
pal1 <- pjo_palettes$BatOfTheLabyrinth 
pal2 <- pjo_palettes$TitansCurse

# My custom ggplot theme (based on ggthemes::theme_fivethirtyeight)
theme_set(theme_maia()) 

# Get data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# Explore -----------------------------------------------------------------

glimpse(beer_states)
skimr::skim(beer_states)
check_na(beer_states) # 19 NAs in barrels

# Total all barrels produced by state per year (regardless of type)
all_produced_barrels <-
  beer_states %>%
  na.omit() %>%
  filter(state != 'total') %>%
  group_by(state, year) %>%
  summarise(barrels = sum(barrels))

# Find top 3 states with largest percent increase in barrels from 2008 to 2019
increase_states <-
  all_produced_barrels %>%
  mutate(total_barrels = cumsum(barrels)) %>%
  filter(year %in% c(2008, 2019)) %>%
  mutate(pct_chg = (total_barrels - lag(total_barrels)) / lag(total_barrels)) %>%
  ungroup() %>%
  top_n(3, pct_chg) %>%
  pull(state) # Top 3 increase: AL, CT, TN

# Find top 3 states with smallest percent increase in barrels from 2008 to 2019
decrease_states <-
  all_produced_barrels %>%
  filter(year %in% c(2008, 2019)) %>%
  mutate(pct_chg = (barrels - lag(barrels)) / lag(barrels)) %>%
  ungroup() %>%
  top_n(-3, pct_chg) %>%
  pull(state) # Top 3 decrease: CO, NC, NJ


# Data Prep ---------------------------------------------------------------

# Shout out to @OppenheimerEvan for telling me about these handy character vectors stored in R
# No need for typing them all out or scraping!
state_key <- tibble(state = state.abb, state_name = state.name)

# Do some data manipulation
beer_data <- 
  beer_states %>%
  na.omit() %>%
  filter(state != 'total') %>%
  group_by(state, year) %>%
  summarise(barrels = sum(barrels)) %>%
  mutate(total_barrels = cumsum(barrels)) %>%
  left_join(state_key, by = 'state')

# Viz ---------------------------------------------------------------------

# Plot largest increase in production
increase_plot <-
  ggplot(beer_data, aes(year, barrels, group = state)) +
  geom_line(aes(color = state), show.legend = F) +
  gghighlight(                               # I love gghighlight
    state %in% increase_states,
    label_key = state_name,
    label_params = list(size = 3, family = 'Arial Narrow',
                        fill = 'grey95', alpha = 0.8, 
                        label.r = 0.1, segment.alpha = 0),
    unhighlighted_params = list(color = 'grey88')
  ) +
  labs(
    x = 'Year',
    y = 'Total number of barrels produced',
    title = 'States with the largest increase in beer production',
    subtitle = 'From 2008 to 2019',
    caption = '@MaiaPelletier | #TidyTuesday\nData sourced from Alcohol and Tobacco Tax and Trade Bureau'
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_log10(labels = scales::label_number()) +
  scale_color_manual(values = pal1) +
  theme_maia() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_blank())

# Plot largest decrease in production
decrease_plot <-
  ggplot(beer_data, aes(year, barrels, group = state)) +
  geom_line(aes(color = state), show.legend = F) +
  gghighlight(
    state %in% decrease_states,
    label_key = state_name,
    label_params = list(size = 3, family = 'Arial Narrow',
                        fill = 'grey95', alpha = 0.8, 
                        label.r = 0.1, segment.alpha = 0),
    unhighlighted_params = list(color = 'grey88')
  ) +
  labs(
    x = 'Year',
    y = 'Total number of barrels produced',
    title = 'States with the largest decrease in beer production',
    subtitle = 'From 2008 to 2019',
    caption = '@MaiaPelletier | #TidyTuesday\nData sourced from Alcohol and Tobacco Tax and Trade Bureau'
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_log10(labels = scales::label_number()) +
  scale_color_manual(values = pal2) +
  theme_maia() +
  theme(panel.grid = element_blank(),
        panel.grid.major = element_blank())

ggsave(increase_plot, file = 'Increase_StateBeerProd.png', type = 'cairo')
ggsave(decrease_plot, file = 'Decrease_StateBeerProd.png', type = 'cairo')
