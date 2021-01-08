# Week 20: Volcanoes ------------------------------------------------------

# >:(

# Load libraries
library(tidyverse) # All the fun tidy functions
library(mythemes)  # My personal ggplot themes
library(ggtext)    # Better text rendering for ggplot

# Read in data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# Join and process data
region_eruptions <-
  eruptions %>%
  select(
    volcano_number,
    eruption_category,
    erupt_latitude = latitude,
    erupt_longitude = longitude
  ) %>%
  left_join(volcano, by = 'volcano_number') %>%
  select(-c(
    starts_with("major"),
    starts_with("minor"),
    starts_with("population")
  )) %>%
  filter(!is.na(volcano_name)) %>%
  add_count(subregion, name = 'n_subregion_eruptions') %>%
  group_by(subregion) %>%
  mutate(
    avg_lat = mean(erupt_latitude),
    avg_long = mean(erupt_longitude)
    ) %>%
  ungroup() %>%
  select(subregion,
         avg_lat,
         avg_long,
         n_subregion_eruptions) %>%
  distinct(subregion,
           avg_lat,
           avg_long,
           n_subregion_eruptions)

# Get data for creating map in ggplot
map_dat <- 
  ggplot2::map_data("world") %>% 
  fortify() %>% 
  as_tibble()

# Create viz
ggplot(data = map_dat, aes(long, lat)) +
  geom_map(map = map_dat, aes(map_id = region), fill = 'grey38') +
  geom_point(
    data = region_eruptions,
    aes(
      x = avg_long,
      y = avg_lat,
      alpha = n_subregion_eruptions,
      size = n_subregion_eruptions
    ),
    color = "#DE4400"
  ) +
  labs(
    title = "Global Eruptions <span style='font-size:10pt; color:grey90;'>Recorded volcanic eruptions in regions across the globe</span>",
    caption = "@MaiaPelletier | #TidyTuesday | Data sourced from The Smithsonian Institution"
  ) +
  scale_alpha(range = c(0.65, 0.25)) +
  scale_size(range = c(1, 10)) +
  guides(
    alpha = guide_legend(title = "Number of eruptions",
                         title.position = "bottom",
                         title.hjust = 0.5,
                         title.vjust = 0.9),
    size = guide_legend(title = "Number of eruptions")
    ) +
  theme_map_dark() +
  theme(
    legend.position = c(0.865, 0.965),
    plot.title = element_markdown(size = 20, color = '#B12800')
    )

ggsave('images/Week20_Volcanos.png', type = 'cairo')

