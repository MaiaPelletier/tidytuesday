
# Week 37: Friends --------------------------------------------------------

# Load libraries
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(ggplot2)
library(mythemes) # My personal theme package
library(here)
library(ggtext)

# Set default theme
theme_set(theme_maia()) 

# Create directory to save all progress plots
dir.create(here("images", "progress", "imgs_week37"))

# Load fonts
extrafont::loadfonts(device = "win")

# Load data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 37)
friends <- tuesdata$friends

# Count mentions of "lesbian" (or words containing lesbian i.e. lesbians, etc.)
lesbi_friends <-
  friends %>% 
  mutate(lesbian = str_count(text, pattern = "lesbian")) %>% 
  group_by(season, episode) %>% 
  summarise(n_lesbian = sum(lesbian)) %>% 
  ungroup() %>% 
  mutate(episode_id = row_number())

# Summarize mentions by season
lesbian_by_season <-
  lesbi_friends %>% 
  group_by(season) %>% 
  mutate(n_lesbian_season = sum(n_lesbian)) %>% 
  ungroup() %>% 
  distinct(season, n_lesbian_season)

# Build plot!
lesbi_friends %>% 
  ggplot(aes(-season, episode)) +
  geom_point(
    aes(size = n_lesbian, color = factor(n_lesbian)), 
    show.legend = F
    ) +
  geom_point(
    data = lesbian_by_season,
    aes(x = -season, y = 27, fill = n_lesbian_season),
    shape = 21,
    size = 12,
    stroke = 1,
    color = "#FFFFFF",
    #color = "#FF9B55",
    show.legend = F
  ) +
  geom_text(
    data = lesbian_by_season,
    aes(x = -season, y = 27, label = n_lesbian_season),
    family = "Alata",
    color = "grey35",
    size = 3,
    show.legend = F
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "\"And you never knew she was a <i style = 'color:black'>lesbian...</i>\"",
    subtitle = "The number of times the word <span style = 'color:black;'>\"lesbian\"</span> was used over the 10 season run of <span style = 'color:black;'>FRIENDS</span>",
    caption = "@MaiaPelletier | #TidyTuesday | Data: {friends} by Emil Hvitfeldt"
  ) +
  coord_flip() +
  scale_x_continuous(
    breaks = seq(-10, -1),
    labels = paste("Season", seq(10, 1, -1))
  ) +
  scale_y_continuous(
    limits = c(1, 27)
  ) +
  scale_color_manual(
    values = c("#D6D6D6", "#D462A5", "#CC007A", "#7A0049"),
    name = "n_lesbian"
    ) +
  scale_fill_gradient(low = "#FFF3EB", high = "#FFB885") +
  theme(
    rect = element_rect(fill = "#F5F5F5"),
    text = element_text(family = "Alata"),
    plot.title = element_markdown(face = "plain", hjust = 0.25),
    plot.subtitle = element_markdown(size = 10, hjust = 0.1),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12)
  ) +
  ggsave(here("images", "progress", "imgs_week37", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', width = 7, height = 8)

