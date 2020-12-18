# Week 51: Ninja Warrior -------------------------------------------

# Load libraries
library(tidytuesdayR)
library(here)
library(tidyverse)
library(fishualize)
library(gghighlight)
library(ggtext)

# Create directory to save all progress plots
dir.create(here("images", "progress", "imgs_week51"))

# Load fonts
extrafont::loadfonts(device = "win")

# Load data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 51)
ninja_warrior <- tuesdata$ninja_warrior

# Create obstacle groups
warrior_tidy <- 
  ninja_warrior %>% 
  mutate(
    id = row_number(),
    obstacle_group = cumsum(obstacle_order == 1)
    ) %>% 
  add_count(obstacle_name, sort = T) %>%
  mutate(
    obstacle_name = ifelse(n <= 11, "Other", obstacle_name),
    obstacle_name = str_to_upper(obstacle_name)
    ) %>% 
  arrange(id) 

# Create season groups for season labels
season_groups <- 
  warrior_tidy %>% 
  group_by(season) %>% 
  summarise(
    min_group = min(obstacle_group),
    max_group = max(obstacle_group),
    mean_group = mean(obstacle_group),
  ) %>% 
  mutate(
    label_season = paste0("S", season)
  )

season_groups2 <- 
  left_join(
    expand(season_groups, obstacle = season, min_group) %>% mutate(id = row_number()),
    expand(season_groups, season, max_group) %>% mutate(id = row_number()),
    by = "id"
  )

# Create obstacle labels
obstacle_labels <-
  warrior_tidy %>% 
  distinct(obstacle_order) %>% 
  mutate(obstacle_label = c(paste0("OBSTACLE 0", 1:9), "OBSTACLE 10"))

# Colour palette inspired by {fishualize} palette "Sparisoma_viride"
palette <- c("#4558C4", "#7E8BD6", "#77E7F8", "#14CCBC", "#86DB7B", "#FFEB00", "#FDAD0D", "#FE6F5D")


# Plot creation -----------------------------------------------------------

ggplot(warrior_tidy, aes(obstacle_group, -obstacle_order)) +
  #### Geoms ####
  geom_segment(
    data = season_groups2,
    aes(x = min_group, xend = max_group, y = -obstacle, yend = -obstacle),
    color = "grey35",
    size = 0.1
  ) +
  geom_point(
    aes(color = obstacle_name), 
    size = 2, 
    shape = 18
  ) +
  gghighlight(
    obstacle_name != "OTHER", 
    unhighlighted_params = list(shape = 16, size = 0.75, color = "grey35")
  ) +
  geom_richtext(
    data = season_groups,
    aes(y = -10.75, x = mean_group, label = label_season),
    family = "Roboto",
    fontface = "bold",
    size = 2,
    text.color = "white",
    fill = NA, 
    color = NA
  ) +
  geom_richtext(
    data = obstacle_labels,
    aes(x = 5, y = -obstacle_order + 0.35, label = obstacle_label),
    family = "Roboto",
    fontface = "bold",
    size = 2.5,
    text.color = "white",
    fill = NA,
    color = NA
  ) +
  #### LABELS ####
  labs(
    title = "NINJAWARRIOR",
    subtitle = "THE MOST POPULAR OBSTACLES",
    caption = "@MaiaPelletier | #TidyTuesday | Data source: sasukepedia"
  ) +
  #### SCALES ####
  scale_color_manual(values = palette) +
  guides(color = guide_legend(
    title = NULL,
    nrow = 1,
    keyheight = unit(10, "points"),
    keywidth = unit(60, "points"),
    override.aes = list(size = 4),
    label.position = "top",
    label.theme = element_text(size = 6, color = "white", family = "Roboto", face = "bold")
  )) +
  #### THEME ELEMENTS ####
  theme_void(base_size = 6, base_family = "Roboto") +
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(10, 0, 5, 0),
    plot.title = element_markdown(
      hjust = 0.5,
      size = 54, 
      face = "bold"
      ),
    plot.subtitle = element_markdown(
      hjust = 0.5,
      face = "bold",
      size = 10,
      margin = margin(-8, 0, 0, 0)
    ),
    plot.caption = element_text(color = "grey25", hjust = 0.95),
    legend.position = "top",
    legend.box.margin = margin(20, 0, 0, 0),
  ) +
  ggsave(here("images", "progress", "imgs_week51", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', width = 8.5, height = 5.5)

