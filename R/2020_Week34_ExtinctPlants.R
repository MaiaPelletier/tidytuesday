
# Week 34: Extinct Plants -------------------------------------------------

# Load libraries
library(tidytuesdayR)
library(tidyverse)
library(here)

# Set my personal theme
theme_set(mythemes::theme_maia()) 

# Load fonts
extrafont::loadfonts("win")

# Define colour palette for dots
pal <- c("#9FBCA5", "#52796F", "#354F52", "#2F3E46")

# Read in data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 34)
threats <- tuesdata$threats

# Data wrangling (I love forcats!)
threats_per_cont <- 
  threats %>% 
  filter(threatened != 0) %>% 
  group_by(continent) %>% 
  count(threat_type, name = "n_threats") %>% 
  mutate(
    threat_type = str_replace_all(threat_type, " ", "\n"),
    continent = str_replace(continent, " ", "\n"),
    total_extinctions_per_cont = sum(n_threats)
  ) %>% 
  ungroup() %>% 
  group_by(threat_type) %>% 
  mutate(
    total_extinctions_per_threat = sum(n_threats)
    ) %>% 
  ungroup() %>% 
  mutate(
    continent = fct_reorder(continent, total_extinctions_per_cont),
    threat_type = fct_reorder(threat_type, -total_extinctions_per_threat),
    threat_type = fct_relevel(threat_type, "Unknown", after = Inf)
  )


# Construct plot
threats_per_cont %>% 
  ggplot(aes(threat_type, continent)) +
  geom_point(size = 8, shape = 21, color = "grey55", fill = "white") +
  geom_point(aes(size = n_threats, color = n_threats)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Threats to Global Plant Existences",
    subtitle = "As of 2020, five hundred plant species globally are considered extinct. 19.6% of those were endemic to Madagascar (Africa).\nAfrica is faced with significant and mounting threats resulting from activities including logging, fuelwood collection, and deforestation for agriculture and mining.\nThe ongoing negative effects on biodiversity are projected to be compounded further by climate change by the end of this century.",
    caption = "@MaiaPelletier | #TidyTuesday | Data: IUCN Red list of Threatened Species (Version 2020-1)"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "left") +
  scale_size(
    range = c(1, 7), 
    breaks = c(10, 50, 100),
    name = "Extinct plant species"
  ) +
  scale_color_gradientn(
    colours = pal,
    breaks = c(10, 50, 100),
    name = "Extinct plant species",
    guide = guide_legend()
    ) +
  theme(
    rect = element_rect(fill = "#F5F6F4"),
    text = element_text(family = "Raleway"),
    plot.title = element_text(family = "Lora",
                              face = "plain",
                              hjust = 0.5,
                              margin = margin(10, 0, 0, 0),
                              size = 24),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 7,
                                 color = "grey55",
                                 margin = margin(5, 0, 15, 0)),
    plot.caption = element_text(size = 5,
                                color = "grey75",
                                vjust = -10),
    panel.grid.major.x = element_line(color = "grey55", size = 0.5),
    legend.title = element_text(size = 8, color = "grey40"),
    legend.text = element_text(size = 6),
    legend.margin = margin(),
    axis.text.x = element_text(size = 8, 
                               family = "Lora"),
    axis.text.y = element_text(size = 10, 
                               hjust = 0.5)
  ) +
  ggsave(here("images", "progress", "imgs_week34", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', width = 10, height = 6)
