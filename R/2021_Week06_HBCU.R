# 2021 - Week 06 - HBCU ---------------------------------------------------

# Load libraries
library(tidyverse)
library(here)
library(ggtext)
library(glue)

# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_week06"))

# load fonts
extrafont::loadfonts(device = "win")

# Load data ---------------------------------------------------------------

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% 
  janitor::clean_names()

hbcu_black <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_black.csv') %>% 
  janitor::clean_names()


# Wrangling ---------------------------------------------------------------


all <- 
  hbcu_all %>% 
  select(year, all_enrol = total_enrollment)

black <-
  hbcu_black %>% 
  select(year, black_enrol = total_enrollment)

data <- 
  left_join(all, black, by = "year") %>% 
  mutate(
    non_black_enrol = all_enrol - black_enrol,
    perc_black = black_enrol/all_enrol,
    perc_non_black = non_black_enrol/all_enrol,
    med = median(perc_black)
    ) %>% 
  select(year, black = perc_black, non_black = perc_non_black, med) %>% 
  pivot_longer(
    cols = contains("black"),
    names_to = "race",
    values_to = "value"
  ) %>% 
    mutate(
      ymin = ifelse(race == "black", 0, 1-value),
      ymax = ifelse(race == "black", value, 1),
      xmin = 0,
      xmax = 1,
      med_ind = ifelse(race=="black", value > med, NA)
    )

med <- pull(distinct(data, med))

# Plot construction -------------------------------------------------------


ggplot(data, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)) + 
  geom_rect(aes(fill = race)) +
  geom_text(
    aes(x = 0.5, y = 0.5, label = year), 
    color = "#F9F1F6",
    size = 3, 
    family = "Montserrat Light"
    ) +
  geom_segment(
    aes(x = 0, xend = 1, y = med, yend = med), 
    color = "#FFBB33", 
    size = 0.5, 
    lty = 1
    ) +
  geom_point(
    aes(x = 0.5, y = 0.25, shape = med_ind), 
    color = "#F9F1F6", 
    size = 2,
    na.rm = TRUE
    ) +
  labs(
    title = "Black Enrollment at Historically Black Colleges and Universities",
    subtitle = "The percentage of Black students enrolled at HBCUs hovers around 80% of total students pretty consistently over the past 3 decades.",
    caption = "Viz: @MaiaPelletier | Data: NCES / Data.world"
    ) +
  scale_fill_manual(
    values = c("#963673", "#F9F1F6"), 
    guide = guide_none()
    ) +
  scale_shape(
    na.translate = FALSE, 
    name = "<span style=color:'#FFBB33'>Median enrollment (1976-2015) = 82%</span>",
    labels = c("Under median", "Over median")
  ) +
  facet_wrap(year~., nrow = 4) +
  coord_equal() +
  theme_void(base_family = "Montserrat") +
  theme(
    plot.margin = margin(t = 15, b = 10, l = 20, r = 20),
    plot.background = element_rect(fill = "#2D1122", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#F9F1F6", size = 16, margin = margin(b = 0)),
    plot.subtitle = element_markdown(hjust = 0.5, size = 7, color = "#F9F1F6", margin = margin(b = 15), family = "Lato"),
    plot.caption = element_text(hjust = 0.5, size = 5, color = "#F9F1F6"),
    legend.position = "top",
    legend.margin = margin(t = -10),
    legend.title = element_markdown(size = 8),
    legend.text = element_text(color = "#F9F1F6", size = 8),
    strip.text = element_blank()
  ) +
  ggsave(here("images", "progress", "imgs_2021_week06", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')
