# (2021 - week 04 - kenya census) ----------------------------------

##### set up #####

# Load libraries
library(tidyverse)
library(here)
library(ggforce)
library(patchwork)

# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_week04"))

# load fonts
extrafont::loadfonts(device = "win")

##### data cleaning + manipulation #####

# read data
gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')


##### data manip #####

crops_tidy <- 
  crops %>% 
  select(-Farming) %>% 
  pivot_longer(
    cols = where(is.numeric),
    names_to = "crop",
    values_to = "population_growing"
  ) %>% 
  mutate(
    SubCounty = str_to_title(SubCounty),
    population_growing = replace_na(population_growing, 0),
    crop_id = rep(1:9, n()/9),
    xstart = 0, 
    ystart = 0,
    xend = rep(cos(seq(0.3141, pi-0.3141, 0.31)), n()/9),
    yend = rep(sin(seq(0.3141, pi-0.3141, 0.31)), n()/9),
    alpha = ifelse(population_growing > 0, TRUE, FALSE)
  ) %>% 
    group_by(SubCounty) %>% 
    mutate(
      primary_crop = ifelse(which.max(population_growing) == crop_id, TRUE, FALSE),
      primary_crop = ifelse(population_growing == 0, FALSE, primary_crop)
    )


##### Main plot #####

kenyan_crops <- 
  crops_tidy %>% 
  filter(SubCounty != "Kenya") %>% 
  ggplot() +
  geom_segment(
    aes(x = 0, xend = 0, y = -0.5, yend = 0, size = population_growing),
    color = "#5B4F48",
    show.legend = FALSE
    ) +
  geom_diagonal(
    aes(xstart, ystart, xend = xend, yend = yend), 
    color = "#5B4F48",
    strength = 0.75, 
    lineend = "round", 
    size = 0.5,
    show.legend = FALSE,
    ) +
  geom_point(
    data = filter(crops_tidy, primary_crop, SubCounty != "Kenya"), 
    aes(x = xend, y = yend), 
    color = "#9d0208"
    ) +
  labs(
    title = "KENYA'S CROPS",
    caption = "@MaiaPelletier | Data: rKenyaCensus"
  ) +
  ylim(c(-0.5, 1.1)) +
  gghighlight::gghighlight(population_growing > 0) +
  scale_size(range = c(0.75, 5)) +
  facet_wrap(vars(SubCounty), nrow = 8) +
  theme_void(base_family = "Lato") +
  theme(
    plot.margin = margin(10, 20, 5, 20),
    plot.background = element_rect(fill = "#efefef", color = NA),
    plot.title = element_text(family = "Yeseva One", hjust = 0.5, size = 24, margin = margin(b = 10)),
    plot.caption = element_text(size = 6, hjust = 0.5, color = "grey50", margin = margin(t = 10)),
    strip.text = element_text(size = 7, margin = margin(b = 2))
  ) +
  ggsave(here("images", "progress", "imgs_2021_week04", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', height = 8, width = 6)


##### Tree Legend ######

label_data <- 
  crops_tidy %>% 
  filter(SubCounty == "Kenya") %>% 
  distinct(crop, xend, yend)

legend_tree <- 
  crops_tidy %>% 
  filter(SubCounty == "Kenya") %>% 
  ggplot() +
  geom_segment(
    aes(x = 0, xend = 0, y = -0.5, yend = 0, size = population_growing),
    color = "#5B4F48",
    show.legend = FALSE
  ) +
  geom_diagonal(
    aes(xstart, ystart, xend = xend, yend = yend), 
    color = "#5B4F48",
    strength = 0.75, 
    lineend = "round", 
    size = 0.75,
    show.legend = FALSE,
  ) +
  geom_text(
    data = label_data,
    aes(x = xend, y = yend, label = crop),
    size = 2, family = "Yeseva One", vjust = -1.5, color = "grey25"
  ) +
  annotate("text", label = "Branches = Crop farmed", 
           x = -0.65, y = -0.25, family = "Montserrat", color = "grey50", size = 3) +
  xlim(c(-1.1, 1.1)) +
  ylim(c(-0.5, 1.15)) +
  scale_size(range = c(0.75, 5)) +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#efefef", color = NA),
    plot.title = element_text(hjust = 0.5, size = 10, color = "red")
  )


##### Point legend #####

point_legend <- 
  ggplot(data.frame()) +
  geom_point(
    aes(x = 0.5, y = 0),
    color = "#9d0208",
    size = 4
  ) +
  geom_text(
    data = data.frame(),
    aes(x = 0.5, y = 0),
    label = "Most Farmed Crop",
    size = 3, 
    vjust = -3,
    family = "Montserrat",
    color = "grey50"
  ) +
  xlim(c(0,1)) +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#efefef", color = NA)
  )


##### Segment legend #####

segment_legend <- 
  data.frame(
  x = 1:4,
  y = 0,
  xend = 1:4,
  yend = 0.5,
  pop = c(0, 250000, 500000, 750000)
) %>% 
  ggplot() +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend, size = pop),
    color = "#5B4F48",
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = xend, y = yend, label = paste0(pop/1000, "K")),
    family = "Yeseva One",
    size = 2,
    color = "#5B4F48",
    vjust = -1
  ) +
  annotate("text", label = "Trunk thickness = Farming population",
           x = 2.5, y = 0.75,
           size = 3, color = "grey50", family = "Montserrat") +
  scale_x_continuous(expand = c(0.5, 0.5)) +
  ylim(c(0, 1)) +
  scale_size(
    range = c(0.75, 5),
    breaks = c(250000, 500000, 750000)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#efefef", color = NA)
  )


##### Full legend #####

(legend_tree + (point_legend / segment_legend) &
  theme(
    plot.background = element_rect(fill = "#efefef", color = NA),
    plot.title = element_text(family = "Yeseva One", hjust = 0.5, size = 24, margin = margin(b = 2)),
    plot.subtitle = element_text(face = "italic", hjust = 0.5, margin = margin(b = 2)),
    plot.caption = element_text(size = 4, hjust = 0.5, color = "grey70", margin = margin(t = 2))
  )) +
  plot_layout(widths = c(0.75, 0.5)) +
  plot_annotation(
    title = "KENYA'S CROPS",
    subtitle = "How to read",
    caption = "@MaiaPelletier | Data: rKenyaCensus"
  ) +
  ggsave(here("images", "progress", "imgs_2021_week04", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', height = 4, width = 6)









