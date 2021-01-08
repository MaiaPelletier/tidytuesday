# (2021 - week 2 - global transit costs) ----------------------------------

#### set up ####

# Load libraries
library(tidyverse)
library(here)
library(ggforce)
library(cowplot)

# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_week2"))

# load fonts
extrafont::loadfonts(device = "win")

#### data cleaning + manipulation ####

# read data
transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# clean up data + manipulate + create derived columns for labels in plot
transit_cost_tidy <- 
  transit_cost %>% 
  slice(-c(538:544)) %>% # Remove a bunch of NA cols
  mutate(
    real_cost = as.numeric(real_cost),
    label_cost_km_millions = paste0("$", round(cost_km_millions), "M"),
    line_city = paste0(city, "\n", line),
    line_city = factor(line_city),
    line_city = fct_reorder(line_city, -cost_km_millions)
    ) %>% 
  filter(country %in% c("US", "CA", "MX"))

# circle specifications
R = 1 
x0 = 0 
y0 = 0

# functions to generate random point within a circle with the specifications
gen_r <- function(n, radius = R) radius * sqrt(runif(n))
gen_theta <- function(n) runif(n) * 2 * pi

# create the columns with the generated points & convert to cartesian
transit_cost_math <-
  transit_cost_tidy %>% 
    select(country, city, line, line_city, length, cost_km_millions, label_cost_km_millions) %>% 
    mutate(
      # Point generation
      r = map(cost_km_millions/10, gen_r),
      theta = map(cost_km_millions/10, gen_theta)
    ) %>% 
    unnest() %>% 
    mutate(
      # Convert to cartesian
      x = x0 + r * cos(theta),
      y = y0 + r * sin(theta)
    )

# data for ggforce::geom_circle() (speeds up geom rendering - ggplot tries to draw a circle for every data point otherwise)
circle <- data.frame(x0 = 0, y0 = 0, r = 1.1)

# data for geom_text() (same as above)
cost_labels <- 
  distinct(transit_cost_math, line_city, label_cost_km_millions, city, line) %>% 
  add_column(x = 0, y = -1.3)

#### plot creation ####
transit_plot <- 
  transit_cost_math %>% 
  ggplot() +
  geom_point(
    aes(x = 0.8, y = -0.85, size = length), 
    color = "red4"
    ) +
  geom_circle(
    data = circle, 
    aes(x0 = x0, y0 = y0, r = r), 
    fill = "white", 
    color = "white", 
    alpha = 0.8
    ) +
  geom_point(
    aes(x = x, y = y),
    color = "grey25",
    #position = "jitter",
    alpha = 0.25,
    size = 1.5
  ) +
  geom_text(
    data = cost_labels, 
    aes(x, y, label = label_cost_km_millions),
    size = 2,
    family = "Lato"
    ) +
  labs(
    caption = "@MaiaPelletier | Source: Transit Costs Project"
  ) +
  scale_size(range = c(1, 15), guide = guide_none()) +
  xlim(c(-1.5, 1.5)) +
  ylim(c(-1.55, 1.15)) +
  facet_wrap(line_city~., ncol = 5) +
  coord_fixed() +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#efefef", color = NA),
    plot.margin = margin(125, 25, 10, 25),
    strip.text = element_text(size = 6),
    plot.caption = element_text(size = 4)
  ) 

#### create legend ####

cost_labels_legend <- 
  cost_labels %>% 
  filter(city == "Montreal", str_detect(line, "Blue")) %>% 
  mutate(
    line_city = "City\nTransit Line"
  )

legend1 <- 
  transit_cost_math %>% 
  filter(city == "Montreal", str_detect(line, "Blue")) %>%
  mutate(
    line_city = "City\nTransit Line"
  ) %>% 
  ggplot() +
  geom_point(
    aes(x = 0.8, y = -0.85, size = length),
    color = "red4"
  ) +
  geom_circle(
    data = circle, 
    aes(x0 = x0, y0 = y0, r = r), 
    fill = "white", 
    color = "white", 
    alpha = 0.8
  ) +
  geom_point(
    aes(x = x, y = y),
    color = "grey25",
    #position = "jitter",
    alpha = 0.25,
    size = 1.5
  ) +
  geom_text(
    data = cost_labels_legend, 
    aes(x, y, label = label_cost_km_millions),
    size = 2,
    family = "Lato"
  ) +
  scale_size(range = c(2, 14), guide = guide_none()) +
  xlim(c(-1.5, 1.5)) +
  ylim(c(-1.5, 1.15)) +
  facet_wrap(line_city~., ncol = 5) +
  coord_fixed() +
  theme_void(base_family = "Lato") +
  theme(
    strip.text = element_text(size = 6),
    plot.title = element_text(family = "Libre Caslon Display", face = "bold")
  )

legend2 <- 
  transit_cost_math %>% 
    ggplot() +
    geom_point(
      aes(x = 0, y = 0, size = length),
      color = "red4"
    ) +
    geom_point(
      aes(x = 0, y = 0), size = 30, color = "#efefef"
    ) +
    scale_size(range = c(1, 15), breaks = c(2, 5, 15), labels = c("2 km", "5 km", "15 km"), name = NULL,
               guide = guide_legend(ncol = 1, label.position = "right")) +
    theme_void(base_family = "Lato") +
    theme(
      legend.position = c(0.5, 0.5),
      legend.text = element_text(size = 6)
    )



#### layers ####
ggdraw(transit_plot) +
  draw_label("North American\ntransit costs", 
             x = 0.22, y = 0.9, fontfamily = "Libre Caslon Display", size = 18, hjust = 0.5) +
  draw_line(x = c(0.55, 0.61), y = c(0.905, 0.905), color = "grey25", size = 0.25) +
  draw_line(x = c(0.55, 0.61), y = c(0.86, 0.86), color = "grey25", size = 0.25) +
  draw_line(x = c(0.1, 0.9), y = c(0.805, 0.805), color = "grey35", size = 0.5, lty = 3) +
  draw_line(x = c(0.4, 0.4), y = c(0.85, 0.95), color = "grey35", size = 0.5) +
  draw_label("number of dots: cost per\nkm of the transit line\n1 dot = 100k USD/km",
             x = 0.665, y = 0.91, fontfamily = "Lato", size = 5) +
  draw_label("area of circle: length of\ntransit line in km",
             x = 0.665, y = 0.8625, fontfamily = "Lato", size = 5) +
  draw_plot(legend1, height = 0.15, width = 0.15, x = 0.45, y = 0.825) +
  draw_plot(legend2, height = 0.15, width = 0.15, x = 0.75, y = 0.825) +
  ggsave(here("images", "progress", "imgs_2021_week2", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')

