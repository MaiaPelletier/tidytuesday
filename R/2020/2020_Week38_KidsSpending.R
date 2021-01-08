
# Week 38: Public spending on kids -------------------------------------------

# Load libraries
library(tidytuesdayR)
library(mythemes) # My personal theme package
library(here)
library(tidyverse)
library(geofacet)
library(cowplot)

# Set default theme
theme_set(theme_maia()) 

# Create directory to save all progress plots
dir.create(here("images", "progress", "imgs_week38"))

# Load fonts
extrafont::loadfonts(device = "win")

# Load data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 38)
kids <- tuesdata$kids

# Function to create the points to be passed thru geom_polygon() for the squares
make_points <- function(point) {
  
  tribble(
    ~x,     ~y,
    0,      0,
    0,      point,
    point,  point,
    point,  0
  )
  
}

# Data wrangling
education_spending <- 
  kids %>% 
  filter(
    variable %in% c("PK12ed", "highered"),
    year %in% c(1997, 2016)
  ) %>%
  select(state, year, variable, inf_adj) %>% 
  arrange(state, variable) %>% 
  pivot_wider(names_from = year, values_from = inf_adj) %>% 
  mutate(
    inc = `2016` - `1997`,
    perc_inc = inc / `1997`,            # Calculate % increase
    coords = map(perc_inc, make_points) # Make points for squares
    ) %>% 
  unnest(coords) %>% 
  mutate(
    x = ifelse(variable == "PK12ed", -x, x),   # Rotate so the PK12ed squares are opposite to the highered sqaures
    y = ifelse(variable == "PK12ed", -y, y)
    ) %>% 
  left_join(                                  # Add state abbrevs for shorter facet titles
    tibble(state_label = state.abb, state.name) %>% 
      add_row(state_label = "DC", state.name = "District of Columbia"),
    by = c("state" = "state.name")
  )

# Build main plot
main_plot <- 
  education_spending %>% 
  ggplot() +
  geom_polygon(aes(x, y, fill = variable), show.legend = F) +
  geom_segment(
    aes(x = -1.5, xend = 1.5, y = 2, yend = 2),
    color = "grey75",
    size = 0.15
    ) +
  geom_text(
    data = education_spending %>% distinct(state, state_label),
    aes(x = 0, y = 2.7, label = state_label),
    family = "Roboto",
    size = 2,
    color = "grey50"
    ) +
  labs(
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c("#F8A88B", "#F6CA83")) +
  scale_x_continuous(limits = c(-2.75, 2.75)) +
  scale_y_continuous(limits = c(-2.75, 2.75)) +
  facet_geo(~ state, grid = "us_state_grid2") + 
  theme(
    rect = element_rect(fill = "#FFF8F0"),
    text = element_text(family = "Roboto", size = 5),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  ) +
  ggsave(here("images", "progress", "imgs_week38", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', width = 8, height = 6)

# Data for the legend text
legend_data <- 
  tribble(
    ~x, ~y, ~text,
    2.2, -0.2, "0%",
    -2.2, 0.2, "0%",
    2.2, 2.2, "200%",
    -2.2, -2.2, "200%",
    3, 1, "% increase\nin higher\neducation\nspending",
    -3, -1, "% increase\nin PK-12\neducation\nspending"
)

# Data points to create the legend sqaure outline (showing the max value)
max_square <- 
  tribble(
    ~x,    ~y,
    -2,    -2,
    -2,     2,
    2,      2,
    2,     -2
  )

# Build legend plot
legend_plot <- 
  education_spending %>% 
  filter(state_label == "WY") %>% 
  ggplot() +
  geom_polygon(
    data = max_square,
    aes(x, y),
    fill = "#FFF5EB",
    color = "#FFE2C2",
    lty = 3,
    size = 0.25
    ) +
  geom_polygon(aes(x, y, fill = variable), show.legend = F) +
  geom_segment(
    aes(x = 2.2, xend = 2.2, y = 0, yend = 2),
    color = "grey75",
    size = 0.25
    ) +
  geom_segment(
      aes(x = -2.2, xend = -2.2, y = 0, yend = -2),
      color = "grey75",
      size = 0.25
    ) +
  geom_text(
    data = legend_data,
    aes(x, y, label = text),
    family = "Raleway",
    size = 1.5,
    color = "grey50"
  ) +
  labs(
    x = NULL, y = NULL
  ) +
  scale_fill_manual(values = c("#F8A88B", "#F6CA83")) +
  scale_x_continuous(limits = c(-3.5, 3.5)) +
  scale_y_continuous(limits = c(-3.5, 3.5)) +
  theme(
    rect = element_rect(fill = NA),
    text = element_text(family = "Roboto", size = 5),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )

# Draw legend plot on main plot, add titles, and save
ggdraw(main_plot) +
  draw_plot(
    legend_plot,
    x = 0.25, y = 0.675, height = 0.4, width = 0.28
    ) +
  draw_label(
    "Increased Spending on Youth Education in the US", 
    x = 0.575, y = 0.1,
    size = 18, fontfamily = "Raleway", color = "grey30"
    ) +
  draw_label(
    "The increase of public spending on elementary/secondary education and higher education from 1997 to 2016, adjusted for inflation.", 
    x = 0.575, y = 0.07,
    size = 7, fontfamily = "Raleway", color = "grey55"
    ) +
  draw_label(
    "@MaiaPelletier | #TidyTuesday | Data source: Urban Institute & Joshua Rosenberg {tidykids}", 
    x = 0.985, y = 0.5,
    size = 4, fontfamily = "Roboto", color = "grey80", angle = 90
    ) +
    ggsave(here("images", "progress", "imgs_week38", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', width = 8, height = 5.5)
  




