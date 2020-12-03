# Week 49: Toronto Shelters -------------------------------------------

# Load libraries
library(tidytuesdayR)
library(here)         
library(tidyverse)
library(sf)
library(rvest)
library(pjocolors)
library(lubridate)
library(cowplot)

# Create directory to save all progress plots
dir.create(here("images", "progress", "imgs_week49"))

# Load custom fonts
extrafont::loadfonts(device = "win")

# Load data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 49)
shelters <- tuesdata$shelters

# Scrape Toronto's FSAs (first 3 characters of postal codes) off wikipedia
to_fsa <- 
  "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_M" %>% 
  read_html() %>% 
  html_node(".wikitable") %>% 
  html_table() %>%
  distinct(fsa = `Postal Code`, borough = Borough)

# Calculate the average % of capacity currently occupied at shelters per FSA
occupancy_per_fsa <- 
    shelters %>%
    filter(
      !is.na(occupancy), occupancy != 0,
      !is.na(capacity), capacity != 0,
      year(occupancy_date) == 2019
      ) %>% 
    mutate(fsa = str_sub(shelter_postal_code, 1, 3)) %>% 
    group_by(fsa) %>% 
    summarise(`%capacity` = median(occupancy/capacity))
    
# Read in FSA shapefile (downloaded from Statistics Canada) and join with shelter data
fsa <- 
  here("data", "FSA", "lfsa000b16a_e.shp") %>% 
  st_read() %>% 
  filter(CFSAUID %in% to_fsa$fsa) %>% 
  left_join(occupancy_per_fsa, by = c("CFSAUID" = "fsa"))

# Constuct plot
p <- 
  fsa %>% 
  ggplot() +
  geom_sf(aes(fill = `%capacity`), color = "white") +
  scale_fill_pjo(
    palette = "SeaOfMonsters",
    discrete = FALSE,
    na.value = "grey95",
    labels = scales::percent_format(accuracy = 1)
  ) +
 guides(
    fill = guide_colourbar(
      barwidth = 10,
      barheight = 0.5,
      title = NULL,
      ticks = FALSE,
      direction = "horizontal",
      label.theme = element_text(size = 8, family = "Lato", color = "grey40")
      )
    ) +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = "#fef9f6", color = NA),
    plot.margin = margin(100, 50, 20, 50),
    legend.position = c(0.825, 1),
    legend.margin = margin(0, 0, 80, 0),
  )
  
# Add text to plot & save
ggdraw(p) +
  draw_label("Toronto\nShelter\nOccupancy", x = 0.3, y = 0.875,
             fontfamily = "Lato", size = 20, hjust = 1, fontface = "bold", color = "grey20") +
  draw_label("The average percentage of\noccupants compared to the\ncapcity of shelters in the City\nof Toronto, evaluated by postal\nregions (first 3 characters of\nthe shelter's postal code).", 
             x = 0.32, y = 0.87, color = "grey60",
             fontfamily = "Lato", size = 8, hjust = 0) +
  draw_label("NORTH YORK", x = 0.45, y = 0.625, 
             fontfamily = "Lato", fontface = "bold", size = 8, color = "grey80") +
  draw_label("SCARBOROUGH", x = 0.875, y = 0.515, 
             fontfamily = "Lato", fontface = "bold", size = 8, color = "grey80") +
  draw_label("DOWNTOWN", x = 0.65, y = 0.175, 
             fontfamily = "Lato", fontface = "bold", size = 8, color = "grey80") +
  draw_label("ETOBICOKE", x = 0.125, y = 0.25, 
             fontfamily = "Lato", fontface = "bold", size = 8, color = "grey80") +
  draw_label("@MaiaPelletier | #TidyTuesday | Data source: City of Toronto Open Data Portal {opendatatoronto}", 
             x = 0.725, y = 0.02, 
             fontfamily = "Lato", size = 6, color = "grey80") +
  ggsave(here("images", "progress", "imgs_week49", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo') 



