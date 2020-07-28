
# Week 31: Penguins! ------------------------------------------------------

# Load used libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(mythemes) # My personal themes package
library(here)


# Set up ------------------------------------------------------------------

# Set my personal theme
theme_set(theme_maia())

# Load system fonts
extrafont::loadfonts("win")

# Colour palette inspired by the new Seattle Kraken logo/jersey!
kraken_pal <- c(
  dark = "#001628",
  dark_blue = "#44748c",
  medium_blue = "#639FB6",
  light_blue = "#96D8D8",
  red = "#C8102E",
  off_white = "#F5F5F5"
)

# Read in data
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

# Find median mass of each species (too lazy to change the variable name)
mean_body_mass <-
  penguins %>%
  na.omit() %>% 
  group_by(species) %>%
  summarise(mean_mass = median(body_mass_g))


# Plotting ----------------------------------------------------------------

# Construct plot
p <-
  penguins %>% 
  na.omit() %>% 
  ggplot(aes(body_mass_g)) +
  geom_vline(
    data = mean_body_mass, 
    aes(xintercept = mean_mass), 
    color = kraken_pal["off_white"],
    lty = 2
  ) +
  geom_density(
    aes(fill = species, color = species), 
    alpha = 0.8, 
    outline.type = "upper", 
    adjust = 1.5,
    show.legend = F
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Body Mass of Palmer Penguins",
    subtitle = "Gentoo penguins tend to skew on the chonkier side as compared to their neighbours, the Adélie & Chinstrap penguins.",
    caption = "@MaiaPelletier | #TidyTuesday | Source: Gorman, Williams and Fraser, 2014 | Artwork by @allison_horst"
  ) +
  facet_wrap(species~., ncol = 1) +
  scale_x_continuous(labels = paste0(3:6, " kg"), position = "top") +
  scale_fill_manual(values = c("#44748c", "#639FB6", "#96D8D8")) +
  scale_color_manual(values = c("#44748c", "#639FB6", "#96D8D8")) +
  theme(
    rect = element_rect(fill = kraken_pal["dark"]),
    text = element_text(color = kraken_pal["off_white"],
                        family = "Lora"),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(size = 6),
    strip.text = element_blank(),
    axis.text.y = element_blank()
  ) +
  ggsave(here("images", "progress", "imgs_week31", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')

# Draw on custom images and labels
# Amazing penguin designs by the incredible @allison_horst
ggdraw(p) +
  draw_image(here("images", "penguins", "adelie_penguin_blue.png"), x = 0.8, y = 0.61, height = 0.1, width = 0.1) +
  draw_label("Adélie", x = 0.85, y = 0.73, size = 10, color = kraken_pal["off_white"], fontfamily = "Lora") +
  draw_image(here("images", "penguins", "chinstrap_penguin_blue.png"), x = 0.8, y = 0.34, height = 0.1, width = 0.1) +
  draw_label("Chinstrap", x = 0.85, y = 0.455, size = 10, color = kraken_pal["off_white"], fontfamily = "Lora") +
  draw_image(here("images", "penguins", "gentoo_penguin_blue.png"), x = 0.1, y = 0.07, height = 0.1, width = 0.1) +
  draw_label("Gentoo", x = 0.155, y = 0.185, size = 10, color = kraken_pal["off_white"], fontfamily = "Lora") +
  ggsave(here("images", "progress", "imgs_week31", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')







