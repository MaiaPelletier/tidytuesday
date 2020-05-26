# Week 22: Cocktails ------------------------------------------------------

## Load libraries used
library(tidyverse) # All the fun tidy functions
library(mythemes)  # My personal ggplot themes
library(cowplot)   # Extra features for ggplot 
library(ggimage)   # For layers on plots

## Load fonts
extrafont::loadfonts(device = 'win')

## Read in data
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

## Find top 5 most used cocktail glasses
top_glasses <- 
  cocktails %>% 
  mutate(glass = str_to_lower(glass)) %>% 
  count(glass, sort = TRUE) %>% 
  top_n(5)

## An empty plot base to draw the images of the glasses onto
base <- 
  ggplot(top_glasses) +
  theme_void() +
  theme(
    rect = element_rect(fill = "#F5F5ED", 
                        color = NULL,
                        linetype = 0)
  )


# The base plot in front of the glasses (the data labels, axis, titles, etc.)
p <- 
  top_glasses %>% 
  mutate(glass = str_remove(glass, ' glass'),
         glass = str_to_sentence(glass)) %>% 
  ggplot(aes(reorder(glass, -n), n)) +
  geom_label(aes(label = n), 
             fill = "#DCDCD5", 
             color = "#6A6A68", 
             vjust = 1,
             size = 5, 
             alpha = 0.9,
             family = 'Lora') +
  labs(x = NULL, 
       y = "Number of recipes used in",
       title = "Most Commonly Used Glasses in Cocktail Recipes",
       caption = "@MaiaPelletier | #TidyTuesday") +
  ylim(c(0, 600)) +
  theme_maia() +
  theme(
    rect = element_rect(fill = NA, color = NA, linetype = 0),
    plot.title = element_text(family = 'Yeseva One', hjust = 0.5, color = "#6A6A68"),
    plot.caption = element_text(margin = margin(4, 0, 0, 0)),
    axis.text.x = element_text(family = 'Lora', size = 12, color = "#6A6A68"),
    axis.text.y = element_blank(),
    axis.title.y = element_text(family = "Lora", size = 8)
  )

# Constructing the layers and saving the plot
ggdraw(base) +
  draw_image("images/glasses/cocktail.png", hjust = 0, x = -0.355, y = 0.12, height = 0.75) +
  draw_image("images/glasses/collins.png", hjust = 0, x = -0.17, y = 0.12, height = 0.7) +
  draw_image("images/glasses/highball.png", hjust = 0, x = 0.008, y = 0.12, height = 0.64) +
  draw_image("images/glasses/oldfashioned.png", hjust = 0, x = 0.19, y = -0.195, scale = 0.38) +
  draw_image("images/glasses/shot.png", hjust = 0, x = 0.37, y = 0.12, height = 0.25) +
  draw_plot(p) +
ggsave(paste0("imgs_week22/cocktail_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), type = 'cairo', width = 8.5, height = 4)

