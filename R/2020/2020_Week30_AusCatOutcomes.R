# Week 30: Australian Cat Outcomes --------------------------------------------

# Load libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(wesanderson)
library(ggtext)
library(cowplot) # once again, {cowplot} saves the damn day
library(here)
library(mythemes) # my personal theme package

# Set my custom theme
theme_set(theme_maia())

# Load fun fonts to use for plot
extrafont::loadfonts(device = "win")

# Read data
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

cat_outcomes <- 
  animal_outcomes %>% 
  filter(animal_type == "Cats") %>% 
  select(year, animal_type, outcome, outcome_total = Total) %>% 
  mutate(
    outcome = case_when(
      outcome %in% c("Rehomed", "Reclaimed") ~ "Rescued",
      outcome == "Euthanized" ~ "Euthanized",
      TRUE ~ "Other"
    ),
    outcome = factor(outcome),
    outcome = fct_relevel(outcome, "Euthanized", "Rescued", "Other")
  ) %>% 
  group_by(year, outcome) %>% 
  summarise(outcome_total = sum(outcome_total)) %>% 
  mutate(outcome_percent = outcome_total/sum(outcome_total))

# Create plot
p <- 
  cat_outcomes %>%
  ggplot(aes(year, outcome_percent)) +
  geom_line(aes(color = outcome), size = 1) +
  labs(
    title = "Adopt, don't shop!",
    subtitle = "The rate of cats being humanely euthanized in Australia has decreased substantially over the last decade.",
    x = NULL,
    y = NULL,
    caption = "@MaiaPelletier | #TidyTuesday | Data source: RSPCA"
  ) +
  scale_y_continuous(
    limits = c(0, 0.75),
    breaks = c(0, 0.25, 0.5, 0.75),
    labels = scales::percent_format()
  ) +
  scale_color_manual(
    values = wes_palette("Royal2")[c(5, 3, 4)],
    name = NULL
    ) +
  theme(
    rect = element_rect(fill = "#fff1eb"),
    text = element_text(family = "Lato"),
    plot.title = element_text(family = "Patrick Hand SC", 
                              hjust = 0.5, 
                              size = 26,
                              margin = margin(5, 0, 15, 0)),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 10),
    plot.caption = element_text(family = "Lato")
  )

# Draw on images & annotation
ggdraw(p) +
  draw_image(here("images", "cats", "cat_bgrd.png"), height = 0.2, width = 0.2, x = 0.75, y = 0.63) + 
  draw_image(here("images", "cats", "happy_cat.png"), height = 0.075, width = 0.075, x = 0.255, y = 0.8855) +
  draw_image(here("images", "cats", "sad_cat.png"), height = 0.08, width = 0.08, x = 0.7, y = 0.885) +
  geom_text(data = data.frame(x = 0.855, y = 0.72, label = "66% of the\nRSPCA's cats\nwere adopted\nin 2018!"), 
            aes(x, y, label = label), 
            color = "grey25", family = "Lato", size = 3
            ) +
ggsave(paste0("imgs_week30/animaloutcomes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), type = 'cairo')


  