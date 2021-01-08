# Week 22: Cocktails ------------------------------------------------------

# load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(mythemes)
library(cowplot)
library(ggimage)

# set personal theme
theme_set(theme_maia())

# load data
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# list of african countries to filter out other countries in data
african_countries <- c(
  "Ethiopia",
  "Kenya",
  "Burundi",
  "Rwanda",
  "Malawi",
  "Zambia",
  "Mauritius",
  "Cote d'Ivoire"
)

# tidy & get avg scores
coffee_scores <- 
  coffee_ratings %>% 
  mutate(country_of_origin = ifelse(country_of_origin == "Cote d?Ivoire", "Cote d'Ivoire", country_of_origin)) %>% 
  filter(country_of_origin %in% african_countries) %>% 
  select(country_of_origin, total_cup_points, 21:31) %>% 
  group_by(country_of_origin) %>% 
  summarise_if(is.numeric, mean)

# avg category scores
coffee_scores_long <- 
  coffee_scores %>% 
  pivot_longer(
    cols = aroma:moisture,
    names_to = "category",
    values_to = "score"
  ) %>% 
  mutate(category = str_to_title(category),
         category = str_replace(category, "_", " ")) %>% 
  filter(!category %in% c("Moisture", "Cupper points")) %>% 
  group_by(country_of_origin) %>% 
  mutate(avg_score = mean(score))

# make overall score plot
p1 <- 
  coffee_scores %>% 
  mutate(max = 100) %>% 
  ggplot(aes(reorder(country_of_origin, total_cup_points), total_cup_points)) +
  geom_segment(
    aes(x = reorder(country_of_origin, total_cup_points), 
        xend = country_of_origin, 
        y = 0, yend = 100),
    color = "grey70",
    lty = 2
    ) +
  geom_segment(
      aes(x = country_of_origin, xend = country_of_origin, y = 0, yend = total_cup_points),
      color = "grey50"
    ) +
  geom_point(shape = 21,
             fill = "#F0F0F0",
             color = "grey25") +
  geom_point(aes(country_of_origin, max),
             size = 2,
             color = "grey25") +
  labs(
    x = NULL,
    y = NULL,
    title = ""
  ) +
  geom_text(
    aes(label = round(total_cup_points)),
    size = 2.5,
    vjust = -1,
    family = "Arial Narrow"
  ) +
  scale_y_continuous(
    limits = c(0, 100), 
    position = "right", 
    breaks = c(0, 50, 100)
  ) +
#  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  coord_flip() +
  theme(
    rect = element_rect(fill = "#fcfcfc"),
    axis.text.x = element_blank(),
    axis.title.x.top = element_blank()
  )
  
# make category plot
p2 <-
  coffee_scores_long %>% 
  ungroup() %>% 
  mutate(country_of_origin = factor(country_of_origin),
         country_of_origin = forcats::fct_reorder(country_of_origin, -total_cup_points)) %>% 
  ggplot() +
  geom_hline(
    aes(yintercept = avg_score), 
    color = "grey50",
    size = 0.25,
    alpha = 0.75
    ) +
  geom_segment(
      aes(x = reorder(category, score), 
          xend = category, 
          y = 0, yend = 10),
      color = "grey70",
      lty = 2
    ) +
  geom_segment(
      aes(x = category, xend = category, y = 0, yend = score),
      color = "grey50"
    ) +
  geom_point(aes(category, 10),
             size = 2,
             color = "grey25") +
  geom_point(
    aes(reorder(category, score), score),
    shape = 21,
    fill = "#F0F0F0",
    color = "grey25"
    ) +
  labs(
    x = NULL,
    y = NULL,
    title = ""
  ) +
  scale_x_discrete(
    position = "bottom"
  ) +
  scale_y_continuous(
    limits = c(0, 10), 
    position = "right", 
    breaks = c(10)
    ) +
  coord_flip() +
  facet_wrap(country_of_origin~., ncol = 2) +
  theme(
    rect = element_rect(fill = "#fcfcfc"),
     axis.text.x = element_text(size = 6),
    # axis.title.x.top = element_blank(),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8),
    strip.placement = "outside"
    )


# patch together with {patchwork} & add labels
p1 + p2 +
  plot_annotation(
    title = "African Originated Coffee Ratings",
    caption = "@MaiaPelletier | #TidyTuesday | James LeDoux (Coffee Quality Database)"
    ) &
  theme(
    rect = element_rect(fill = "#F2EBE4"),
    text = element_text("Arial Narrow"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, -10, 0), size = 14),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 5)
    ) -> p

# save plot
p +
 ggsave(paste0("imgs_week28/coffee_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), type = 'cairo')

