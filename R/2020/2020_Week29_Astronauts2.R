library(dplyr)
library(ggplot2)
library(ggtext)
library(gghighlight)
library(mythemes)

theme_galaxy <- function() {
  theme_maia() +
    theme(
      rect = element_rect(fill = "black"),
      text = element_text(color = "#F0F0F0")
    )
}

theme_set(theme_galaxy())

galaxy_pal <- c(
  "#05BFCBFF",
  "#8900F2",
  "#E500A4",
  "#39beff",
  "#c051ff"
)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

ids <- c(658, 1274, 242, 18, 1033)

highlighted_data <- 
  astronauts %>% 
  filter(id %in% ids)

astronauts %>%
  ggplot(aes(year_of_mission, hours_mission)) +
  geom_point(
    color = "grey90",
    fill = "grey90",
    shape = 21,
    stroke = 2,
    alpha = 0.35
  ) +
  geom_point(color = "grey90", size = 1) +
  geom_point(
    data = highlighted_data,
    aes(year_of_mission, hours_mission, color = factor(id), fill = factor(id)), 
    size = 4,
    shape = 21,
    stroke = 3,
    alpha = 0.5,
    show.legend = F
    ) +
  labs(
    x = NULL, 
    y = "Mission hours",
    title = "Women in Space",
    subtitle = '"Never be limited by other people\'s limited imaginations... - Mae Jemison"',
    caption = "@MaiaPelletier | #TidyTuesday | Source: Mariya Stavnichuk and Tatsuya Corlett"
    ) +
  annotate(
    "richtext", 
     x = 1965, y = 2000, 
     color = "white", fill = "black", family = "Arial Narrow", 
      size = 2, alpha = 0.5,
      label = "<span style='color:#05BFCBFF'>Valentina Vladimirovna Tereshkova</span><br> is the first woman to have been<br>selected as a cosmonaut (1962) and<br>is still the only woman to have gone<br> to space on a solo mission (1963)."
    ) +
  annotate(
    "richtext", 
    x = 1980, y = 3500, 
    color = "white", fill = "black", family = "Arial Narrow", 
    size = 2, alpha = 0.5,
    label = "<span style='color:#8900F2'>Sally Ride</span> was the first American<br>woman in space & the youngest<br>Americanto have traveled to<br>space (age 32)."
  ) +
  annotate(
    "richtext", 
    x = 1993, y = 5500, 
    color = "white", fill = "black", family = "Arial Narrow", 
    size = 2, alpha = 0.5,
    label = "<span style='color:#E500A4'>Mae Jemison</span> became the first Black<br>woman to ever go to space<br>when she served as a mission specialist<br>aboard the Space Shuttle Endeavour."
  ) +
  annotate(
    "richtext", 
    x = 2007, y = 6500, 
    color = "white", fill = "black", family = "Arial Narrow", 
    size = 2, alpha = 0.5,
    label = "<span style='color:#39beff'>Peggy Whitson</span> holds the records<br>for the oldest woman<br>spacewalker, and the record<br>for total spacewalks<br>by a woman."
  ) +
  annotate(
    "richtext", 
    x = 2015, y = 9000, 
    color = "white", fill = "black", family = "Arial Narrow", 
    size = 2, alpha = 0.5,
    label = "<span style='color:#c051ff'>Christina Koch</span> broke the<br>record for most continuous time<br>in space by a woman.<br>She returned from space in<br>February 2020, just in<br>time for COVID-19."
  ) +
  scale_color_manual(values = galaxy_pal) +
  scale_fill_manual(values = galaxy_pal) +
  scale_y_continuous(position = "left",
                     breaks = seq(0, 10000, 2500),
                     labels = paste0(seq(0, 10, 2.5), "k")) +
  gghighlight(sex == "female",
              unhighlighted_params = list(color = "grey20", fill = "grey20", size = 1, stroke = 0),
              use_direct_label = FALSE) +
  theme(
    axis.text.x = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
    plot.caption = element_text(size = 6, hjust = 1.2)
  ) +
  ggsave(paste0("imgs_week29_3/astronauts_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), type = 'cairo')

