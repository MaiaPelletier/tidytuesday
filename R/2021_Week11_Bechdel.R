# 2021 - Week 11 - Bechdel Test ---------------------------------------------------

# Load libraries
library(tidyverse)
library(here)
library(glue)
library(ggtext)

# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_week11"))

# load fonts
extrafont::loadfonts(device = "win")

# Load data ---------------------------------------------------------------

raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


bechdel_palette <- c(
  "#7C272E", # ruby red 
  "#CE646C", # pink
  "#EDAD9C", # pale pink
  "#36996B"  # green cyan
  )


# Data transformation -----------------------------------------------------
  
tidy_bechdel <-
  raw_bechdel %>% 
  left_join(movies, by = "imdb_id") %>% 
  select(year = year.x, imdb_id, title = title.x, rating, imdb_rating) %>% 
  arrange(desc(imdb_rating), title) %>% 
  top_n(20, imdb_rating) %>% 
  mutate(
    title = str_replace(title, ": ", ":<br>"),
    title = ifelse(str_detect(title, "Star Wars"), "Star Wars: Episode V<br>The Empire Strikes Back", title)
    ) %>% 
  mutate(
    y = 1,
    movie_rank = row_number(),
    title_label = glue("<span style='font-weight:bold;'>{title}</span><br><span style = 'color:#808080;font-size:4pt;font-weight:normal;font-family:Lato'>IMBD rating: {imdb_rating}</span>"),
    title_label = fct_reorder(title_label, movie_rank),
    rating = map(rating, function(x) if (x > 0) seq(1, x) else 0)
    ) %>% 
  unnest(rating) %>% 
  mutate(
    rating_label = case_when(
      rating == 0 ~ "Bechdel fail",
      rating == 1 ~ "2 women with names...",
      rating == 2 ~ "who talk to each other...",
      rating == 3 ~ "about something other than a man."
    ),
    rating_label = fct_reorder(rating_label, rating)
  )


# Plot construction -------------------------------------------------------

ggplot(tidy_bechdel, aes(rating, y)) +
  geom_col(aes(fill = rating_label), width = 1) +
  labs(
    title = "The Bechdel Test",
    subtitle = "The Bechdel test was developed by queer cartoonist Alison Bechdel in '85. A movie passes the test if it contains 2 female<br>characters with names, who talk to each other, about something other than a man. <span style = 'color:#7C272E;'>Some movies fail on all 3 counts.</span><br>This viz shows how some of the top rated movies on IMBD fare on the Bechdel scale.",
    caption = "Viz: @MaiaPelletier | Data: BechdelTest.com"
  ) +
  scale_fill_manual(
    values = bechdel_palette,
    breaks = c("2 women with names...", "who talk to each other...", "about something other than a man."),
    guide = guide_legend(title = NULL, direction = "vertical")
    ) +
  facet_wrap(title_label~., ncol = 6) +
  coord_polar(theta = "y") +
  theme_void(base_family = "Lora") +
  theme(
    plot.background = element_rect(fill = "#F0FAF5", color = NA),
    plot.margin = margin(10, 20, 5, 20),
    plot.title = element_text(size = 28, face = "bold", margin = margin()),
    plot.subtitle = element_markdown(family = "Lato", size = 4, margin = margin(b = 7.5), color = "#808080"),
    plot.caption = element_text(family = "Lato", size = 3, color = "#808080"),
    strip.text = element_markdown(size = 5, face = "bold"),
    legend.position = c(0.81, 1.115),
    legend.margin = margin(b = 10),
    legend.text = element_text(size = 7),
    legend.key.height = unit(2, "points")
    ) +
  ggsave(here("images", "progress", "imgs_2021_week11", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')

  
