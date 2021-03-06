
# Week 33: ATLA -----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(mythemes)
library(here)
library(ggstream)
library(ggforce)
library(ggtext)
library(ggimage)
library(cowplot)

theme_set(theme_maia())

extrafont::loadfonts("win")

# Read in data with {tidytuesdayR}
tuesdata <- tt_load(2020, week = 33)

# Correct missing IMDB score
avatar <- 
  tuesdata$avatar %>% 
  mutate(imdb_rating = ifelse(book_num == 1 & chapter_num == 20, 9.7, imdb_rating))

# List of main characters
main_chars <- c("Aang", "Katara", "Sokka", "Zuko", "Iroh", "Azula", "Toph")

# Palettes for plots
character_palette <- c("#ffcc33", "#0047ab", "#a2caed", "#a10000", "#c24841", "#ff9933", "#015e05")
element_palette <- c("#a10000", "#015e05", "#0047ab")

# Drawings of the element symbols
element_imgs <- 
  tibble(
    book = c("Water", "Earth", "Fire"),
    img = c(
      here("images", "atla", "WaterSymbol.png"),
      here("images", "atla", "EarthSymbol.png"),
      here("images", "atla", "FireSymbol.png")
    )
  )

# Define arrow for circle plots
arrow <-
  grid::arrow(
    angle = 30,
    length = unit(0.1, "lines"),
    ends = "last",
    type = "closed"
  )

# Wrangling data --------------------------------------------------------------------

avatar_tidy <-
  avatar %>% 
  group_by(chapter) %>% 
  mutate(imdb_rating = imdb_rating/10) %>% 
  ungroup() %>% 
  distinct(book, chapter, imdb_rating, chapter_num, book_num) %>% 
  mutate(
    id = row_number(),
    book = forcats::fct_relevel(book, c("Fire", "Earth", "Water"))
  ) %>% 
  group_by(book) %>% 
  mutate(
    book_median = median(id),
    book_rating_avg = mean(imdb_rating),
    book_start = min(id),
    book_end = max(id),
    ) %>% 
  ungroup()

avatar_character <- 
  avatar %>% 
  filter(character != "Scene Description") %>% 
  group_by(chapter) %>% 
  add_count(character, sort = TRUE, name = "n_lines") %>% 
  ungroup() %>% 
  distinct(book_num, book, chapter, character, n_lines) %>% 
  left_join(
    avatar %>% 
      distinct(chapter) %>% 
      mutate(chapter_id = row_number())
  ) %>% 
  filter(character %in% main_chars) %>% 
  mutate(
    character = forcats::fct_relevel(character, main_chars),
    book = forcats::fct_relevel(book, c("Fire", "Earth", "Water"))
    ) %>% 
  group_by(book) %>% 
  mutate(
    book_median = mean(chapter_id),
    book_start = min(chapter_id),
    book_end = max(chapter_id)
  ) %>% 
  ungroup() %>% 
  left_join(element_imgs)

episode_data <- 
  bind_rows(
    avatar_tidy %>%
      group_by(book) %>% 
      top_n(1, imdb_rating),
    avatar_tidy %>%
      group_by(book) %>% 
      top_n(-1, imdb_rating)
  ) %>%
  ungroup %>% 
  mutate(
    labels = case_when(
      str_detect(chapter, "Part 3") ~ "Sozin's Comet\n(Part 3 & Part 4)",
      str_detect(chapter, "Part 4") ~ "",
      str_detect(chapter, "Part 2") ~ "The Siege of the North\n(Part 2)",
      TRUE ~ chapter
    ),
    labels = ifelse(labels == "", NA, labels)
  )

# Plots -------------------------------------------------------------------

# Main circle plot of individual episode ratings
circle_ratings_plt <-
  ggplot(avatar_tidy) +
  geom_link(
    aes(x = id, xend = id, y = 0, yend = imdb_rating, color = book, alpha = imdb_rating),
    arrow = arrow,
    size = 0.35,
    show.legend = F
  ) +
  geom_text(
    data = avatar_tidy %>% distinct(book, book_median),
    aes(x = book_median, y = 0, label = book),
    family = "Slayer",
    color = "grey25",
    size = 2,
    vjust = -1.15
  ) +
  geom_text(
    data = data.frame(x = 61, y = 1, text = "10.0"),
    aes(x, y, label = text),
    family = "Slayer",
    color = "grey25",
    size = 1.5,
    hjust = -1.5
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  coord_polar(theta = "y", start = 1.57) +
  scale_alpha(range = c(0.05, 0.8)) +
  scale_x_continuous(limits = c(-25, 61)) +
  scale_y_continuous(limits = c(0, 1.01)) +
  scale_color_manual(values = element_palette) +
  theme(
    text = element_text(family = "Slayer"),
    rect = element_rect(fill = "#E4DDD2"),
    plot.margin = margin(0, 250, 65, -10),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Lines of dialogue stream plot
character_lines_stream <- 
  avatar_character %>%
  ggplot(aes(chapter_id, n_lines)) +
  geom_segment(
    aes(x = book_start, xend = book_end, y = 90, yend = 90),
    color = "grey45"
  ) +
  geom_stream(
    aes(fill = character),
    method = "density", color = "#E4DDD2", alpha = 0.5
    ) +
  geom_image(
    data = avatar_character %>% distinct(book, book_num, book_median, img),
    aes(x = book_median, y = 130, image = img, color = book),
    alpha = 0.5, size = 0.15
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(breaks = c(0), limits = c(-90, 170), labels = c("Lines\nof\ndialogue")) +
  scale_fill_manual(values = character_palette, name = NULL) +
  scale_color_manual(values = c("#015e05", "#a10000", "#0047ab"), guide = guide_none()) +
  theme_void() +
  theme(
    text = element_text(family = "Slayer", size = 4),
    plot.margin = margin(0, 45, 0, 10),
    axis.text.y = element_text(family = "Slayer", color = "grey10", hjust = 0.5, size = 4.25),
    legend.position = c(1, 0.4),
    legend.direction = "vertical",
    legend.key.size = unit(0.5, "lines")
  ) +
  ggsave(here("images", "progress", "imgs_week33", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')

# Average book rating circle plots
avg_book_rating <- 
  avatar_tidy %>% 
  mutate(book = forcats::fct_rev(book)) %>% 
  ggplot() +
  geom_link(
    aes(x = 1, xend = 1, y = book_rating_avg, yend = 1),
    size = 0.35,
    lty = 3,
    color = "grey45",
    show.legend = F
  ) +
  geom_segment(aes(x = 0.75, xend = 1.25, y = mean(imdb_rating), yend = mean(imdb_rating)),
               size = 0.15, color = "grey35") +
  geom_link(
    aes(x = 1, xend = 1, y = 0, yend = book_rating_avg, color = book),
    arrow = grid::arrow(length = unit(0.2, "lines"), type = "closed"),
    size = 0.25,
    alpha = 0.25,
    show.legend = F
  ) +
  geom_text(
    data = data.frame(x = 1, y = 1, text = "10.0"),
    aes(x, y, label = text), 
    family = "Slayer",
    size = 1.25, hjust = -0.5
  ) +
  facet_wrap(book~.) +
  coord_polar(theta = "y", start = 1.57) +
  xlim(c(0, 1.5)) +
  ylim(c(0, 1)) +
  theme_void() +
  theme(
    strip.text = element_blank()
    ) +
  scale_color_manual(values = rev(element_palette))

# Best and worst episodes from each book plot
best_worst_episodes <- 
  avatar_tidy %>% 
  ggplot() +
  geom_segment(
    aes(x = id, xend = id, y = book_rating_avg, yend = imdb_rating),
    arrow = grid::arrow(length = unit(0.1, "lines"), type = "closed"),
    color = "grey",
    size = 0.15
  ) +
  geom_segment(
    aes(x = book_start, xend = book_end, y = book_rating_avg, yend = book_rating_avg),
    color = "grey35",
    size = 0.25
  ) +
  geom_segment(
    data = episode_data,
    aes(x = id, xend = id, y = book_rating_avg, yend = imdb_rating, color = book),
    arrow = grid::arrow(length = unit(0.25, "lines"), type = "closed"),
    size = 0.25,
    alpha = 0.65
  ) +
  geom_text(
    data = episode_data,
    aes(x = id, y = imdb_rating, label = labels),
    size = 1.25,
    vjust = "outward",
    color = "grey10",
    family = "Slayer"
  ) +
  guides(color = guide_none()) +
  xlim(c(1, 70)) +
  scale_y_continuous(limits = c(0.7, 1), breaks = c(0.75, 0.95), labels = c("7.5", "9.5")) +
  scale_color_manual(values = element_palette) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 3, family = "Slayer")
  )

# Layer everything & save final image
ggdraw(circle_ratings_plt) +
  draw_image(here("images", "atla", "appa.png"), height = 0.275, width = 0.275, x = 0.65, y = 0.025) +
  draw_plot(character_lines_stream, height = 0.45, width = 0.5, x = 0.525, y = 0.56) +
  draw_plot(best_worst_episodes, height = 0.275, width = 0.35, x = 0.615, y = 0.28) +
  draw_plot(avg_book_rating, height = 0.3, width = 0.35, x = 0.1, y = 0) +
  draw_label("A:TLA", fontfamily = "Slayer", size = 26, x = 0.625, y = 0.4- 0.2, color = "grey10") +
  draw_label("A Book by Book Overview", fontfamily = "Slayer", size = 8, x = 0.85, y = 0.2, color = "grey35") +
  draw_label("Avatar: the Last Airbender is regarded as one of the best cartoons ever made \n (maybe even amoung the best shows ever made), and its IMDB ratings reflect that. \n\n The show only improved over its 3 season run as the memorable characters traversed their \n individual hero's journeys, with Book 3: Fire becoming the best critically received of the 3 books.",
             hjust = 0.5, x = 0.75, y = 0.1, fontfamily = "Montserrat", size = 6, color = "grey35") +
  draw_label("@MaiaPelletier | #TidyTuesday | Data: {appa} by Avery Robbins", color = "grey50", fontfamily = "Montserrat", size = 5,
             x = 0.16, y = 0.03) +
  ggsave(here("images", "progress", "imgs_week33", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')
