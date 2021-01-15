# (2021 - week 3 - art collections) ----------------------------------

#### set up ####

# Load libraries
library(tidyverse)
library(here)
library(colorfindr)
library(glue)
library(cowplot)

# create directory to save all progress plots
dir.create(here("images", "progress", "imgs_2021_week03"))

# load fonts
extrafont::loadfonts(device = "win")

#### data cleaning + manipulation ####

# read data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

# count the artists works & randomly assign an x/y value

set.seed(44)
top_artists <- 
  artwork %>% 
  count(artist, sort = T) %>% 
  filter(!str_detect(artist, "British")) %>% 
  mutate(
    x = map_dbl(n, function(n) runif(1)),
    y = map_dbl(n, function(n) runif(1)),
    top_artist = ifelse(row_number() <= 10, "yes", "no"),
    artist_rank = row_number()
    )

#### plot creation ####

# Use {colorfindr} to generate a palette from the kandinsky piece
# make_kandinsky_palette <- 
#   get_colors("https://www.phaidon.com/resource/kandinsky-circles.jpg") %>% 
#   make_palette(n = 15)
                

# define the kandinsky palette with the desired values
kandinsky_palette <- c(
  Dark = make_kandinsky_palette[2],
  Grey = make_kandinsky_palette[3],
  make_kandinsky_palette[4],
  make_kandinsky_palette[11],
  make_kandinsky_palette[6],
  make_kandinsky_palette[7],
  make_kandinsky_palette[9]
)


# main plot building
p <- 
  top_artists %>% 
  ggplot() +
  geom_point(
    data = top_artists %>% filter(top_artist == "no"),
    aes(x, y, size = n, fill = n), 
    alpha = 1, 
    color = kandinsky_palette[1], 
    shape = 21, 
    show.legend = F
  ) +
  geom_point(
    data = top_artists %>% filter(top_artist == "yes"),
    aes(x, y, size = n, color = top_artist), 
    alpha = 0.55, 
    shape = 21, 
    fill = kandinsky_palette[3], 
    show.legend = F
    ) +
  geom_point(
    data = top_artists %>% filter(top_artist == "yes"),
    aes(x, y, size = n), 
    shape = 1, 
    color = "#FDF8D7", 
    stroke = 2
  ) +
  geom_text(
    data = top_artists %>% filter(top_artist == "yes"),
    aes(x, y, label = artist_rank),
    color = "white", 
    family = "Montserrat SemiBold", 
    size = 2
  ) +
  labs(
    title = "TATE GALLERY ARTISTS"
  ) +
  scale_size(range = c(1, 50), guide = guide_none()) +
  scale_fill_gradientn(colours = rev(kandinsky_palette[-c(1, 2)])) +
  theme_void(base_family = "Montserrat") +
  theme(
    plot.background = element_rect(fill = "#F2F1EF", color = NA),
    panel.background = element_rect(fill = kandinsky_palette[1], color = "#403F3E", size = 5),
    plot.margin = margin(10, 120, 10, 20),
    plot.title = element_text(family = "Montserrat Black", size = 24, margin = margin(0, 0, 10, 0))
    )

# top 10 plot creation
top_artists_p <-
  top_artists %>% 
    top_n(10, n) %>% 
    left_join(artists, by = c("artist" = "name")) %>%
    mutate(
      artist = case_when(
      artist_rank == 1 ~ "Turner, Joseph M.W.",
      artist_rank == 3 ~ "Moore, Henry",
      TRUE ~ artist
      ),
      n = scales::comma(n, accuracy = 1)
    ) %>% 
    mutate(artist_label = glue("{artist_rank}. {artist} ({dates}) - {n} pieces")) %>% 
    ggplot(aes(x = 1, y = -artist_rank)) +
    geom_text(
      aes(label = artist_label), 
      family = "Montserrat", 
      hjust = 0.5,
      size = 1.2
      ) +
    labs(
      title = "TOP TEN ARTISTS"
    ) +
    theme_void(base_family = "Montserrat SemiBold") +
    theme(
      panel.background = element_rect(fill = "#F9F9F9", color = "#403F3E", size = 1),
      plot.margin = margin(5, 50, 5, 50),
      plot.title = element_text(hjust = 0.5, size = 8, margin = margin(0, 0, 5, 0))
    )
  
plot_expl <- "Each circle represents\nan artist who has at least one\npiece owned by Tate.\n\n The British School was\nexcluded to represent\nindividual artists.\n\nThe area of each circle is\nproportional to the\nnumber of works by\nthe artist owned by Tate.\n\nThe outlined dark blue\ncircles indicate the ten\nartists with the most\nworks owned by Tate.\n\nViz inspired by\nSeveral Circles (1926)\nby Wassily Kandinsky."
caption <- "@MaiaPelletier | Data source: Tate Art Museum"

# combine plot layers & save
ggdraw(p) +
  draw_plot(top_artists_p, height = 0.4, width = 0.4, x = 0.675, y = 0.05) +
  draw_label(plot_expl, x = 0.875, y = 0.7,
             size = 5, fontfamily = "Montserrat Medium", color = "#403F3E") +
  draw_label(caption, x = 0.02, y = 0.2,
             size = 5, fontfamily = "Montserrat", color = "grey55", angle = 90) +
  ggsave(here("images", "progress", "imgs_2021_week03", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')


 
