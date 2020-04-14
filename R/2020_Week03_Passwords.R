#TidyTuesday: Week 3 (Passwords) ----------------------------------------

# Load libraries used
library(tidyverse)
library(Cairo)

# Read data
passwords <- read_csv('TidyTuesday/2020/Week 3/passwords.txt')

# Remove NA values
passwords <- 
  passwords %>%
  filter(!is.na(password))

# Select top 5 most popular passwords
password_plot <- 
  passwords %>%
  group_by(category) %>%
  top_n(-5, rank) %>%
  ungroup() %>%
  mutate(
    category = str_replace(category, '-', '/'),
    category = str_to_title(category),
    category = str_replace(category, 'Password/Related', 'Password related')
    )

# Construct plot
password_plot %>%
  ggplot(aes(reorder(password, strength), strength)) +
  geom_segment(aes(
    x = reorder(password, strength),
    xend = reorder(password, strength),
    y = 0,
    yend = strength
  ), color = 'grey25') +
  geom_point(aes(color = category, size = 1 / rank)) +
  labs(
    x = NULL, 
    y = 'Relative strength', 
    caption = 'Data sourced from Information is Beautiful\n@MaiaPelletier #TidyTuesday'
    ) +
  ggtitle('Popular Passwords\' Relative Strength', subtitle = 'Top 5 most popular passwords per password category') +
  facet_wrap(category ~ ., scales = 'free_y') +
  coord_flip() +
  fishualize::scale_color_fish_d(option = 'Halichoeres_bivittatus') +
  scale_size_continuous(
    name = 'POPULARITY',
    labels = c('High', '', '', 'Low'),
    breaks = seq(1, 0.25,-0.25),
    range = c(2, 5)
  ) +
  guides(color = 'none') +
  ggthemes::theme_few() +
  theme(
    strip.background = element_rect(fill = 'black'),
    strip.text = element_text(color = 'white', size = 9, face = 'bold'),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8, face = 'italic'),
    legend.position = 'bottom',
    legend.title = element_text(size = 8, face = 'bold'),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(size = 18)
  )

# Save plot to PNG file
ggsave('Week3_PasswordsPlot.png', type = 'cairo')

