# Week 20: Volcanoes ------------------------------------------------------

library(tidyverse)
library(mythemes)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

# This code to tidy player data is from the awesome
# Alex Cookson (@alexcookson) who posted it on Twitter
players <- 
  vb_matches %>%
  # Add unique id to identify matches
  mutate(match_id = row_number()) %>%
  # Rename name fields for consistent format (win/lose_playernumber_fieldname)
  rename(w_p1_name = w_player1,
         w_p2_name = w_player2,
         l_p1_name = l_player1,
         l_p2_name = l_player2) %>%
  # Select only fields associated with an individual player
  select(match_id, matches("[wl]_p[12]")) %>%
  # Date class of birthdate columns causes pivot_longer() not to break
  select(-contains("birthdate")) %>%
  # Separate and pivot players based on win/lose, player number, and field name
  pivot_longer(w_p1_name:l_p2_tot_digs,
               names_to = c("win_lose", "player", "measure"),
               names_pattern = "([wl])_(p[12])_(.*)",
               values_to = "value",
               values_ptypes = list(value = "character")) %>%
  # Re-assemble data so the unit of observation is an individual player in an individual match
  pivot_wider(names_from = measure,
              values_from = value) %>%
  # Convert number fields into double or integer
  mutate_at(vars(age, tot_hitpct), as.double) %>%
  mutate_at(vars(hgt, tot_attacks:tot_errors, tot_aces:tot_digs), as.integer)

# Create a gender match id to join with player data
match_gender <- 
  vb_matches %>% 
  mutate(match_id = row_number()) %>% 
  select(match_id, gender)

# Join gender data and process for only relevant data
blocking_data <- 
  players %>%
  left_join(match_gender) %>% 
  select(-matches('tot_[^b]')) %>%
  filter(!is.na(tot_blocks) & !is.na(hgt))

# Calculate blocks per game for each player
plotting_data <- 
  blocking_data %>% 
  add_count(name, name = 'n_matches') %>% 
  group_by(name, hgt, gender, n_matches) %>% 
  summarise(n_blocks = sum(tot_blocks)) %>% 
  ungroup() %>% 
  mutate(prop = n_blocks/n_matches)

# Plot!
plotting_data %>%
  filter(prop > 0) %>% 
  mutate(gender = ifelse(gender == 'W', 'Women', 'Men')) %>% 
  ggplot(aes(hgt, prop)) +
  geom_jitter(
    aes(fill = gender, color = gender),
    alpha = 0.75,
    shape = 21,
    show.legend = FALSE,
    width = 0.5,
    size = 2
    ) +
  geom_smooth(
    method = 'lm',
    color = 'grey60',
    size = 0.5,
    se = FALSE
  ) +
  labs(
    x = NULL, 
    y = 'Blocks per match',
    title = 'Relationship between height and blocking in beach volleyball',
    subtitle = 'There is a positive correlation between a player\'s height & how much a player blocks',
    caption = '@MaiaPelletier | #TidyTuesday | Data sourced from BigTime Stats (Adam Vagner)'
    ) +
  facet_wrap(gender~., nrow = 2) +
  ylim(c(0, 7)) +
  scale_x_continuous(
    breaks = seq(65, 85, 5),
    labels = paste0(seq(65, 85, 5), ' in.')
  ) +
  scale_fill_manual(values = c("#46ACC8", "#85D4E3")) +
  scale_color_manual(values = c("#46ACC8", "#85D4E3")) +
  theme_maia() +
  theme(
    rect = element_rect(fill = "#f5f2e9"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 6)
    )

# Save plot
ggsave("Week21_BeachVolleyball.png", type = 'cairo')


