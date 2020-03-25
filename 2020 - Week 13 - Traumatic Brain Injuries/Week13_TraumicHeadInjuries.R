
# March 24 ----------------------------------------------------------------

# Load libraries used
library(tidyverse)  # All the fun tidy functions!
library(GGally)     # Creates plot matrices for data explorin
library(Cairo)      # Anti-aliasing to combat shitty Windows graphics
library(patchwork)  # To combine plots in a seamless way
library(fishualize) # Pretty color palettes based on fish colors

# Read in data
tbi_age <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')


# Explore data ------------------------------------------------------------

skimr::skim(tbi_age) # Skim data to inspect
ggpairs(tbi_age) # Look at plot matrix

colSums(is.na(tbi_age)) # There's 11 NAs total, feel comfortable omitting them


# Data prep ---------------------------------------------------------------

# Names for injury_mechanism factor 
# I use this to create nice facet labels
inj_mech_labs <- c(
  "Assault",
  "Intentional self-harm",
  'Motor Vehicle Crashes',
  "Other or no mechanism specified",
  "Other unintentional injury,\nmechanism unspecified",
  'Unintentional Falls',
  'Unintentionally struck by or\nagainst an object'
)

# Names for age_group factor
# I use this to order my categorical axis in a way that makes sense
age_group_lvls <- c('0-4', '5-14', '15-24', '25-34', '35-44', 
                    '45-54', '55-64', '65-74', '75+')

# Prep data
new_tbi_age <- 
  tbi_age %>%
  na.omit() %>% # Omit 11 NA rows
  filter(!age_group %in% c('0-17', 'Total')) %>% # There's overlap in the U17 group so filter out
  group_by(age_group, injury_mechanism) %>%
  summarise(number_est = sum(number_est)) %>%
  ungroup() %>%
  mutate(age_group = factor(age_group, levels = age_group_lvls), 
         age_group = fct_rev(age_group)) %>%
  mutate(injury_mechanism = factor(injury_mechanism))

# Relevel injury_mechanism
levels(new_tbi_age$injury_mechanism) <- inj_mech_labs


# Viz ---------------------------------------------------------------------

# Plot unintentional causes
p1 <- 
  new_tbi_age %>%
  filter(str_detect(injury_mechanism, '[Uu]nintentional')) %>%
  ggplot(aes(x = age_group, y = number_est)) +
  geom_col(aes(fill = age_group, color = age_group), 
           alpha = 0.7,
           width = 0.6,
           show.legend = FALSE) +
  ggtitle('Traumatic Brain Injuries by Age Group (2014)',
          subtitle = 'Unintentional causes') +
  coord_flip() +
  scale_fill_fish_d(option = 'Halichoeres_bivittatus') +
  scale_color_fish_d(option = 'Halichoeres_bivittatus') +
  scale_y_continuous(breaks = scales::extended_breaks(n = 3),
                     labels = scales::label_number(big.mark = ',')) +
  facet_wrap(injury_mechanism~.) +
  theme_maia() +
  theme(axis.text.x = element_text(family = 'Arial Narrow', size = 9),
        axis.text.y = element_text(family = 'Arial Narrow', size = 10)) +
  labs(y = NULL, x = NULL)

# Plot intentional or other causes
p2 <- 
  new_tbi_age %>%
  filter(!str_detect(injury_mechanism, '[Uu]nintentional')) %>%
  ggplot(aes(x = age_group, y = number_est)) +
  geom_col(aes(fill = age_group, color = age_group), 
           alpha = 0.7,
           width = 0.6,
           show.legend = FALSE) +
  coord_flip() +
  scale_fill_fish_d(option = 'Halichoeres_bivittatus') +
  scale_color_fish_d(option = 'Halichoeres_bivittatus') +
  scale_y_continuous(breaks = scales::extended_breaks(n = 3),
                     labels = scales::label_number(big.mark = ',')) +
  facet_wrap(injury_mechanism~.) +
  theme_maia() +
  theme(plot.caption = element_text(size = 8)) +
  labs(y = 'Estimated number of injuries', x = NULL,
       caption = '@MaiaPelletier\nData sourced from CDC and Veterans Brain Injury Center',
       subtitle = 'Intentional harm or other causes')

p1 / p2 # Patch plots together with {patchwork}

ggsave('TBI.png', type = 'cairo') # Save final plot!
  