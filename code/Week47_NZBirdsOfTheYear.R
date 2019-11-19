# Week 47 - NZ Bird of the Year -------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(Cairo)

tuesdata <- tt_load(2019, week = 47)
nz_bird <- tuesdata$nz_bird

nz_bird <- 
  nz_bird %>%
    mutate(
      vote_rank = str_replace(vote_rank, '_', ' '),
      vote_rank = str_to_title(vote_rank)
    )

winning_birds <- 
  
  right_join(
    
    nz_bird %>%
      count(vote_rank, bird_breed) %>%
      filter(!is.na(bird_breed)) %>%
      group_by(vote_rank) %>%
      top_n(2) %>%
      mutate(min_votes = min(n),
             rank = ifelse(n == min_votes, 'runner_up_breed', 'winner_breed')) %>%
      pivot_wider(names_from = rank, values_from = bird_breed, id_cols = vote_rank),
    
    nz_bird %>%
      count(vote_rank, bird_breed, name = 'num_of_votes') %>%
      filter(!is.na(bird_breed)) %>%
      group_by(vote_rank) %>%
      mutate(total_votes = sum(num_of_votes)) %>%
      top_n(2, num_of_votes) %>%
      mutate(frac_votes = num_of_votes/total_votes) %>%
      mutate(
        min_votes = min(frac_votes), 
        rank = ifelse(frac_votes == min_votes, 'runner_up', 'winner')) %>%
      select(-min_votes, -num_of_votes, -total_votes)  %>%
      pivot_wider(
        names_from = rank, 
        values_from = frac_votes, 
        id_cols = vote_rank
      ),
    
    by = 'vote_rank'
    
  )

winning_birds$vote_rank <- 
  factor(
    winning_birds$vote_rank, levels = paste('Vote', 5:1)
  )

ggplot(winning_birds) +
  geom_segment(aes(x = vote_rank, xend = vote_rank, y = runner_up, yend = winner), color = 'grey50') +
  geom_point(aes(x = vote_rank, y = runner_up, color = runner_up_breed), alpha = 0.8, size = 4, shape = 17) +
  geom_point(aes(x = vote_rank, y = winner, color = winner_breed), alpha = 0.6, size = 6) +
  labs(y = 'Fraction of total votes received', x = NULL, caption = 'Data from the New Zealand Forest and Bird Organization, courtesy of Dragonfly Data Science') +
  ggtitle('The race between the Yellow-Eyed Penguin and its runner-ups',
          subtitle = 'New Zealand bird of the year 2019 voting results') +
  coord_flip() +
  ggthemes::scale_color_canva(palette = 'Birds and berries', name = 'Bird breed') +
  ggthemes::theme_hc() +
  theme(axis.title.x = element_text(size = 10, face = 'italic', color = 'grey40'),
        axis.text.x = element_text(size = 8, color = 'grey50'),
        axis.text.y = element_text(size = 10, face = 'italic', color = 'grey35'),
        legend.title = element_text(size = 9, face = 'italic', color = 'grey35'),
        legend.text = element_text(size = 8, color = 'grey30'),
        legend.position = 'top',
        plot.title = element_text(size = 15, face = 'italic', color = 'grey30', hjust = 0.5),
        plot.subtitle = element_text(size = 8, color = 'grey55', hjust = 0.5),
        plot.caption = element_text(size = 6.5, color = 'grey30'))

ggsave('TidyTuesday/Week 47 - NZ Birds of the Year/Week47_NZBirdsOfTheYear.png', type = 'cairo')

