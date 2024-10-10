library(tidyverse)
library(arrow)

source("Team005_starter_code.R") 

# When the home team is batting 
example_info <- game_info_df %>% 
  filter(top_bottom == "bottom", 
         batter %in% final_df$shortstop) %>%
  drop_na(batter) %>%
  select(game_str, play_per_game, batter) %>%
  rename(play_id = play_per_game) 

# Events where the ball was put into play and the batter ran 
example_events <- game_events_df %>% 
  group_by(game_str, play_id) %>% 
  filter(player_position == 10 & event_code == 4, 
         lead(player_position) == 255 & lead(event_code) == 16) %>% 
  # no foul balls or home runs 
  ungroup() %>% 
  inner_join(example_info, by = c("game_str", "play_id")) %>%
  select(game_str, play_id, timestamp, batter) %>%
  rename(timestamp_on_contact = timestamp) 

# We only want to track player movement until first base 
example_pos <- player_pos %>% 
  filter(game_str %in% example_events$game_str, 
         play_id %in% example_events$play_id, 
         field_x >= 0 & field_x <= 62.58, 
         field_y >= 0 & field_y <= 63.64, 
         player_position == 10) %>%
  collect() %>% 
  group_by(game_str, play_id) %>%
  arrange(timestamp) %>% 
  # timestamps are not always in order 
  ungroup() 

# Joining everything together 
home_to_first <- example_events %>% 
  left_join(example_pos, by = c("game_str", "play_id"), 
            relationship = "many-to-many") %>% 
  filter(timestamp >= timestamp_on_contact) %>% 
  select(game_str, play_id, batter, timestamp, field_x, field_y) %>% 
  group_by(game_str, play_id) %>% 
  mutate(change_x = field_x - lag(field_x)) %>% 
  filter(change_x >= 0 | is.na(change_x)) %>% 
  # this excludes runners going from first to second 
  ungroup() 

# Calculating speed for each play 
speed_data <- home_to_first %>% 
  group_by(game_str, play_id, batter) %>% 
  summarize(
    start_x = min(field_x), 
    start_y = min(field_y), 
    end_x = max(field_x), 
    end_y = max(field_y), 
    run_dist = sqrt((end_x - start_x)^2 + (end_y - start_y)^2), 
    time = max(timestamp) - min(timestamp),  
    speed = (run_dist / time) * 1000 
  ) %>%
  drop_na()

# Average speeds of individual shortstops 
player_speed <- speed_data %>% 
  group_by(batter) %>% 
  filter(speed >= quantile(speed, 0.90)) %>% 
  summarize(average_speed = round(mean(speed),2)) 


  