library(tidyverse) 
library(arrow) 
library(janitor)
library(lsa) 

source("SMT_Data_starter.R") 

# Load in game event data
game_events_83 <- game_events %>%
  filter(substr(game_str, 1, 4) == "1883") %>%
  collect() 

game_events_84 <- game_events %>%
  filter(substr(game_str, 1, 4) == "1884") %>%
  collect() 

game_events_df <- bind_rows(game_events_83, game_events_84) 

# Query balls that were hit into play, bounced, then caught 
# There should be no other events in between the aforementioned three 
# Assumption: for bip we are interested in, only the shortstop (6), 
# left fielder (7), or center fielder (8) fielded the ball 
one_bounce <- game_events_df %>%
  group_by(game_str, play_id) %>%
  filter((player_position == 6 & event_code == 2 | 
            player_position == 7 & event_code == 2 | 
            player_position == 8 & event_code == 2) & 
           (lag(player_position) == 255 & lag(event_code) == 16) & 
           (lag(player_position, n=2) == 10 & lag(event_code, n=2) == 4)
         ) %>%
  ungroup() 

two_bounce <- game_events_df %>% 
  group_by(game_str, play_id) %>% 
  filter((player_position == 6 & event_code == 2 | 
            player_position == 7 & event_code == 2 | 
            player_position == 8 & event_code == 2) &
           (lag(player_position) == 255 & lag(event_code) == 16) & 
           (lag(player_position, n=2) == 255 & lag(event_code, n=2) == 16) & 
           (lag(player_position, n=3) == 10 & lag(event_code, n=3) == 4)
         ) %>%
  ungroup() 

three_bounce <- game_events_df %>% 
  group_by(game_str, play_id) %>% 
  filter((player_position == 6 & event_code == 2 | 
            player_position == 7 & event_code == 2 | 
            player_position == 8 & event_code == 2) &
           (lag(player_position) == 255 & lag(event_code) == 16) & 
           (lag(player_position, n=2) == 255 & lag(event_code, n=2) == 16) & 
           (lag(player_position, n=3) == 255 & lag(event_code, n=3) == 16) & 
           (lag(player_position, n=4) == 10 & lag(event_code, n=4) == 4)
         ) %>%
  ungroup() 

four_bounce <- game_events_df %>% 
  group_by(game_str, play_id) %>% 
  filter((player_position == 6 & event_code == 2 | 
            player_position == 7 & event_code == 2 | 
            player_position == 8 & event_code == 2) &
           (lag(player_position) == 255 & lag(event_code) == 16) & 
           (lag(player_position, n=2) == 255 & lag(event_code, n=2) == 16) & 
           (lag(player_position, n=3) == 255 & lag(event_code, n=3) == 16) & 
           (lag(player_position, n=4) == 255 & lag(event_code, n=4) == 16) & 
           (lag(player_position, n=5) == 10 & lag(event_code, n=5) == 4)
         ) %>%
  ungroup() 

five_bounce <- game_events_df %>% 
  group_by(game_str, play_id) %>% 
  filter((player_position == 6 & event_code == 2 | 
            player_position == 7 & event_code == 2 | 
            player_position == 8 & event_code == 2) &
           (lag(player_position) == 255 & lag(event_code) == 16) & 
           (lag(player_position, n=2) == 255 & lag(event_code, n=2) == 16) & 
           (lag(player_position, n=3) == 255 & lag(event_code, n=3) == 16) & 
           (lag(player_position, n=4) == 255 & lag(event_code, n=4) == 16) & 
           (lag(player_position, n=5) == 255 & lag(event_code, n=5) == 16) & 
           (lag(player_position, n=6) == 10 & lag(event_code, n=6) == 4) 
        ) %>%
  ungroup() 

# There are no six-bounce grounders, so this is everything 
grounder_candidates <- 
  bind_rows(one_bounce, two_bounce, three_bounce, four_bounce, five_bounce) 

# Select game event data that overlap with the grounder candidates 
grounder_events <- game_events_df %>% 
  inner_join(select(grounder_candidates, game_str, play_id), 
             by = c("game_str", "play_id")) %>% 
  # Only include parts of plays that are relevant  
  filter(player_position %in% c(10,255,6,7,8), 
         event_code %in% c(4,16,2)) %>% 
  group_by(game_str, play_id) %>% 
  # No interactions between fielders 
  filter(n_distinct(player_position) == 3) %>% 
  ungroup() 

# Acquiring ball position data with appropriate filters 
ball_pos_df <- ball_pos %>%
  filter(play_id %in% grounder_events$play_id, 
         game_str %in% grounder_events$game_str) %>% 
  collect() 

# Join event data with batted ball position data 
# Then calculate the launch angle of each ball in play 
grounder_la <- grounder_events %>% 
  filter(event_code == 4) %>% 
  left_join(ball_pos_df, by = c("game_str", "play_id"), 
            relationship = "many-to-many") %>%
  rename(timestamp_on_contact = timestamp.x, 
         timestamp = timestamp.y) %>% 
  group_by(game_str, play_id) %>% 
  mutate(
    launch_angle = round(
      atan2(lead(ball_position_z) - ball_position_z, 
            lead(ball_position_y) - ball_position_y) * (180 / pi), 3
      ) 
  ) %>% 
  ungroup() %>% 
  # This launch angle threshold seems to work the best, along with 
  # a one-frame delay to account for interpolation error 
  filter(timestamp_on_contact + 50 == timestamp, launch_angle < 5) 

# Only return events that are also in the grounders_la table 
# These will be the grounders that we work with 
grounder_events <- grounder_events %>% 
  inner_join(select(grounder_la, game_str, play_id), 
             by = c("game_str", "play_id")) 

# Create indicators for: 
# Whether the shortstop got to the ball or not 
# The level at which the game was played  
grounder_events <- grounder_events %>% 
  group_by(game_str, play_id) %>% 
  mutate(
    ss_flag = if_else(player_position == 6, 1, 0), 
    ss_got_ball = if_else(sum(ss_flag) > 0, 'Yes', 'No'), 
    farm_level = case_when(
      substr(HomeTeam, 5, 6) == '1A' ~ "One", 
      substr(HomeTeam, 5, 6) == '2A' ~ "Two", 
      substr(HomeTeam, 5, 6) == '3A' ~ "Three", 
      .default = "Four"
      )
    ) %>% 
  ungroup() 

# When the ball is hit (start) and acquired by a fielder (end)
grounder_start_end <- grounder_events %>% 
  group_by(game_str, play_id) %>% 
  filter(player_position != 255) %>% 
  summarize(timestamp_on_contact = min(timestamp), 
            timestamp_on_acquire = max(timestamp)) 

grounder_events <- grounder_events %>% 
  left_join(grounder_start_end, by = c("game_str", "play_id"))
  
# Ball location data that we're actually interested in 
ss_tracking <- grounder_events %>% 
  filter(event_code == 2) %>% 
  select(game_str, play_id, timestamp_on_contact, timestamp_on_acquire,
         ss_flag, ss_got_ball, farm_level) %>% 
  left_join(select(ball_pos_df, game_str, play_id, timestamp,  
                   ball_position_x, ball_position_y), 
            by = c("game_str", "play_id"), relationship = "many-to-many") %>% 
  group_by(game_str, play_id) %>% 
  filter(between(timestamp, timestamp_on_contact, timestamp_on_acquire)) %>% 
  ungroup() 

# Acquiring player position data with appropriate filters 
player_pos_df <- player_pos %>% 
  filter(play_id %in% ss_tracking$play_id,   
         game_str %in% ss_tracking$game_str,  
         timestamp %in% ss_tracking$timestamp, 
         player_position == 6) %>% 
  collect() 

# Add shortstop position data
ss_tracking <- ss_tracking %>% 
  left_join(select(player_pos_df, game_str, play_id, 
                   timestamp, field_x, field_y), 
            by = c("game_str", "play_id", "timestamp")) 

# We don't care about position beyond a certain point
max_y <- max(subset(ss_tracking, ss_got_ball == "Yes")$ball_position_y)
ss_tracking <- ss_tracking %>% filter(ball_position_y <= max_y) 

# Important! There are three plays whose player_pos timestamps are 
# misaligned with their ball_pos timestamps, leading to NA values 
# All three are from the same game:  
ss_tracking_na <- ss_tracking %>% 
  filter(game_str == "1884_054_Vis1AR_Home1A")

# Acquire ball position data from problematic game 
ball_pos_054 <- ball_pos %>% 
  filter(game_str %in% ss_tracking_na$game_str, 
         play_id %in% ss_tracking_na$play_id) %>% 
  select(game_str, play_id, timestamp, ball_position_x, ball_position_y) %>% 
  collect() %>% 
  group_by(play_id) %>% 
  filter(
    case_when(
      play_id==162 ~ timestamp >= min(subset(ss_tracking_na, play_id==162)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==162)$timestamp), 
      play_id==198 ~ timestamp >= min(subset(ss_tracking_na, play_id==198)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==198)$timestamp), 
      .default = timestamp >= min(subset(ss_tracking_na, play_id==244)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==244)$timestamp) 
    )
  ) %>% 
  ungroup() 

# Acquire player position data from problematic game 
# Comparing with the ball_pos_054 table, we see that the timestamps are 
# systemically off by +7 ms 
player_pos_054 <- player_pos %>%
  filter(game_str %in% ss_tracking_na$game_str, 
         play_id %in% ss_tracking_na$play_id, 
         player_position == 6) %>%
  select(game_str, play_id, timestamp, field_x, field_y) %>% 
  collect() %>% 
  group_by(play_id) %>% 
  filter(
    case_when(
      play_id==162 ~ timestamp >= min(subset(ss_tracking_na, play_id==162)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==162)$timestamp) + 7, 
      play_id==198 ~ timestamp >= min(subset(ss_tracking_na, play_id==198)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==198)$timestamp) + 7, 
      .default = timestamp >= min(subset(ss_tracking_na, play_id==244)$timestamp) & 
        timestamp <= max(subset(ss_tracking_na, play_id==244)$timestamp) + 7 
    )
  ) %>% 
  ungroup() 

# Assuming constant acceleration, we estimate what the ball positions are 
# 7 ms after their originally listed times 
ball_pos_054 <- ball_pos_054 %>% 
  group_by(play_id) %>% 
  mutate(
    change_x = lead(ball_position_x) - ball_position_x, 
    change_y = lead(ball_position_y) - ball_position_y, 
    ball_position_x = ball_position_x + 0.14*change_x, # 7 ms / 50 ms = 0.14 
    ball_position_y = ball_position_y + 0.14*change_y, 
    timestamp = timestamp + 7 
  ) %>% 
  ungroup() %>% 
  select(-change_x, -change_y)

# We adjust the timestamps and replace everything with what have we calculated 
ss_tracking_na <- ss_tracking_na %>% 
  mutate(
    timestamp = timestamp + 7, 
    timestamp_on_contact = timestamp_on_contact + 7, 
    timestamp_on_acquire = timestamp_on_acquire + 7, 
    ball_position_x = ball_pos_054$ball_position_x, 
    ball_position_y = ball_pos_054$ball_position_y, 
    field_x = player_pos_054$field_x, 
    field_y = player_pos_054$field_y
  ) 

# Add the data back into the original and drop any relevant NA values 
# Now we're good to go 
ss_tracking <- bind_rows(ss_tracking, ss_tracking_na) %>% 
  drop_na(ball_position_x, ball_position_y, field_x, field_y)

# Check to see the distribution of times until fielder acquires ball 
grounder_start_end$acquire_time <- 
  grounder_start_end$timestamp_on_acquire - 
  grounder_start_end$timestamp_on_contact

hist(grounder_start_end$acquire_time) 
median(grounder_start_end$acquire_time)
mean(grounder_start_end$acquire_time)
# Median acquisition time is 1.85 seconds 
# Mean is higher at 2.25 seconds 

# At the half-second mark, calculate the following:  
# The shortstop's euclidean distance to the ball 
# The absolute difference between angle of ball and player 
ss_metrics_one <- ss_tracking %>% 
  group_by(game_str, play_id) %>% 
  filter(timestamp_on_contact + 500 == timestamp) %>% 
  ungroup() %>% 
  mutate(
    euclid_dist = round(
      sqrt((field_x - ball_position_x)^2 + (field_y - ball_position_y)^2), 3), 
    ball_angle = round(
      atan2(ball_position_x, ball_position_y) * (180 / pi), 3), 
    player_angle = round(
      atan2(field_x, field_y) * (180 / pi), 3), 
    angle_between = abs(ball_angle - player_angle)
  ) %>% 
  select(game_str, play_id, euclid_dist, angle_between, ss_got_ball) %>%
  # For some reason one duplicate row is created in the process 
  distinct(game_str, play_id, .keep_all = T) 

# Starting at the half-second mark, calculate the following: 
# How much distance the shortstop covers
# The angle at which the shortstop moves relative to the ball, which we will  
# represent as the cosine similarity between the ball and player vectors 
ss_metrics_two <- ss_tracking %>% 
  group_by(game_str, play_id) %>% 
  filter(timestamp >= timestamp_on_contact + 500) %>% 
  filter(row_number(timestamp) == 1 | 
           row_number(timestamp) == n()) %>% 
  mutate(
    ball_change_x = round(ball_position_x - lag(ball_position_x), 3), 
    ball_change_y = round(ball_position_y - lag(ball_position_y), 3), 
    player_change_x = round(field_x - lag(field_x), 3), 
    player_change_y = round(field_y - lag(field_y), 3), 
    distance_covered = round(
      sqrt((player_change_x)^2 + (player_change_y)^2), 3)
  ) %>% 
  ungroup() %>% 
  drop_na(distance_covered) %>% 
  select(game_str, play_id, ball_change_x:distance_covered) 

# Vectors of the ball and player 
ss_metrics_two <- ss_metrics_two %>% 
  rowwise() %>% 
  mutate(ball_vec = list(cbind(ball_change_x, ball_change_y)), 
         player_vec = list(cbind(player_change_x, player_change_y))
  ) 

# Calculate cosine similarity  
ss_metrics_two <- ss_metrics_two %>% 
  mutate(similarity = as.numeric(
    round(cosine(as.vector(unlist(ball_vec)), as.vector(unlist(player_vec))), 3)
    )
  ) 

# At long last 
# This is the data we'll use for training and testing models 
final_df <- ss_metrics_two %>% 
  left_join(ss_metrics_one, by = c("game_str", "play_id")) 
  select(game_str, play_id, euclid_dist, angle_between, 
         distance_covered, similarity, ss_got_ball)

# Correlations between variables 
# There is notable co-linearity (which we'll address when modeling) 
cor(select(final_df, euclid_dist:similarity))
plot(select(final_df, euclid_dist:similarity)) 
