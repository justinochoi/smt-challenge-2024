library(tidyverse)
library(sportyR)
library(scales) 
library(car)

# Baseball field graphs 
# Example play for max y-coordinate 
play_136 <- ss_tracking %>%
  filter(game_str == "1884_078_Vis3AO_Home3A", 
         play_id == 136, 
         field_y == max(field_y)) 

# Example play for variable explanation 
play_115 <- ss_tracking %>% 
  filter(game_str == "1883_010_Vis1AD_Home1A", 
         play_id == 115) 

# Max y-coordinate graph
geom_baseball(league = "MiLB") + 
  geom_point(data=play_136, aes(x=field_x, y=field_y, size=3), 
             shape=21, fill="skyblue", show.legend = F) + 
  geom_segment(data=play_136, aes(x=0, y=0, yend=field_y), color="red", 
               arrow = arrow(length = unit(0.10, "inches")), 
               show.legend = F) + 
  geom_hline(yintercept = play_136$field_y, color="red", linetype="dashed") + 
  geom_label(aes(x=120, y=190, label = "Y = 165.69 ft.")) 

# Initial distance + angle between 
geom_baseball(league = "MiLB", display_range = "infield") + 
  geom_point(data=play_115 %>% filter(timestamp == timestamp_on_contact + 500), 
             aes(x=ball_position_x, y=ball_position_y, size=2), 
             shape=21, fill="white", show.legend = F) + 
  geom_point(data=play_115 %>% filter(timestamp == timestamp_on_contact + 500), 
             aes(x=field_x, y=field_y, size=2), 
             shape=21, fill="skyblue", show.legend = F) + 
  geom_segment(data=play_115 %>% filter(timestamp == timestamp_on_contact + 500), 
               aes(x=ball_position_x, y=ball_position_y, xend=field_x, yend=field_y),
               color="red", show.legend = F) + 
  geom_segment(data=play_115 %>% filter(timestamp == timestamp_on_contact + 500), 
               aes(x=ball_position_x, y=ball_position_y, xend=ball_position_x, yend=500), 
               color="red", linetype=2, show.legend = F) + 
  geom_label(aes(x=-55, y=100, label = "Initial Distance")) + 
  geom_label(aes(x=35, y=80, label = "Angle Between")) 

# Distance covered
geom_baseball(league = "MiLB", display_range = "infield") + 
  geom_point(data=play_115 %>% filter(timestamp == timestamp_on_acquire - 50), 
             aes(x=ball_position_x, y=ball_position_y, size=2), 
             shape=21, fill="white", show.legend = F) + 
  geom_point(data=play_115 %>% filter(timestamp == timestamp_on_acquire), 
             aes(x=field_x, y=field_y, size=2), 
             shape=21, fill="skyblue", show.legend = F) + 
  geom_segment(data=play_115 %>% filter(timestamp == timestamp_on_contact + 500 | 
                                        timestamp == timestamp_on_acquire), 
               aes(x=min(field_x), y=min(field_y), 
                   xend=max(field_x), yend=max(field_y)), 
               color="red", arrow = arrow(length = unit(0.10, "inches")), 
               show.legend = F) + 
  geom_label(aes(x=-10, y=130, label = "Distance Covered"))

# Similarity demonstration 
sim_data <- data.frame(
  Mover = c("Player", "Ball"), 
  x_movement = c(-50, 50), 
  y_movement = c(-20, 20)
) 

ggplot(sim_data, aes(x_movement, y_movement)) + 
  geom_point() + 
  geom_segment(
    aes(x=0, y=0, xend=x_movement, yend=y_movement, 
        group=Mover, color=Mover), 
    arrow = arrow(length = unit(0.10, "inches"))
  ) + 
  geom_label(aes(x=-65, y=45, label = "Angle: 180°")) +
  geom_label(aes(x=-65, y=35, label = "Similarity: -1")) + 
  scale_color_manual(values = c("orange", "skyblue")) + 
  theme_bw() + 
  labs(
    x = "Movement in Horizontal Direction (ft.)", 
    y = "Movement in Vertical Direction (ft.)" 
  ) + 
  xlim(-100, 100) + 
  ylim(-50, 50) 

# Graphing the relationships between variables on expected success rate 
# Distance covered vs. angle between 
final_df %>%
  ggplot(aes(x=distance_covered, y=angle_between, z=est_prob)) +
  stat_summary_hex(bins = 10) +
  scale_fill_viridis_b(lim=c(0,1), oob=squish, labels=percent_format()) + 
  geom_vline(xintercept = 25, linetype="dashed", color='black') + 
  geom_hline(yintercept = 10, linetype="dashed", color='black') +
  geom_label(aes(x=5, y=10, label="10 deg.")) + 
  geom_label(aes(x=25, y=25, label="25 ft.")) + 
  geom_text()
  theme_bw() + 
  labs(
    x = "Distance Covered (ft.)",
    y = "Angle Between Fielder and Ball", 
    title = "Distance Covered Doesn't Matter... Until It Does", 
    fill = "Expected\nSuccess\nRate"
  ) + 
  xlim(0,50) + 
  ylim(0,30) + 
  theme(plot.title = element_text(face = "bold"))

# Similarity vs. Distance covered 
# Positive similarity is almost always bad 
final_df %>% 
  ggplot(aes(x=similarity, y=distance_covered, z=est_prob)) + 
  stat_summary_hex(bins = 10) + 
  scale_fill_viridis_b(lim=c(0,1), oob=squish, labels=percent_format()) +
  theme_bw() + 
  labs(
    x = "Similarity", 
    y = "Distance Covered", 
    fill = "Expected\nSuccess\nRate"
  ) + 
  xlim(-1,1) + 
  ylim(0,50) 

# Iniial distance vs. angle between 
# Shortstops actually prefer to have more distance 
final_df %>% 
  ggplot(aes(x=euclid_dist, y=angle_between, z=est_prob)) + 
  stat_summary_hex(bins = 10) + 
  scale_fill_viridis_b(lim=c(0,1), oob=squish, labels=percent_format()) + 
  scale_x_continuous(breaks = pretty_breaks(n=5)) +
  theme_bw() + 
  labs(
    x = "Initial Distance to Ball (ft.)", 
    y = "Angle Between Fielder and Ball", 
    title = "Distance and Angle Create Limitations", 
    fill = "Expected\nSuccess\nRate"
  ) + 
  ylim(0,30) + 
  theme(plot.title = element_text(face = "bold")) 

# Change to factor and make sure it's in correct order 
final_df$farm_level <- 
  fct_relevel(final_df$farm_level, "One","Two","Three","Four") 

# Averages of each metric by farm level 
final_df %>% 
  group_by(farm_level) %>% 
  filter(angle_between < 30) %>% 
  # There are some outlier angles that influence the mean for each level 
  # Could use median, but median CI are a huge hassle in R 
  summarize(
    mean_angle = mean(angle_between), 
    mean_euclid = mean(euclid_dist), 
    mean_dist_covered = mean(distance_covered), 
    mean_similarity = mean(similarity), 
    mean_prob = mean(est_prob)
    ) 

# Not surprisingly, players at higher levels have higher estimated probs. 
# The differentiating factor seems to be the angle_between metric
final_df %>% 
  group_by(farm_level) %>% 
  filter(angle_between < 30) %>% 
  summarize(
    sample = n(), 
    mean = mean(angle_between), 
    sd = sd(angle_between), 
    lower90 = mean - qt(0.95,sample-1)*sd / sqrt(sample), 
    upper90 = mean + qt(0.95,sample-1)*sd / sqrt(sample)
  ) %>% 
  ggplot(aes(x=farm_level, y=mean, label=sample)) + 
  geom_col(aes(fill=farm_level)) + 
  geom_errorbar(aes(ymin=lower90, ymax=upper90), alpha=1, width=0.2, 
                color="orange") +
  geom_label() + 
  theme_bw() + 
  scale_fill_manual(values=c("#003f5c","#00809a","#00c39c","#75ff63")) + 
  scale_y_continuous(breaks = pretty_breaks(n=6)) + 
  labs(
    x = "Farm System Level", 
    y = "Average Angle Between", 
    title = "Are Upper-Level Shortstops Better at\nPositioning Themselves?", 
    subtitle = "Error bars represent 90% confidence intervals"
  ) + 
  theme(plot.title = element_text(face = "bold")) + 
  theme(legend.position = "none") 

leveneTest(angle_between ~ farm_level, 
           data = subset(final_df, angle_between < 30), center = mean) 
# There's evidence that the variances in angle_between are unequal by level 
# Upper-level shortstops are more consistent 
# Also, lower levels are more likely to feature varying levels of skill 
# among their players (org. depth vs. future superstars)

# Numbers by farm level are a bit wonky, so let's smooth them out by 
# sorting the levels into lower and upper: 
final_df %>% 
  mutate(level_type = case_when(
    farm_level == "One" | farm_level == "Two" ~ "Lower", 
    .default = "Upper")
    ) %>%
  filter(angle_between < 30) %>% 
  ggplot(aes(x=angle_between, fill=level_type)) + 
  geom_density(adjust = 1.5, alpha = 0.5) + 
  scale_fill_manual(values=c("#00809a","#75ff63")) + 
  theme_bw() + 
  scale_x_continuous(breaks = pretty_breaks(n=5)) + 
  labs(
    x = "Angle Between", 
    y = "Density", 
    title = "Angle Between in Upper Levels Is More Consistent", 
    subtitle = "Lower = 1A & 2A | Upper = 3A & 4A", 
    fill = "Level Group" 
  ) + 
  theme(plot.title = element_text(face = "bold")) 

# Mean success rate as sanity check
final_df %>% 
  mutate(level_type = case_when(
    farm_level == "One" | farm_level == "Two" ~ "Lower", 
    .default = "Upper")
  ) %>%
  filter(angle_between < 30) %>%
  group_by(level_type) %>% 
  summarize(mean_prob = mean(est_prob))

# Metrics for individual shortstops 
ss_examples <- final_df %>% 
  drop_na(shortstop) %>% 
  group_by(shortstop) %>% 
  summarize(
    sample = n(), 
    angle_between = round(mean(angle_between),1), 
    distance_covered = round(mean(distance_covered),2), 
    similarity = round(mean(similarity),2),
    exp_success_rate = round(mean(est_prob),3)
    ) %>% 
  filter(sample >= 5) %>%
  left_join(player_speed, by = join_by(shortstop == batter)) 

# Home-to-first time vs. expected success rate
ss_examples %>% 
  ggplot(aes(x=average_speed, y=exp_success_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(breaks = pretty_breaks(n=6)) + 
  scale_y_continuous(breaks = pretty_breaks(n=6), labels = percent_format()) + 
  theme_bw() + 
  labs(
    x = "Home-to-First Time (sec.)", 
    y = "Expected Success Rate", 
    title = "Do Faster Players Get to More Balls?",  
    subtitle = "Shortstops with min. 5 opportunities (N = 10)"
  ) + 
  theme(plot.title = element_text(face = "bold"))
  
             