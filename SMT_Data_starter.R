# Welcome to the 2024 SMT Data Challenge! Here are some functions to help you get
# started. After you unzip the dataset, copy the name of the directory you saved 
# it to into the 'data_directory` field below. After making sure you have the 
# `arrow` package installed, you may call this file at the top of your work file(s)
# by calling `source("SMT_Data_starter.R"). Then, you may apply functions and 
# operations to the table names below as you would any other table and load them 
# into your working environment by calling `collect()`. For an example of this 
# process, un-comment and run the lines below the starter code. 
# 
# WARNING: The data subsets are large, especially `player_pos`. Reading the 
#   entire subset at once without filtering may incur performance issues on your 
#   machine or even crash your R session. It is recommended that you filter 
#   data subsets wisely before calling `collect()`.

data_directory <- '/Users/justinchoi/Downloads/2024_SMT_Data_Challenge/2024_SMT_Data_Challenge' 

###############################################################################
################## STARTER CODE: DO NOT MODIFY ################################
###############################################################################

library(arrow)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

team_info <- arrow::open_csv_dataset(paste0(data_directory,"/team_info.csv"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

###############################################################################
########################## END STARTER CODE ###################################
###############################################################################

# game_info_demo <- game_info |>
#   filter(Day == "day_059", 
#          inning == 3) |> 
#   collect()
