# Computing Team Winning Percentages

# Clean workspace
rm(list=ls())

# Calling Necessary Libraries
library(dplyr)
library(tidyverse)
library(lubridate)



# Reading in the necessary files ------------------------------------------


setwd("~/Desktop/NFL_PBP_DATA")
pbp2018 <- read.csv(file="reg_pbp_2018.csv", header=TRUE, sep=",") %>% select(-touchback)


# Checking the Data -------------------------------------------------------

# Figuring out how many games were played to check the data (teams should equal 32)
games = length(unique(pbp2018$game_id))  # this is the unique number of games played
teams = games/16*2  # Each team plays 16 games, but each game has two teams

# Determining Outcomes of Every Game --------------------------------------

# creating unique team codes
team_names = unique(pbp2018$home_team)
codes = seq(32)
team_codes = data.frame("team" = team_names,"team_id" = codes)

# Getting the outcomes for every game
outcomes = pbp2018 %>%
  group_by(game_id,home_team,away_team) %>%
  summarize(home_score = max(total_home_score),
            away_score = max(total_away_score))

# Computing the score differential for each game
outcomes2 = outcomes %>%
  mutate(diff = home_score - away_score)

# Getting the Matrices of Interest
outcomes3 = outcomes2 %>%
  inner_join(team_codes,by = c("home_team" = "team")) %>%
  inner_join(team_codes,by = c("away_team" = "team")) %>%
  mutate(winning_team = if_else(diff > 0, team_id.x, team_id.y),
         losing_team = if_else(diff > 0, team_id.y, team_id.x),
         tie_team_1 = if_else(diff == 0, team_id.x, as.integer(100)),
         tie_team_2 = if_else(diff == 0, team_id.y, as.integer(100))) %>%
  mutate(diff_abs = abs(diff)) %>%
  mutate(winning_team = if_else(tie_team_1 == 100,winning_team,as.integer(100)),
         losing_team = if_else(tie_team_1 == 100,losing_team,as.integer(100)))

# winning matrix
wins = outcomes3 %>%
  group_by(winning_team) %>%
  summarize(wins = length(unique(game_id))) %>%
  filter(winning_team != 100)

losses = outcomes3 %>%
  group_by(losing_team) %>%
  summarize(losses = length(unique(game_id))) %>%
  filter(losing_team != 100)

ties1 = outcomes3 %>%
  group_by(tie_team_1) %>%
  summarize(ties = length(unique(game_id))) %>%
  filter(tie_team_1 != 100)
  
ties2 = outcomes3 %>%
  group_by(tie_team_2) %>%
  summarize(ties = length(unique(game_id))) %>%
  filter(tie_team_2 != 100)

# Joining them together

RESULTS = team_codes %>%
  left_join(wins,by = c("team_id" = "winning_team")) %>%
  left_join(losses, by = c("team_id" = "losing_team")) %>%
  left_join(ties1, by = c("team_id" = "tie_team_1")) %>%
  left_join(ties2, by = c("team_id" = "tie_team_2"))
  
RESULTS[is.na(RESULTS)] <- 0

RESULTS = RESULTS %>%
  mutate(win_perc_2018 = (wins + .5*ties.x + .5*ties.y)/16) %>%
  select(team,win_perc_2018)

# Saving the File ---------------------------------------------------------
setwd("~/Desktop/Eigth Semester/Stochastic Modelling/NFL_Project/2018")
write.csv(RESULTS, "2018_win_perc.csv")

