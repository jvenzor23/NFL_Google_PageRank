# Producing the Correct Data Set (INCLUDING WEEK THAT GAME OCCURRED)
# Here, we want to obtain a 32x32 matrix for each year,
# where the ijth entry is the amount that team j lost to team i by

# So, if team j beat team i, the ijth entry is 0, but the 
# jith entry is the amount the jth team won by.

# A subtlety is that each team plays 3 other teams in their division twice.
# So we average over the amount of points that the two games went, and then
# apply this. So if on average they tied, Pij = Pji = 0

# Last point is that it was found on average that home teams win by 2.5 points a game
# (see the python code that was already produced). As a result, we subtract 2.5 points from the
# home teams score to account for home field advantage, a major factor in the outcomes of games.
# (For more support on this, it is a common practice of Las Vegas casinos to "spot" the home team
# 3 points on a spread).

# Clean workspace
rm(list=ls())

# Calling Necessary Libraries
library(dplyr)
library(tidyverse)
library(lubridate)



# Reading in the necessary files ------------------------------------------


setwd("~/Desktop/NFL_PBP_DATA")
pbp2018 <- read.csv(file="reg_pbp_2018.csv", header=TRUE, sep=",",as.is = c("home_team","away_team","game_date")) 


# Checking the Data -------------------------------------------------------

# Figuring out how many games were played to check the data (teams should equal 32)
games = length(unique(pbp2018$game_id))  # this is the unique number of games played
teams = games/16*2  # Each team plays 16 games, but each game has two teams
teams = 32


# Determining the Date of Each Game ---------------------------------------

game_week = pbp2018 %>%
  select(game_id, game_date) %>%
  group_by(game_id) %>%
  summarize(game_date = unique(game_date))

game_week = game_week %>%
  mutate(data_diff = as.integer(ymd(game_date)- ymd(game_week$game_date[1]))) %>%
  mutate(week = floor(as.numeric(data_diff)/7)+1)

game_week_join = game_week %>%
  select(game_id, week)
  

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

# Subtracting the advantage of a home team from their score
outcomes2 = outcomes %>%
  mutate(home_score = home_score - 2.5)

# Computing the score differential for each game
outcomes2 = outcomes2 %>%
  mutate(diff = home_score - away_score)

# Forming the Matrix
outcomes3 = outcomes2 %>%
  inner_join(team_codes,by = c("home_team" = "team")) %>%
  inner_join(team_codes,by = c("away_team" = "team")) %>%
  mutate(winning_team = if_else(diff > 0, team_id.x, team_id.y),
         losing_team = if_else(diff > 0, team_id.y, team_id.x)) %>%
  mutate(diff_abs = abs(diff))

# adding in the weeks
outcomes4 = outcomes3 %>%
  inner_join(game_week_join, by = "game_id")

# outcomes final
outcomes_5 = outcomes4 %>%
  select(winning_team,losing_team,diff_abs,week)

# getting teams
team_nums = seq(32)
team_names_df = data.frame("team_name" = team_names, "team_num" = team_nums)

# Saving the File ---------------------------------------------------------
setwd("~/Desktop/Eigth Semester/Stochastic Modelling/NFL_Project/2018")
write.csv(outcomes_5, "week_data_2018.csv")
write.csv(team_names_df, "teams_2018.csv")



