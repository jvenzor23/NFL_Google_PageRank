# Producing the Correct Data Set
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

# getting rid of cases where the same team won in the division (other case will be handled later)
outcomes4 = outcomes3 %>%
  group_by(winning_team,losing_team) %>%
  summarize(diff_abs = mean(diff_abs))

Pij = matrix(0, nrow = 32, ncol = 32)
win_vect= outcomes4$winning_team
loss_vect = outcomes4$losing_team
diff_vect = outcomes4$diff_abs

for(iCnt in 1:length(win_vect)){
  Pij[win_vect[iCnt],loss_vect[iCnt]] = diff_vect[iCnt]
}

# now, need to average over the cases where one team won one game and another won the other

combs = combn(teams,2)

for(iCnt in 1:(length(combs)/2)){
  if((Pij[combs[,iCnt][1],combs[,iCnt][2]] != 0) & (Pij[combs[,iCnt][2],combs[,iCnt][1]] != 0)){
    if(Pij[combs[,iCnt][1],combs[,iCnt][2]] >= Pij[combs[,iCnt][2],combs[,iCnt][1]]){
      Pij[combs[,iCnt][1],combs[,iCnt][2]] = (Pij[combs[,iCnt][1],combs[,iCnt][2]] - 
                                                Pij[combs[,iCnt][2],combs[,iCnt][1]])/2
      Pij[combs[,iCnt][2],combs[,iCnt][1]] = 0
    }else{
      Pij[combs[,iCnt][2],combs[,iCnt][1]] = (Pij[combs[,iCnt][2],combs[,iCnt][1]] - 
                                                Pij[combs[,iCnt][1],combs[,iCnt][2]])/2
      Pij[combs[,iCnt][1],combs[,iCnt][2]] = 0
    }
  }
}

# THIS QUANTITY SHOULD BE EQUAL TO 208 (256 - 3*32/2)
sum(Pij != 0)

# ADDING COLUMN AND ROW NAMES TO MAKE IT A DATA FRAME
output = as.data.frame(Pij)
colnames(output) = as.character(team_names)
rownames(output) = as.character(team_names)


# Saving the File ---------------------------------------------------------
setwd("~/Desktop/Eigth Semester/Stochastic Modelling/NFL_Project/2018")
write.csv(output, "Pij_2018.csv")

# Adjusting for outlier scores
output[output >20] = 20
write.csv(output, "Pij_outlier_removed_2018.csv")