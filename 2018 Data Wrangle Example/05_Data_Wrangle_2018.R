# Producing the Correct Data Set
# Here, we want to obtain a 32x32 matrix for each year,
# where the ijth entry is the amount of FIRST DOWNS that team i got more than team j 
# (normalizing for number of possessions)

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

# Pre-processing the pbo data frame
pbp2018_new = pbp2018 %>%
  select(game_id,play_id,home_team,away_team,posteam,yards_gained) %>%
  filter(!(is.na(posteam)))

pbp2018_new[is.na(pbp2018_new)] = 0

pbp2018_new2 = pbp2018_new %>%
  mutate(home_yard = if_else(posteam == home_team,yards_gained,as.integer(0)),
         away_yard = if_else(posteam == away_team,yards_gained,as.integer(0)))
  
# checking the stats
yards_home = sum(pbp2018_new2$home_yard)/length(unique(pbp2018_new2$game_id))
yards_away = sum(pbp2018_new2$away_yard)/length(unique(pbp2018_new2$game_id))

homeadv = yards_home - yards_away

# Getting the outcomes for every game
outcomes = pbp2018_new2 %>%
  group_by(game_id,home_team,away_team) %>%
  summarize(home_yards = sum(home_yard),
            away_yards = sum(away_yard))

outcomes2 = outcomes %>%
  mutate(home_yards = home_yards - 14.5)

# Computing the score differential for each game
outcomes2 = outcomes2 %>%
  mutate(diff = home_yards - away_yards)

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
write.csv(output, "Pij_YDS_2018.csv")

