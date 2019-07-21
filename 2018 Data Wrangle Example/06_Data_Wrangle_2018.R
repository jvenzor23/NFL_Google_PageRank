# Producing the Correct Data Set
# Doing YARDS/DRIVE

# Clean workspace
rm(list=ls())

# Calling Necessary Libraries
library(dplyr)
library(tidyverse)
library(lubridate)

# Reading in the necessary files ------------------------------------------

setwd("~/Desktop/NFL_PBP_DATA")
pbp2018 <- read.csv(file="reg_pbp_2018.csv", header=TRUE, sep=",",as.is = c("home_team","away_team","posteam"))


# Checking the Data -------------------------------------------------------

# Figuring out how many games were played to check the data (teams should equal 32)
games = length(unique(pbp2018$game_id))  # this is the unique number of games played
teams = games/16*2  # Each team plays 16 games, but each game has two teams
teams = 32

# Determining Outcomes of Every Game --------------------------------------


# creating unique team codes
team_names = unique(pbp2018$home_team)
codes = seq(32)
team_codes = data.frame("team" = team_names,"team_id" = codes)

# Pre-processing the pbo data frame
pbp2018_new = pbp2018 %>%
  select(game_id,play_id,home_team,away_team,posteam,drive,yards_gained) %>%
  filter(!(is.na(posteam)))

pbp2018_new[is.na(pbp2018_new)] = 0

# checking the stats
yards_per_drive_stats_home = pbp2018_new %>%
  filter(posteam == home_team) %>%
  group_by(game_id)%>%
  summarize(yards_per_drive = sum(yards_gained)/length(unique(drive)))

mean(yards_per_drive_stats_home$yards_per_drive)

yards_per_drive_stats_away = pbp2018_new %>%
  filter(posteam == away_team) %>%
  group_by(game_id)%>%
  summarize(yards_per_drive = sum(yards_gained)/length(unique(drive)))

mean(yards_per_drive_stats_away$yards_per_drive)

homeadv = mean(yards_per_drive_stats_home$yards_per_drive) - mean(yards_per_drive_stats_away$yards_per_drive)


# adding home and away yards

pbp2018_new3 = pbp2018_new %>%
  mutate(home_yards = if_else(posteam == home_team,yards_gained,as.integer(0)),
         away_yards = if_else(posteam == away_team,yards_gained,as.integer(0)))

# adding home and away drives in a game
pbp2018_drives_home = pbp2018_new3 %>%
  filter(posteam == home_team) %>%
  group_by(game_id) %>%
  summarize(drives_home = length(unique(drive)))

pbp2018_drives_away = pbp2018_new3 %>%
  filter(posteam == away_team) %>%
  group_by(game_id) %>%
  summarize(drives_away = length(unique(drive)))

pbp2018_new4 = pbp2018_new3 %>%
  inner_join(pbp2018_drives_home,by = "game_id") %>%
  inner_join(pbp2018_drives_away,by = "game_id")


# Getting the outcomes for every game
outcomes = pbp2018_new4 %>%
  group_by(game_id,home_team,away_team) %>%
  summarize(home_yards_per_drive = sum(home_yards)/mean(drives_home),
            away_yards_per_drive = sum(away_yards)/mean(drives_away))


outcomes2 = outcomes %>%
  mutate(home_yards_per_drive = home_yards_per_drive - homeadv)

# Computing the yards differential for each game
outcomes2 = outcomes2 %>%
  mutate(diff = home_yards_per_drive - away_yards_per_drive)

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
write.csv(output, "Pij_YPDRIVE_2018.csv")
