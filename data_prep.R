#------------------------------------------------------------------------------#
# Project
#
# Author: David Jeong
# Class: ISYE6420 Fall23 Project
# Description: 
#------------------------------------------------------------------------------#


# Clear existing data and close all active graphics
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

#------------------------------------------------------------------------------#
# Load required libraries
#------------------------------------------------------------------------------#

library(rjags)
library(coda)
library(bayestestR)
library(dplyr)
library(tidyr)
library(ggplot2)


#------------------------------------------------------------------------------#
# Create functions
#------------------------------------------------------------------------------#

calculate_points <- function(home_score, away_score) {
  if (home_score > away_score) {
    return(c(HomePoints = 3, AwayPoints = 0))
  } else if (home_score < away_score) {
    return(c(HomePoints = 0, AwayPoints = 3))
  } else {
    return(c(HomePoints = 1, AwayPoints = 1))
  }
}

#------------------------------------------------------------------------------#
# Prepare data and initialize parameters for model running
#------------------------------------------------------------------------------#

# Load the data from an csv file and preprocess it for model input.
data <- read.csv("2021-2022.csv")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y") #re-format the date format

# Select most relevant columns - others are related to sports betting
data_2021_epl <- data %>%
    select(Date, Time, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR) %>%
    rename(Date=Date, Time=Time, HomeTeam=HomeTeam, AwayTeam=AwayTeam, HomeGoal=FTHG, AwayGoal=FTAG, FullTimeResult=FTR, HomeTeamShots=HS, AwayTeamShots=AS, HomeTeamShotsTarget=HST, AwayTeamShotsTarget=AST, HomeFouls=HF, AwayFouls=AF, HomeCorners=HC, AwayCorners=AC, HomeYellows=HY, AwayYellows=AY, HomeReds=HR, AwayReds=AR)

# Put an index next to HomeTeam and AwayTeam
team_alphabetical <- sort(unique(data_2021_epl$HomeTeam))
team_indices <- setNames(seq_along(team_alphabetical), team_alphabetical)
data_2021_epl$HomeTeamId <- team_indices[data_2021_epl$HomeTeam]
data_2021_epl$AwayTeamId <- team_indices[data_2021_epl$AwayTeam]

# write.csv(data_2021_epl, file = "data_2021_epl.csv", row.names = FALSE)

data538 <- read.csv("spi_matches.csv")
data538_2021_epl <- data538 %>%
  filter(season == 2021, league == "Barclays Premier League") %>%
  select(-season, -league_id, -league)

#------------------------------------------------------------------------------#
# Add week number for each match and create a data vis week over week
#------------------------------------------------------------------------------#

df <- data_2021_epl %>% 
  pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "HomeAway", values_to = "Team") %>%
  arrange(Date, Time) %>% 
  group_by(Team) %>% 
  mutate(MatchPlayed = row_number()) %>%
  ungroup() 
# %>% select()




#------------------------------------------------------------------------------#
# Create the final aggregate table for observed data
#------------------------------------------------------------------------------#

# Apply points function row by row
points <- t(apply(data_2021_epl, 1, function(x) calculate_points(x['HomeGoal'], x['AwayGoal'])))
data_2021_epl <- cbind(data_2021_epl, points) 

# Home data and away data
home_agg_data <- data_2021_epl %>%
  select(HomeTeam, HomeGoal, AwayGoal, HomePoints) %>% 
  rename(Team = HomeTeam, GoalsFor = HomeGoal, GoalsAgainst = AwayGoal, Points = HomePoints)
away_agg_data <- data_2021_epl %>%
  select(AwayTeam, AwayGoal, HomeGoal, AwayPoints) %>%
  rename(Team = AwayTeam, GoalsFor = AwayGoal, GoalsAgainst = HomeGoal, Points = AwayPoints)

# Final aggregation observed data
final_observed_data <- bind_rows(home_agg_data, away_agg_data) %>%
    group_by(Team) %>%
    summarise(
      GoalsFor = sum(GoalsFor),
      GoalsAgainst = sum(GoalsAgainst),
      GoalsDifference = sum(GoalsFor) - sum(GoalsAgainst),
      Points = sum(Points)
    ) %>% arrange(desc(Points))

# write.csv(final_observed_data, file = "observed_final_data_2021_epl.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# Create the goals scored distribution for 2021-2022 data
#------------------------------------------------------------------------------#


df_score_histogram <- data_2021_epl %>% 
  pivot_longer(cols = c(HomeGoal, AwayGoal), names_to = "ScoreType", values_to = "Score") %>%
  select("ScoreType", "Score")

ggplot(df_score_histogram, aes(x = Score, fill = ScoreType)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  scale_fill_manual(values = c("HomeGoal" = "darkslategray3", "AwayGoal" = "deepskyblue4")) +
  theme_minimal() +
  labs(title = "Observed Home vs Away Scores (21-22)", x = "Score", y = "Frequency") +
  xlim(-0.5, 8)

