#------------------------------------------------------------------------------#
# Project
#
# Author: David Jeong
# Class: ISYE6420 Fall23 Project
# Description: Implement Bayesian hierarchical Poisson model in JAGS and output
# results for the report
#------------------------------------------------------------------------------#


# Clear existing data and close all active graphics
rm(list = ls())
# dev.off(dev.list()["RStudioGD"])

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
data <- read.csv("data_2021_epl.csv")

# Select most relevant columns - others are related to sports betting
data_2021_epl <- data %>%
  select(HomeTeam, AwayTeam, HomeGoal, AwayGoal)


# Put an index next to HomeTeam and AwayTeam
team_alphabetical <- sort(unique(data_2021_epl$HomeTeam))
team_indices <- setNames(seq_along(team_alphabetical), team_alphabetical)
data_2021_epl$HomeTeamId <- team_indices[data_2021_epl$HomeTeam]
data_2021_epl$AwayTeamId <- team_indices[data_2021_epl$AwayTeam]

data_2021_epl <- data_2021_epl %>%
  select(HomeTeamId, AwayTeamId, HomeGoal, AwayGoal)


#------------------------------------------------------------------------------#
# Specify model for RJAGS
#------------------------------------------------------------------------------#

# Setting seed for reproducibility
set.seed(100)

# Define the JAGS model specification
model.sat.text<- "
model{
  eps <- 0.0000001
  # Likelihood

  for (i in 1:nGames) {

    # Poisson distribution for HomeGoals
    y_homegoals[i] ~ dpois(lambda_home[i])
    pred_home[i] ~ dpois(lambda_home[i])
    
    # Poisson distribution for AwayGoals
    y_awaygoals[i] ~ dpois(lambda_away[i])
    pred_away[i] ~ dpois(lambda_away[i]) 
    
    
    # Scoring intensity using random effect
    lambda_home[i] <- exp(constant + home_effect + att[HomeTeamId[i]] + def[AwayTeamId[i]])
    lambda_away[i] <- exp(constant + att[AwayTeamId[i]] + def[HomeTeamId[i]])
    
    
    # Deviance calculation for Home
    devres_home[i] <- 2*y_homegoals[i]* log(y_homegoals[i]/lambda_home[i] +eps) +
      2*(y_homegoals[i] - lambda_home[i])
    # Deviance calculation for Away
    devres_away[i] <- 2*y_awaygoals[i]* log(y_awaygoals[i]/lambda_away[i] + eps) +
      2*(y_awaygoals[i] - lambda_away[i])
    
  }

  # Priors for lambda hyperparameters
  for (j in 1:nTeams) {
    att_sumtozero[j] ~ dnorm(mu_attack, tau_attack)
    def_sumtozero[j] ~ dnorm(mu_defense, tau_defense)
    
    att[j] <- att_sumtozero[j] - mean(att_sumtozero[])
    def[j] <- def_sumtozero[j] - mean(def_sumtozero[])
  }
  
  # Priors for home effect and Bayesian hierarchial modeling
  home_effect ~ dnorm(0, 0.1)
  constant ~ dnorm(0, 0.01)
  mu_attack ~ dnorm(0,0.0001)
  mu_defense ~ dnorm(0,0.0001)
  tau_attack ~ dgamma(0.01, 0.01)
  tau_defense ~ dgamma(0.01, 0.01)
  
  # Calculate Deviance for HomeTeam bayesian Poisson model
  dev_home  <-  sum(devres_home[])
  
  # Calculate Deviance for AwayTeam bayesian Poisson model
  dev_away  <-  sum(devres_away[])
  dev <- dev_home + dev_away
}
"

#------------------------------------------------------------------------------#
# Run Model with RJAGS
#------------------------------------------------------------------------------#

# Define data for JAGS model
dat1 <- list(nGames=nrow(data_2021_epl),
             nTeams=length(team_alphabetical),
             HomeTeamId=data_2021_epl$HomeTeamId,
             AwayTeamId=data_2021_epl$AwayTeamId,
             y_homegoals=data_2021_epl$HomeGoal,
             y_awaygoals=data_2021_epl$AwayGoal
)


# Initialize model parameters
model.inits <- list(home_effect = 0, constant = 0, mu_attack = 0, mu_defense = 0, tau_attack = 0, tau_defense = 0
)


# Set number of iterations and burn-in period
iterations <- 10000
burnin <- 1000

# Create a text connection for the model specification
model.sat.spec<-textConnection(model.sat.text)

# Fit the model using RJAGS
model.fit <- jags.model(model.sat.spec,
                        data=dat1)

# Extract samples from the model

# model.samples <- coda.samples(model.fit, c("att","def", "home_effect","constant","mu_attack","mu_defense","tau_attack","tau_defense"), n.iter=iterations)
model.samples <- coda.samples(model.fit, c("pred_home","y_homegoals","pred_away", "y_awaygoals", "home_effect","constant","mu_attack","mu_defense","tau_attack","tau_defense","dev"), n.iter=iterations)
#------------------------------------------------------------------------------#
# Summary of Model Results
#------------------------------------------------------------------------------#

# View summary of results, excluding burn-in
summary(window(model.samples, start = burnin))

# Compute and display the HDI (Highest Density Interval) confidence interval
ci_hdi <- ci(model.samples, method = "HDI", ci = 0.95)
ci_hdi[which(grepl("home_effect", ci_hdi$Parameter)),]
ci_hdi[which(grepl("constant", ci_hdi$Parameter)),]
ci_hdi[which(grepl("mu", ci_hdi$Parameter)),]
ci_hdi[which(grepl("tau", ci_hdi$Parameter)),]
ci_hdi[which(grepl("dev", ci_hdi$Parameter)),]


#------------------------------------------------------------------------------#
# Create Histogram of all Home/Away Goals
#------------------------------------------------------------------------------#

summary_mean_list <- summary(window(model.samples, start = burnin))$statistics[,1]
summary_sd_list <- summary(window(model.samples, start = burnin))$statistics[,2]

summary_mean_list[grep("home_effect", names(summary_mean_list))]
summary_sd_list[grep("home_effect", names(summary_sd_list))]
summary_mean_list[grep("constant", names(summary_mean_list))]
summary_sd_list[grep("constant", names(summary_sd_list))]
summary_mean_list[grep("mu", names(summary_mean_list))]
summary_sd_list[grep("mu", names(summary_sd_list))]
summary_mean_list[grep("tau", names(summary_mean_list))]
summary_sd_list[grep("tau", names(summary_sd_list))]
summary_mean_list[grep("dev", names(summary_mean_list))]
summary_sd_list[grep("dev", names(summary_sd_list))]

# Extract prediction elements
pred_home <- summary_mean_list[grep("pred_home", names(summary_mean_list))]
pred_away <- summary_mean_list[grep("pred_away", names(summary_mean_list))]
# Extract observed elements
y_home <- summary_mean_list[grep("y_homegoals", names(summary_mean_list))]
y_away <- summary_mean_list[grep("y_awaygoals", names(summary_mean_list))]

df_bhm <- data %>% select(HomeTeam, AwayTeam) %>% 
  cbind(pred_home, pred_away) %>%
  rename(HomeGoal=pred_home, AwayGoal=pred_away)
row.names(df_bhm) <- NULL

df_bhm$HomeGoal <- floor(df_bhm$HomeGoal)
df_bhm$AwayGoal <- floor(df_bhm$AwayGoal)

df_score_histogram <- df_bhm %>% 
  pivot_longer(cols = c(HomeGoal, AwayGoal), names_to = "ScoreType", values_to = "Score") %>%
  select("ScoreType", "Score")

ggplot(df_score_histogram, aes(x = Score, fill = ScoreType)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  scale_fill_manual(values = c("HomeGoal" = "darkslategray3", "AwayGoal" = "deepskyblue4")) +
  theme_minimal() +
  labs(title = "BHM Predicted Home vs Away Scores (21-22)", x = "Score", y = "Frequency") +
  xlim(-0.5, 8)

#------------------------------------------------------------------------------#
# Create Predicted Aggregate Table
#------------------------------------------------------------------------------#

# Put the predicted values together in a nice table form to be transformed like in the observed value table
# Remember to preserve rows in the same sequence

# Apply the calculate_points function and aggregate
points <- t(apply(df_bhm, 1, function(x) calculate_points(x['HomeGoal'], x['AwayGoal'])))
rankdf_bhm <- cbind(df_bhm, points) 

# Home data and away data
home_agg_data <- rankdf_bhm %>%
  select(HomeTeam, HomeGoal, AwayGoal, HomePoints) %>% 
  rename(Team = HomeTeam, GoalsFor = HomeGoal, GoalsAgainst = AwayGoal, Points = HomePoints)
away_agg_data <- rankdf_bhm %>%
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
  ) %>% arrange(desc(Points), desc(GoalsDifference))

# write.csv(final_observed_data, file = "bhm_final_data_2021_epl.csv", row.names = FALSE)


#------------------------------------------------------------------------------#
# End of Script
#------------------------------------------------------------------------------#
