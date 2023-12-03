#------------------------------------------------------------------------------#
# Project
#
# Author: David Jeong
# Class: ISYE6420 Fall23 Project
# Description: Implement Multiple Linear Regression in JAGS and output
# results for the report
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
data <- read.csv("data_2021_epl.csv")

home_data <- data %>% select(HomeTeamShots, HomeTeamShotsTarget, HomeFouls, HomeCorners, HomeYellows, HomeReds)
y_homegoals <- data$HomeGoal

away_data <- data %>% select(AwayTeamShots, AwayTeamShotsTarget, AwayFouls, AwayCorners, AwayYellows, AwayReds)
y_awaygoals <- data$AwayGoal


#------------------------------------------------------------------------------#
# Specify model for RJAGS
#------------------------------------------------------------------------------#

# Setting seed for reproducibility
set.seed(100)

# Define the JAGS model specification
model.sat.text<- "
model{
  # Likelihood

  for (i in 1:n) {

    # Linear Regression Model for HomeTeam 
    y_homegoals[i] ~ dnorm(mu_home[i], tau)
    pred_home[i] ~ dnorm(mu_home[i], tau) # Create another mu_home with newer dataset for prediction next year
    mu_home[i] <- beta0 + beta1 * Xhome[i,1] + beta2 * Xhome[i,2] + beta3 * Xhome[i,3] + beta4 * Xhome[i,4] + beta5 * Xhome[i,5] + beta6 * Xhome[i,6]

    # Linear Regression Model for AwayTeam 
    y_awaygoals[i] ~ dnorm(mu_away[i], tau)
    pred_away[i] ~ dnorm(mu_away[i], tau) # Create another mu_away with newer dataset for prediction next year
    mu_away[i] <- beta0 + beta1 * Xaway[i,1] + beta2 * Xaway[i,2] + beta3 * Xaway[i,3] + beta4 * Xaway[i,4] + beta5 * Xaway[i,5] + beta6 * Xaway[i,6]

  }

  # Priors for regression coefficients
  beta0 ~ dnorm(0.0, 0.0000001)
  beta1 ~ dnorm(0.0, 0.0000001)
  beta2 ~ dnorm(0.0, 0.0000001)
  beta3 ~ dnorm(0.0, 0.0000001)
  beta4 ~ dnorm(0.0, 0.0000001)
  beta5 ~ dnorm(0.0, 0.0000001)
  beta6 ~ dnorm(0.0, 0.0000001)
  
  # Prior for the precision of the likelihood (inverse of variance)
  tau ~ dgamma(0.001, 0.001)
  variance <- 1 / tau
  
  # Calculate Deviance for HomeTeam bayesian linear regression model
  for (i in 1:n) {
    dy_home[i] <- (y_homegoals[i] - mean(y_homegoals[]))/variance
  }
  deviance_home <- sum(dy_home[]^2)
  
  # Calculate Deviance for AwayTeam bayesian linear regression model
  for (i in 1:n) {
    dy_away[i] <- (y_awaygoals[i] - mean(y_awaygoals[]))/variance
  }
  deviance_away <- sum(dy_away[]^2)
  
  deviance <- deviance_home + deviance_away
  
  # Calculate total sum of squares (sst) for HomeTeam
  for (i in 1:n) {
    cy_home[i] <- y_homegoals[i] - mean(y_homegoals[])
  }
  sst_home <- sum(cy_home[]^2)
  
  # Calculate the Bayesian R-squared for HomeTeam
  sse_home <- (n - m)*variance
  br2_home <- 1 - sse_home/sst_home
  
  # Calculate total sum of squares (sst) for AwayTeam
  for (i in 1:n) {
    cy_away[i] <- y_awaygoals[i] - mean(y_awaygoals[])
  }
  sst_away <- sum(cy_away[]^2)
  
  # Calculate the Bayesian R-squared for HomeTeam
  sse_away <- (n - m)*variance
  br2_away <- 1 - sse_away/sst_away
}
"

#------------------------------------------------------------------------------#
# Run Model with RJAGS
#------------------------------------------------------------------------------#

# Define data for JAGS model
dat1 <- list(n=nrow(home_data),
             m=ncol(home_data),
             Xhome=home_data,
             Xaway=away_data,
             y_homegoals=y_homegoals,
             y_awaygoals=y_awaygoals
)


# Initialize model parameters
model.inits <- list(beta0 = 1, beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0
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

model.samples <- coda.samples(model.fit, c("pred_home","y_homegoals","pred_away", "y_awaygoals", "beta0","beta1","beta2","beta3","beta4","beta5","beta6","tau","br2_home","br2_away", "deviance"), n.iter=iterations)

#------------------------------------------------------------------------------#
# Summary of Model Results
#------------------------------------------------------------------------------#

# View summary of results, excluding burn-in
summary(window(model.samples, start = burnin))

# Compute and display the HDI (Highest Density Interval) confidence interval
ci_hdi <- ci(model.samples, method = "HDI", ci = 0.95)
ci_hdi[which(grepl("beta", ci_hdi$Parameter)),]
ci_hdi[which(grepl("dev", ci_hdi$Parameter)),]
ci_hdi[which(grepl("tau", ci_hdi$Parameter)),]

#------------------------------------------------------------------------------#
# Create Histogram of all Home/Away Goals
#------------------------------------------------------------------------------#

summary_mean_list <- summary(window(model.samples, start = burnin))$statistics[,1]
summary_sd_list <- summary(window(model.samples, start = burnin))$statistics[,2]

summary_mean_list[grep("beta", names(summary_mean_list))]
summary_sd_list[grep("beta", names(summary_sd_list))]
summary_mean_list[grep("dev", names(summary_mean_list))]
summary_sd_list[grep("dev", names(summary_sd_list))]
summary_mean_list[grep("tau", names(summary_mean_list))]
summary_sd_list[grep("tau", names(summary_sd_list))]


# Extract prediction elements
pred_home <- summary_mean_list[grep("pred_home", names(summary_mean_list))]
pred_away <- summary_mean_list[grep("pred_away", names(summary_mean_list))]
# Extract observed elements
y_home <- summary_mean_list[grep("y_homegoals", names(summary_mean_list))]
y_away <- summary_mean_list[grep("y_awaygoals", names(summary_mean_list))]


df_lr <- data %>% select(HomeTeam, AwayTeam) %>% 
  cbind(pred_home, pred_away) %>%
  rename(HomeGoal=pred_home, AwayGoal=pred_away)
row.names(df_lr) <- NULL

df_lr$HomeGoal <- floor(df_lr$HomeGoal)
df_lr$AwayGoal <- floor(df_lr$AwayGoal)

df_score_histogram <- df_lr %>% 
  pivot_longer(cols = c(HomeGoal, AwayGoal), names_to = "ScoreType", values_to = "Score") %>%
  select("ScoreType", "Score")

ggplot(df_score_histogram, aes(x = Score, fill = ScoreType)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  scale_fill_manual(values = c("HomeGoal" = "darkslategray3", "AwayGoal" = "deepskyblue4")) +
  theme_minimal() +
  labs(title = "MLR Predicted Home vs Away Scores (21-22)", x = "Score", y = "Frequency") +
  xlim(-0.5, 8)

#------------------------------------------------------------------------------#
# Create Predicted Aggregate Table
#------------------------------------------------------------------------------#

# Put the predicted values together in a nice table form to be transformed like in the observed value table
# Remember to preserve rows in the same sequence

# Apply the calculate_points function and aggregate
points <- t(apply(df_lr, 1, function(x) calculate_points(x['HomeGoal'], x['AwayGoal'])))
rankdf_lr <- cbind(df_lr, points) 

# Home data and away data
home_agg_data <- rankdf_lr %>%
  select(HomeTeam, HomeGoal, AwayGoal, HomePoints) %>% 
  rename(Team = HomeTeam, GoalsFor = HomeGoal, GoalsAgainst = AwayGoal, Points = HomePoints)
away_agg_data <- rankdf_lr %>%
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

# write.csv(final_observed_data, file = "mlr_final_data_2021_epl.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# End of Script
#------------------------------------------------------------------------------#
