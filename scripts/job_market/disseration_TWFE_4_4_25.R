# estimate TWFE, Simple DiD, Event-study Style DiD for Dissertation Chapter

# Using 3-3-25 as a guide, I am going to estimate the log CI tax revenue, log NonCI
#tax revenue, and proxy log CI results using the TWFE, DiD, event-study DiD
# I will estimate these for 2006/2007, 2006 alone, and 2007 alone
# with 2015-2018 as the control group.

# Load required libraries
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(lmtest)
library(sandwich)

# Set working directory (modify as needed)
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

# Load data
data <- read.csv("real_log_nci.csv")

#Estimate Simple TWFE with c("AZ", "IN", "ME", "MN", "PA", "SC") switchers.
data_2007_sim <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    PostTreatment = ifelse(State_Acronym %in% c("AZ", "IN", "ME", "MN", "PA", "SC") & year >= 2007, 1, 0)
  )

twfe_lm <- lm(real_log_nci ~ PostTreatment + factor(State_Acronym) + factor(year), data = data_2007_sim)
summary(twfe_lm)
#PostTreatment           -0.34292    0.06777  -5.060 6.93e-07 ***

# Use different package
# --- Filter for 2007 and late switchers c("AZ", "IN", "ME", "MN", "PA", "SC") from 1990 to 2014
data_2007 <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    PostTreatment = ifelse(State_Acronym %in% c("AZ", "IN", "ME", "MN", "PA", "SC") & year >= 2007, 1, 0)
  )

model_twfe_2007 <- feols(real_log_nci ~ PostTreatment | State_Acronym + year, data = data_2007)
summary(model_twfe_2007)
#PostTreatment -0.342919   0.226426 -1.51449  0.15215 


# Simple DiD
data_did_2007 <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    Treated = ifelse(year_effective == 2007, 1, 0),
    Post = ifelse(year >= 2007, 1, 0),
    DiD = Treated * Post
  )

model_did_2007 <- lm(real_log_nci ~ Treated + Post + DiD, data = data_did_2007)
summary(model_did_2007)


# DiD         -0.34292    0.24137  -1.421  0.15624    
#Simple DiD and TWFE functionally same number


# --- Filter for 2006 + 2007 and late switchers, from 1990 to 2014
data_07 <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    Treated = ifelse(year_effective %in% c(2007), 1, 0),
    Post = ifelse(year >= year_effective, 1, 0),
    DiD = Treated * Post
  )

# event-study style DiD
event_study_2007 <- feols(
  real_log_nci ~ i(year, Treated, ref = 2006) | State_Acronym + year,
  data = data_07,
  cluster = ~State_Acronym
)
# Event Study
summary(event_study_2007, cluster = "State_Acronym")
