#Naive Corporate Income Calculation, stacked DiD, Agrawal feedback



library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
library(synthdid)
library(fixest)
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

naive_ci<-read.csv("naive_ci.csv")

#filter to necessary variables
filt_Corp <-naive_ci %>%
  select(State_Acronym,year,year_effective,State_Name,log_ci,Post, rel_year)


#start with one state, nebraska
# Define the state of interest
state_of_interest <- "Nebraska"

# Filter for the state of interest and its treatment year
treatment_year <- filt_Corp %>%
  filter(State_Name == state_of_interest) %>%
  pull(year_effective) %>%
  unique()  # Ensure we get a unique value for the treatment year

# 1. Define the Treated Event for Nebraska
# Filter data for Nebraska within the -4 to +4 relative year window around the treatment year
treatment_event <- filt_Corp %>%
  filter(State_Name == state_of_interest & rel_year >= -4 & rel_year <= 4) %>%
  mutate(treated = 1,  # Mark as treated
         event_id = paste(state_of_interest, treatment_year, sep = "_"))  # Create unique event ID

# 2. Define the Clean Control Group
# Step (1): Filter states that adopt SSFA within the treatment_year + 4 window
remaining_control <- filt_Corp %>%
  filter(State_Name != state_of_interest) %>%  # Exclude the treatment state
  filter(is.na(year_effective) | year_effective > (treatment_year + 4))  # Exclude states adopting within 4 years after treatment

# Step (2): For remaining states, keep only observations within the -4 to +4 year window relative to Nebraska's treatment year
clean_control <- remaining_control %>%
  filter(year >= (treatment_year - 4) & year <= (treatment_year + 4)) %>%
  mutate(treated = 0,  # Mark as control
         event_id = paste(state_of_interest, treatment_year, sep = "_"))  # Use same event ID

# Combine treatment and control data for Nebraska's event
nebraska_event_data <- bind_rows(treatment_event, clean_control)



