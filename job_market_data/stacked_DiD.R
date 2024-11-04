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
  select(State_Acronym,year,year_effective,State_Name,log_ci,Post)


#create a loop for the stacked DiD, creates an event-level dataset
#create result list to store dataframe
result_list <- list()
# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1


# Prepare for stacked DiD by creating an event-level dataset
stacked_df <- data.frame()

for (state_name in unique(filt_Corp$State_Name)) {
  # Reset original dataframe each loop
  df <- filt_Corp
  
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  
  # Get treatment year for the current state
  treatment_year <- df %>% filter(State_Name == state_name) %>% pull(year_effective)
  
  # Define the treated event and clean comparison groups
  treatment_event <- df %>%
    filter(State_Name == state_name & year == treatment_year)
  
  clean_control <- df %>%
    filter(
      !(State_Name == state_name) &
        (is.na(year_effective) | abs(year_effective - treatment_year) > 4)  # no major reforms +/- 4 years
    )
  
  # Stack each treatment event and control data
  event_data <- bind_rows(treatment_event, clean_control)
  
  # Add identifier for each stacked event
  event_data <- event_data %>% 
    mutate(event_id = paste(state_name, treatment_year, sep = "_"),
           treated = ifelse(State_Name == state_name, 1, 0))  # 1 for treatment state
  
  # Add to stacked dataframe
  stacked_df <- bind_rows(stacked_df, event_data)
}


