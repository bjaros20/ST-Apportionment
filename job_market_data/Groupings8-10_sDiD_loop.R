#Micah's August Update- ideas to complete result
# up until this point, no point estimates have been significantly different from 0.
# Will load packages for TWFE, sDiD, Synthetic Control, DiD.
#Have tried TWFE on Log, detrended Total real Revenue, Could try it on CIT
#DiD & Synthetic Control on 2007 adopters, retro-active adopters.
#sDiD on all pre 2021 (29 states) for CIT, real total revenue per capita,
#share CIT/Total Revenue, share CIT/CIT+Ind Income.

#Therefore, going to try a few things:
#(0) Share of nationwide CIT revenue. then try following.
#(1) Group by adoption.  Group early, middle and late adopters together, to see
#how timing differs.
#(2) Level adoption alone, group by treatment level, direct and step-wise,
#staggered by 10 percent, etc.

#Other ideas from Micah Conversation: Industry & Regional groupings of adopters.
#After all these revenue routes, go rout of getting labor share by industry.

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
library(synthdid)
update.packages("synthdid")
library(fixest)
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")


#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")
Corp <-read.csv("CIT_sDID_loop.csv")

#Set up dataframe for running, mutate for share of Nationwide CIT Revenue
#Create a column that has the total annual CIT revenue for that year
Rev1 <-Rev %>%
  group_by(year)%>%
  mutate(Ann_CORPINCTX = sum(CORPINCTX))

Frac_CIT <- Rev1%>%
  mutate(nat_share=CORPINCTX/Ann_CORPINCTX)

#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-Frac_CIT %>%
  select(State_Acronym,year,year_effective,Post,treatment,State_Name,nat_share)

filt_Corp <-Filter_frac

#Loop to create Dataframe for each state:

#Create State Dataframes
#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1

#the break on 2022 as the treatment year is right before the counter after df created

#something is wrong with the filter, might need to restart R.

while (TRUE) {
  #Reset to original each start
  df <-filt_Corp
  
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2022) {break}
  
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective[1]  # Ensures treatment_year is scalar
  treatment_state_name <- treatment_state$State_Name[1]
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, 
  df <- df %>% 
    filter(is.na(year_effective) | is.na(State_Acronym) | year_effective >= treatment_year)
  
  # removes states treated in same year
  #the year_effective=treatment_year are filtered out if they are not 'treatment_state'
  df <- df %>%
    filter(is.na(year_effective) | (!(State_Name != treatment_state_name & year_effective == treatment_year)))
  
  
  # Filter out rows with year <= 2 years after treatment_year
  df <- df %>%
    filter(year <= treatment_year + 2)
  
  #filter out any states that get treated within 2 years of treatment
  df <- df %>%
    filter(is.na(year_effective) | (!(year_effective > treatment_year & year_effective<= treatment_year + 2)))
  
  #filter out Ohio after treatment_year >=2012, because OH eliminates CI in 2014
  df <- df %>%
    filter(!(treatment_year >= 2012 & State_Name == "Ohio"))
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2022) {break}
  
  #increment counter
  counter <-counter + 1
  
  #empty dataframe break
  if (nrow(df) == 0) {break}
}


#STEP 2, create just a point estimate loop with t-stat and confidence intervals

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "nat_share", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  
  # Calculate the t-statistic
  t_statistic <- as.numeric(current_tau_hat) / se
  
  # Calculate the p-value
  p_value <- 2 * pt(abs(t_statistic), df = nrow(current_df) - 1, lower.tail = FALSE)
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.2f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
  #  print(summary(current_tau_hat))
}
