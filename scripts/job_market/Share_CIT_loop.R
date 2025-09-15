#Will use the loop created from loop_sDiD, will modify it to get outputs ordered
# in better fashion.

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

# run loop for share CIT, what do I need for share corp
#for Corporate, will just need CIT_Rev, currently in the "Rev" dataframe. NO WY, WA, TX, SD, OH, NV
Share <- Rev %>%
  select(State_Acronym,year,CORPINCTX,INCTAX,TLINCTAX,SLGRTAX,SALESTAX,TOTLTAX,year_effective,Post,real_cit_capita,State_Name,real_totRev_capita)

#Remove sales tax because not sure what states to eliminate to make this work. 
#rerunning share Corporate/ Corporate income + individual income

frac_inc <- Share %>%
  mutate(frac_inc = CORPINCTX/(CORPINCTX+INCTAX))


filt_Corp <-frac_inc

#Create State Dataframes
#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1

#the break on 2022 as the treatment year is right before the counter after df created

while (TRUE) {
  #Reset to original each start
  df <-filt_Corp
  
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2022) {break}
  
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state$State_Name
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, emoves states that were already treated
  df <- df %>% 
    filter(is.na(year_effective) | year_effective >= treatment_year)
  
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


#Run with (1) CIT/Share of total income, (2) Share of All Tax Rev


#STEP 2, create just a point estimate loop with t-stat and confidence intervals

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "frac_inc", "Post")])
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "frac_inc", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  
  # Calculate the t-statistic
  t_statistic <- as.numeric(current_tau_hat) / se
  
  # Calculate the p-value
  p_value <- 2 * pt(abs(t_statistic), df = nrow(current_df) - 1, lower.tail = FALSE)
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.4f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.4f, %1.4f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
#  print(summary(current_tau_hat))
  
}

#Set-up Summary Statistics for frac_inc
summary_stats <- frac_inc %>%
  filter(!is.na(frac_inc)) %>%  # Remove rows with NA in frac_inc
  group_by(State_Name) %>%
  summarize(
    mean_frac_inc = mean(frac_inc, na.rm = TRUE),
    median_frac_inc = median(frac_inc, na.rm = TRUE),
    IQR_frac_inc = IQR(frac_inc, na.rm = TRUE),
    min_frac_inc = min(frac_inc, na.rm = TRUE),
    max_frac_inc = max(frac_inc, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))

#Something is missing, and can't audit it on the plane.

#FIGURED IT OUT... SOME OF THESE states are missing Sales Tax revenue, makes it an incomplete
#DF




# almost all point estimates are not significant.  But, could still informative.
#though, if nothing shows up here, it almost certainly isn't showing up as a share 
# of the total.



#remove plot for speed of loop



#Step 3- Syn DiD vs. DiD vs Synthetic Control Method Estimates and Plots










