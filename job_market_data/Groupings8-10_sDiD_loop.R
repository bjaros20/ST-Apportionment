#Micah's August Update- ideas to complete result
# up until this point, no point estimates have been significantly different from 0.
# Will load packages for TWFE, sDiD, Synthetic Control, DiD.
#Have tried TWFE on Log, detrended Total real Revenue, Could try it on CIT
#DiD & Synthetic Control on 2007 adopters, retro-active adopters.
#sDiD on all pre 2021 (29 states) for CIT, real total revenue per capita,
#share CIT/Total Revenue, share CIT/CIT+Ind Income.

#Therefore, going to try a few things:
#(0) Share of nationwide CIT revenue. then try following. Check 0.
#(1) Group by adoption.  Group early, middle and late adopters together, to see
#how timing differs. (What dependent variable?)
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
  mutate(Ann_CORPINCTX = sum(CORPINCTX))%>%
  ungroup()

Frac_CIT <- Rev1%>%
  mutate(nat_share=CORPINCTX/Ann_CORPINCTX)

#Get Summary Statistics for Real Total Tax Revenue per capita by State
summary_stats <- Rev %>%
  filter(!is.na(real_totRev_capita)) %>%  # Remove rows with NA in real_totRev_capita
  group_by(State_Name) %>%
  summarize(
    mean_real_cit_capita = mean(real_totRev_capita, na.rm = TRUE),
    median_real_cit_capita = median(real_totRev_capita, na.rm = TRUE),
    IQR_real_cit_capita = IQR(real_totRev_capita, na.rm = TRUE),
    min_real_cit_capita = min(real_totRev_capita, na.rm = TRUE),
    max_real_cit_capita = max(real_totRev_capita, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))


#Summary Statistics for nat_share, Frac_CIT
summary_stats <- Frac_CIT %>%
  filter(!is.na(nat_share)) %>%  # Remove rows with NA in nat_share
  group_by(State_Name) %>%
  summarize(
    mean_nat_share = mean(nat_share, na.rm = TRUE),
    median_nat_share = median(nat_share, na.rm = TRUE),
    IQR_nat_share = IQR(nat_share, na.rm = TRUE),
    min_nat_share = min(nat_share, na.rm = TRUE),
    max_nat_share = max(nat_share, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))



#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-Frac_CIT %>%
  select(State_Acronym,year,year_effective,State_Name,nat_share,Post)

filt_Corp <-Filter_frac

#Loop to create Dataframe for each state:

#Create State Dataframes
#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1


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

#IT WAS THE UNGROUP FROM THE EARLIEST STEP

#STEP 2, create just a point estimate loop with t-stat and confidence intervals

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  #Need to work through no variation step, THINK It is in groupings
  current_df <- current_df %>% ungroup()
  current_df <- as.data.frame(current_df)
  
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
  cat(sprintf('Point estimate: %1.5f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.4f, %1.4f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
  #  print(summary(current_tau_hat))
}

#Run again later with more decimal places.

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  # Ensure no grouping from previous operations
  current_df <- current_df %>% ungroup()
  current_df <- as.data.frame(current_df)
  
  # Group by State_Acronym and year, then compute summary statistics for nat_share across years
  grouped_df <- current_df %>%
    group_by(State_Acronym, year) %>%
    summarise(
      mean_nat_share = mean(nat_share, na.rm = TRUE),
      median_nat_share = median(nat_share, na.rm = TRUE),
      IQR_nat_share = IQR(nat_share, na.rm = TRUE),
      min_nat_share = min(nat_share, na.rm = TRUE),
      max_nat_share = max(nat_share, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Print the summary statistics for nat_share across years for each State_Acronym
  cat(sprintf('Summary statistics for nat_share in %s across years:\n', state_name))
  grouped_df %>%
    group_by(State_Acronym) %>%
    summarise(
      mean_nat_share_across_years = mean(mean_nat_share, na.rm = TRUE),
      median_nat_share_across_years = median(median_nat_share, na.rm = TRUE),
      IQR_nat_share_across_years = mean(IQR_nat_share, na.rm = TRUE),  # Aggregating IQR by mean
      min_nat_share_across_years = min(min_nat_share, na.rm = TRUE),
      max_nat_share_across_years = max(max_nat_share, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      print_output = sprintf(
        'Mean: %1.4f\nMedian: %1.4f\nIQR: %1.4f\nMin: %1.4f\nMax: %1.4f\n',
        mean_nat_share_across_years, 
        median_nat_share_across_years,
        IQR_nat_share_across_years,
        min_nat_share_across_years, 
        max_nat_share_across_years
      )
    ) %>%
    pull(print_output) %>%
    cat(sep = "\n")
}


#There are winners and losers from this policy.  What happens to share over time?
#filter for nat_share in 1976 and nat_share in 2022
bef_af <- Filter_frac %>%
  filter(year==1976 | year==2022)
write.csv(bef_af,"Share_CIT_bef_af.csv",row.names=FALSE)

#Share doesn't work well with DiD or sDiD because it isn't growing over time. 
#estimate two way fixed effect with share
reg_share <- lm(nat_share ~ Post * factor(State_Name) + factor(year), filt_Corp)
summary(reg_share)


#How can I control for rate?  The result with rate might go away.



#Got the share result, now will re-run with different grouping or dependent variable.

