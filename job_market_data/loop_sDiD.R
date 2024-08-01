# Create sDiD loop for Corporate income Tax collections
#can re-run loop for no spillovers

# make correlation matrix, https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
#and Synthetic DiD, https://synth-inference.github.io/synthdid/

#Data, will load this csv "detrend_per_capita.csv", contains detrended, per capita, real revenue

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

install.packages("ggthemes")
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X,-X.1)

#Load all rev
#All_rev <-read.csv("FRED_rev_all_states.csv")

#for Corporate, will just need CIT_Rev, currently in the "Rev" dataframe. NO WY, WA, TX, SD, OH, NV
Corp <- Rev %>%
  select(State_Acronym,year,CORPINCTX,sales:log_CIT,year_effective,Post:rel_year,State_Name,real_cit,logRealCitRev,real_cit_capita,detrended_CIT_capita)
write.csv(Corp,"CIT_sDID_loop.csv",row.names=FALSE)

Corp <-read.csv("CIT_sDID_loop.csv")

#filter Corporate
filt_Corp <- Corp %>%
  select(State_Acronym,year,year_effective,Post,real_cit_capita,State_Name)


#Step 1- Create Dataframe for the state
#group_by(State_Acronym)
#select state for treatment- start with earliest filt_Corp$year_effective
#filter- remove all State_Acronym < year_effecive for treatment state

#1a create a dataframe named after treatment state, call dataframe value in "State_Name"

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




# Step 2- Use that dataframe to create point estimate, se, CI, & summary statistics for state

#Point estimate and summary statistics list
point_estimate_list <- list()

#sDiD panel Matrice
Iowa_sDiD <- panel.matrices(Iowa,unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "Post")
#sDiD
Iowa_tau.hat = synthdid_estimate(Iowa_sDiD$Y, Iowa_sDiD$N0, Iowa_sDiD$T0)
se = sqrt(vcov(Iowa_tau.hat, method='placebo'))

#point Estimate
sprintf('point estimate: %1.2f', Iowa_tau.hat)
#Confidence Interval
sprintf('95%% CI (%1.2f, %1.2f)', Iowa_tau.hat - 1.96 * se, Iowa_tau.hat + 1.96 * se)
#summary statistics
print(summary(Iowa_tau.hat))



#Try point estimate loop
point_estimate_list <- list()

#loop for cuts from dataframes that are not balanced
cut_data_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  
  # Print the point estimate and confidence interval
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  
  # Summary statistics
  print(summary(current_tau_hat))
  
  # Store the results in point_estimate_list
  point_estimate_list[[state_name]] <- list(
    point_estimate = current_tau_hat,
    se = se,
    CI_lower = current_tau_hat - 1.96 * se,
    CI_upper = current_tau_hat + 1.96 * se,
    summary = summary(current_tau_hat)
  )
   
}




#Step 3- Plot Loop
plot(Iowa_tau.hat)+labs(x="Year",y="Real CIT Revenue per Capita") + ggtitle("Synthetic DiD Plot- Iowa = Treated") +theme_fivethirtyeight()





