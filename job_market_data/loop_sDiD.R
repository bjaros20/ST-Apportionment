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

#filter Corporate
filt_Corp <- Corp %>%
  select(State_Acronym,year,year_effective,treatment,real_cit_capita,State_Name)


#Step 1- Create Dataframe for the state
#groub_by(State_Acronym)
#select state for treatment- start with earliest filt_Corp$year_effective
#filter- remove all State_Acronym < year_effecive for treatment state



#1a create a dataframe named after treatment state, call dataframe value in "State_Name"

#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
df <- filt_Corp

while (nrow(df) > 0) {
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  treatment_state <- df %>% slice(1) # Get the first row
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state$State_Name
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  
  # Remove all State_Acronyms with year_effective <= treatment_year
  df <- df %>% filter(!(State_Acronym %in% treatment_state$State_Acronym & year_effective <= treatment_year))
  
  # Filter out State_Acronyms for 3 years after treatment_year
  df <- df %>% filter(!(year_effective <= treatment_year + 3))
  
  # Filter out rows with year <= 3 years after treatment_year
  df <- df %>% filter(year > treatment_year + 3)
}





# Step 2- Use that dataframe to create point estimate, se, CI, & summary statistics for state
#sDiD panel Matrice
setup1 <- panel.matrices(filter_total3,unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

#sDiD
tau.hat = synthdid_estimate(setup1$Y, setup1$N0, setup1$T0)
se = sqrt(vcov(tau.hat, method='placebo'))

#point Estimate
sprintf('point estimate: %1.2f', tau.hat)
#Confidence Interval
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
#summary statistics
print(summary(tau.hat))







#Step 4- Plot Loop
plot(tau.hat)+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Plot- 2007 Switchers = Treated")





