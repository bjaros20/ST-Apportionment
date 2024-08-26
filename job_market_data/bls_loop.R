#Create a loop for the bls employment results by state

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

#Load Detrended, per capita, real revenue data, bls data
Rev <- read.csv("detrend_per_capita.csv")
naive_ci<-read.csv("naive_ci.csv")
bls <-read.csv("bls_NoID_complete.csv")

filter_bls <-read.csv("bls_post.csv")

#remove x and resave bls sheet after adding post from Rev
Post <- Rev%>%
select(State_Name,Post,year,year_effective,State_Acronym)


#merge, full join Post to bls on State_Name and year
bls_merge <- bls %>%
  full_join(Post, by = c("GeoName" = "State_Name", "year" = "year"))


#left join eliminates USA and filters down to just 2001-2022
bls_merge2 <- bls %>%
  left_join(Post, by = c("GeoName" = "State_Name", "year" = "year"))

filter_bls <-bls_merge2 %>%
  select(-X)

# Remove leading "X" from column names from the merge
colnames(filter_bls) <- gsub("^X", "", colnames(filter_bls))

#remove leading periods from spaces
colnames(filter_bls) <- gsub("^X|\\.+", "", colnames(filter_bls))

write.csv(filter_bls,"bls_post.csv", row.names = FALSE)

filter_bls <-filter_bls %>% select(-X)

#Create an industry vector for loop
# Create a vector with the specified column names
selected_columns <- c(
  "Totalemploymentnumberofjobs",
  "Wageandsalaryemployment",
  "Proprietorsemployment",
  "Farmproprietorsemployment",
  "Nonfarmproprietorsemployment2",
  "Farmemployment",
  "Nonfarmemployment",
  "Privatenonfarmemployment",
  "Forestryfishingandrelatedactivities",
  "Miningquarryingandoilandgasextraction",
  "Utilities",
  "Construction",
  "Manufacturing",
  "Wholesaletrade",
  "Retailtrade",
  "Transportationandwarehousing",
  "Information",
  "Financeandinsurance",
  "Realestateandrentalandleasing",
  "Professionalscientificandtechnicalservices",
  "Managementofcompaniesandenterprises",
  "Administrativeandsupportandwastemanagementandremediationservices",
  "Educationalservices",
  "Healthcareandsocialassistance",
  "Artsentertainmentandrecreation",
  "Accommodationandfoodservices",
  "Otherservicesexceptgovernmentandgovernmententerprises",
  "Governmentandgovernmententerprises",
  "Federalcivilian",
  "Military",
  "Stateandlocal",
  "Stategovernment",
  "Localgovernment"
)

# View the vector
selected_columns

#rename GeoName column to State_Name
filter_bls <- filter_bls %>%
  rename(State_Name = GeoName)


#Start Here
#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-filter_bls %>%
  select(State_Acronym,year,year_effective,State_Name,Totalemploymentnumberofjobs,Post)


#Now create a BLS loop, loop through the selected vectors
filt_Corp <-Filter_frac


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
  
  #filter out states with year_effective before 2001
  df <- df %>%
    filter(year_effective>=2001)
  
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


##sDID might not WORK WELL, because there isn't much pre-period to match on.
# might Estimate TWFE

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  #Make the tibble from the result list a dataframe, easier for panel.matrics function
  current_df <- as.data.frame(current_df)
  
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "Totalemploymentnumberofjobs", "Post")])
  
  #eliminate Inf lines and the state that has them for estimation, like Ohio 2009-2013
  states_with_inf <- current_df %>%
    group_by(State_Acronym) %>%
    filter(any(is.infinite(Totalemploymentnumberofjobs))) %>%
    pull(State_Acronym) %>%
    unique()
  
  # Filter out those states from the dataframe
  current_df <- current_df %>%
    filter(!State_Acronym %in% states_with_inf)
  
  
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "Totalemploymentnumberofjobs", treatment = "Post")
  
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
  
}

#Summary Statistics for otalemploymentnumberofjobs
summary_stats <- filter_bls %>%
  filter(!is.na(Totalemploymentnumberofjobs) & is.finite(Totalemploymentnumberofjobs)) %>%  # Remove rows with NA in nat_share
  group_by(State_Name) %>%
  summarize(
    mean = mean(Totalemploymentnumberofjobs, na.rm = TRUE),
    median = median(Totalemploymentnumberofjobs, na.rm = TRUE),
    IQR = IQR(Totalemploymentnumberofjobs, na.rm = TRUE),
    min = min(Totalemploymentnumberofjobs, na.rm = TRUE),
    max = max(Totalemploymentnumberofjobs, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))


#Estimate TWFE from bls 
# Remove rows with any missing values in the relevant columns
filter_bls_clean <- filter_bls %>%
  filter(!is.na(Totalemploymentnumberofjobs) & !is.na(Post) & 
           !is.na(year) & !is.na(state_name))
#Simple Reg (w/o rate) (a) (i)
Simple_reg <- lm(Totalemploymentnumberofjobs ~ Post + factor(year) + factor(State_Name), data = filter_bls_clean)
summary(Simple_reg) 

#Result   Post                               151661      43038   3.524 0.000446 ***

# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg <- lm(Totalemploymentnumberofjobs~ Post * factor(State_Name) + factor(year), data = filter_bls_clean)
summary(Interaction_reg)



#Simple Reg (w/o rate) (a) (i)
filter_bls_man <- filter_bls %>%
  filter(!is.na(Manufacturing) & Manufacturing != "(D)" &
           !is.na(Post) & !is.na(year) & !is.na(state_name))
Man_reg <- lm(Manufacturing ~ Post + factor(year) + factor(State_Name), data = filter_bls_man)
summary(Man_reg) 


