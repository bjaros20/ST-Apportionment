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
library(fixest) # for stacked DiD
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

naive_ci<-read.csv("naive_ci.csv")

#create log real naive CI
real_log_nci <- naive_ci %>%
  mutate(real_log_nci = log(real_ci))

write.csv(real_log_nci,"real_log_nci.csv", row.names = FALSE)

#filter to necessary variables
filt_Corp <-real_log_nci %>%
  select(State_Acronym,year,year_effective,State_Name,real_log_nci,Post, rel_year)


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


# Stacked DiD loop
# Initialize an empty dataframe to store the stacked event-level data
stacked_df <- data.frame()

# Order states by year_effective to ensure chronological processing
ordered_states <- filt_Corp %>%
  filter(!is.na(year_effective)) %>%  # Only include states with a defined year_effective
  filter(year_effective >= 1980 & year_effective <= 2018) %>%  # Keep only events between 1980 and 2018, removes IA and late switchers
  arrange(year_effective) %>%
  distinct(State_Acronym, year_effective)

# Loop through each state and its treatment year
for (i in seq_len(nrow(ordered_states))) {
  # Define the state of interest and its treatment year
  state_of_interest <- ordered_states$State_Acronym[i]
  treatment_year <- ordered_states$year_effective[i]
  
  # 1. Define the Treated Event for the current state
  treatment_event <- filt_Corp %>%
    filter(State_Acronym == state_of_interest & rel_year >= -4 & rel_year <= 4) %>%
    mutate(treated = 1,  # Mark as treated
           event_id = paste(state_of_interest, treatment_year, sep = "_"),
           rel_year_did = rel_year)  # Unique event ID
  
  # 2. Define the Clean Control Group
  remaining_control <- filt_Corp %>%
    filter(State_Acronym != state_of_interest) %>%  # Exclude treatment state
    filter(is.na(year_effective) | year_effective > (treatment_year + 4)) %>%  # Exclude states with SSFA adoption within +4 years
    # Additional Filters for Ohio
    filter(!(State_Acronym == "OH" & year < 2008)) %>%  # Remove Ohio for all years prior to 2008, Oh always giving problems
    filter(!(State_Acronym == "OH" & year_effective > 2004)) # 
  
  # Step (2): Keep only observations within the -4 to +4 year window relative to the treatment year
  clean_control <- remaining_control %>%
    filter(year >= (treatment_year - 4) & year <= (treatment_year + 4)) %>%
    mutate(treated = 0,  # Mark as control
           event_id = paste(state_of_interest, treatment_year, sep = "_"),
           rel_year_did = year - treatment_year)  # Use same event ID
  
  # Combine the treatment and control data for the current state’s event
  event_data <- bind_rows(treatment_event, clean_control)
  
  # Append the event data to the stacked dataframe
  stacked_df <- bind_rows(stacked_df, event_data)
}

na_summary <- stacked_df %>%
  summarize_all(~ sum(is.na(.)))

# View the summary to see where NA values are located
print(na_summary)


write.csv(stacked_df,"stacked__nci_DiD.csv",row.names=FALSE)

stacked_df  <- read.csv("stacked__nci_DiD.csv")

# Part II- use Fixest to estimate Stacked_df

# `stacked_df` is the data. `real_log_nci` id dependent variable of interest, 
# `treated` = treated state in that event, `post' gives the treatment effect
# rel_year`, `
# event_id` unique for each treated state and clean control group, and `year`

# Create a unique identifier for each year and event_id combination
stacked_df <- stacked_df %>%
  mutate(year_event_id = paste(year, event_id, sep = "_"))



#
library(plm)

# Convert data to pdata.frame
stacked_df2 <- pdata.frame(stacked_df, index = c("event_id", "State_Acronym", "year"))

# Run model without interaction in plm
model_plm <- plm(
  real_log_nci ~ treated + i(rel_year_did, ref = -1),  # No interaction term
  data = stacked_df2,
  model = "within",  # Fixed-effects model
  effect = "twoways"  # Include both "event_id" and "year_event_id"
)

summary(model_plm)


#Coefficients:
#Estimate Std. Error t-value  Pr(>|t|)    
#treated                      0.018902   0.027045  0.6989   0.48464    
#i(rel_year_did, ref = -1)-4 -0.170329   0.018909 -9.0077 < 2.2e-16 ***
#  i(rel_year_did, ref = -1)-3 -0.131698   0.018909 -6.9648 3.656e-12 ***
#  i(rel_year_did, ref = -1)-2 -0.074603   0.018909 -3.9454 8.063e-05 ***
#  i(rel_year_did, ref = -1)0   0.031089   0.018909  1.6441   0.10020    
#i(rel_year_did, ref = -1)1   0.034240   0.018909  1.8108   0.07023 .  
#i(rel_year_did, ref = -1)2  -0.015696   0.018909 -0.8301   0.40654    
#i(rel_year_did, ref = -1)3  -0.040829   0.018909 -2.1592   0.03087 *  
 # i(rel_year_did, ref = -1)4  -0.012788   0.018909 -0.6763   0.49889   

#Plot for Stacked DiD
library(broom)


# Extract coefficients related to `rel_year_did`
tidy_model <- broom::tidy(model_plm)

# Filter only for the `rel_year_did` coefficients and add -1 as the reference year
plot_data <- tidy_model %>%
  filter(grepl("rel_year_did", term)) %>%  # Keep only rel_year_did terms
  mutate(
    rel_year_did = as.numeric(gsub(".*\\)", "", term)),  # Extract year from term name
    conf.low = estimate - 1.96 * std.error,  # Lower bound of 95% CI
    conf.high = estimate + 1.96 * std.error   # Upper bound of 95% CI
  )

# Check output
print(plot_data)

# Plot

ggplot(plot_data, aes(x = rel_year_did, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Two-Way Fixed-Effects Plot of SSFA",
    x = "Event Time",
    y = "Estimated Effect on Log(Corporate Income)"
  ) +
  scale_x_continuous(breaks = -4:4) +  # Ensure all event years from -4 to 4 appear on x-axis
  theme_stata()







