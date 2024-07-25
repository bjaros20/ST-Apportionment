# Severance Tax- Two Way Fixed Effects
# will need to include all states, even those with ZERO SEVERANCE TAX, following dougan Input
#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(readxl) #load ssfa switch date
library(did) # for running DiD
library(caret) 
library(plm) # for Two way FE
library(fixest) # multiple FE
library(broom) #extract coefficients for each state

# set seed
set.seed(26)

#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Revenue CIT File
Rev <- read.csv("filled_data_jmp.csv")
#Need to merge back our Non CIT states and their data
NoCIT <- read.csv("Non_CIT_states_FRED_OH.csv")


#Merge NoCIT with Rev2, then select Relevant variables
Merge2 <- NoCIT %>%
  bind_rows(Rev)

#Now, need to just bet back to the original FRED Revenues alone
Orig <- Merge2 %>%
  select(-X,-industry_dum,-election_dum,-phase_in_dum,-rates,-Number)

#Merge Rates and Switch Data onto Original data

SSFA <- read_xlsx("ssfa_data_jmp.xlsx")

#Load rates dataframe
Rates<- read.csv("clean_rates_1976-2022.csv")

Rates2 <- read.csv("rates_jmp.csv")

#don't need to load log_Rev or Rev2, will delete them.  The main difference between 
#filled_data_jmp and detrend_per_capita about 15 logged and detrended columns.
#log revenue includes 3 more log columns.  The difference that should be merged into a default
#dataset is that that Rev alone contains all states
# Even Rev doesn't contain Texas.  Will need to go earlier.

#Orig now needs rate merged... actually will pause on that because rate is CIT rate

Sev <- Orig %>%
  select(State_Acronym,year,SVRNCTAX,state,sales,Region)

#Create df of State Acronyms and State Names
State_Acronym <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                   "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
                   "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
                   "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

state <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", 
                "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", 
                "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", 
                "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", 
                "Missouri", "Mississippi", "Montana", "North Carolina", "North Dakota", 
                "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", 
                "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                "Virginia", "Vermont", "Washington", "Wisconsin", "West Virginia", 
                "Wyoming")

state_data <- data.frame(State_Acronym, state)
print(state_data)

#Merge with Orig
Orig2 <- Orig %>%
  left_join(state_data,by=c("State_Acronym"))

Orig3 <- Orig2 %>%
  select(-state.x)%>%
  rename(state=state.y)

write.csv(Orig3,"FRED_rev_all_states.csv")

#Severance Data
Sev <- Orig3 %>%
  select(State_Acronym,year,SVRNCTAX,state,sales)

#Merge Sev with SSFA2
SSFA2 <- SSFA %>%
  select(state,year_effective)

Sev2 <- Sev %>%
  full_join(SSFA2, by= c("state"))


# Need to create Post, Treatment (no treatment columns, changing groups), and Relative Year columns

# Create a Post Column, for year effective.
Sev3 <- Sev2 %>%
  group_by(state) %>%
  mutate(post_eff = ifelse(year_effective > year, 0, 1)) %>%
  ungroup()

#Create a Relative Year column for the Event Study Plot
# Create rel_year column
Sev3$rel_year <- Sev3$year - Sev3$year_effective

write.csv(Sev3,"Severance_2WFE.csv",row.names=FALSE)

#Now run 2WFE
Sev <-read.csv("Severance_2WFE.csv")

library(readxl)
#Load Files
Pop <-read_excel("State_Resident_Population.xls",sheet=2)

#Clean Up Year for Pop
library(lubridate)
Pop <- mutate(Pop$DATE,year= year(DATE))


#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# Reshape Pop dataframe to long format
Pop_long <- Pop %>% gather(key = "state_abbreviations", value = "population", -DATE)

# Extract year from DATE column in Pop_long dataframe
Pop_long$DATE <- substr(Pop_long$DATE, 1, 4)

#Need to create an acronym column which consists of first two characters of 
#or remove POP from "state_abbreviations" column
Pop_long$state_abbreviations <-sub("POP","",Pop_long$state_abbreviations)

# Merge dataframes on 'Year' and 'State' columns
Sev_pop <- merge(Sev, Pop_long, by.x = c("year", "State_Acronym"), by.y = c("DATE", "state_abbreviations"), all.x = TRUE)

#save Severance Tax with Population
write.csv(Sev_pop,"Severance_Population.csv")


# PART II, create Severance Tax per capita, by state
Sev2 <- Sev_pop %>%
  mutate(Sev_cap=SVRNCTAX/population)

#now, filter for years 1976 through today
Sev3 <- Sev2 %>%
  filter(year>=1976)

#Replace NAs for per capita revenue with 0
Sev3$Sev_cap[is.na(Sev3$Sev_cap)] <- 0


#Simple Reg (w/o rate) (a) (i)
Simple_reg <- lm(Sev_cap ~ post_eff + factor(year) + factor(state), Sev3)
summary(Simple_reg) 

#Result   post_eff                    -24.3767    19.5580  -1.246 0.212807 

#He just wants to see, is there a relationship between severance tax revenue 
#treatment.
# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg <- lm(Sev_cap~ post_eff * factor(state) + factor(year), data = Sev3)
summary(Interaction_reg)

# It is close, just trying to get Alabama
# Set Alabama as the reference category
Sev3$state <- relevel(factor(Sev3$state), ref = "Alabama")

# Interaction Reg (with interaction between Post and state)
Interaction_reg2 <- lm(Sev_cap ~ post_eff * factor(state) + factor(year), data = Sev3)
summary(Interaction_reg2)



#Extract coefficients for each state using Broom
tidy_model <- tidy(Interaction_reg2)

# Filter the coefficients to get only those related to Post
post_coefficients <- tidy_model %>%
  filter(grepl("post_eff", term))

print(post_coefficients)

#Extracting the post coefficient for each state
# Define the model function with added checks
fit_state_model <- function(df) {
  df <- df %>%
    droplevels() %>%
    filter(n_distinct(state) > 1, n_distinct(year) > 1) # Ensure factors have >1 level
  
  # Diagnostic message
  cat("Processing state:", unique(df$state), "\n")
  cat("Number of distinct states:", n_distinct(df$state), "\n")
  cat("Number of distinct years:", n_distinct(df$year), "\n")
  cat("Number of rows:", nrow(df), "\n\n")
}
# Split data by state and fit model for each state
state_models <- Sev3 %>%
  group_by(state) %>%
  group_map(~ fit_state_model(.x))

# Remove any NULL models (if any states had insufficient data)
state_models <- state_models[!sapply(state_models, is.null)]

# Extract coefficients for Post from each model
post_coeffs <- lapply(state_models, function(model) {
  tidy(model) %>% filter(term == "post_eff")
})

# Combine results into a single data frame
post_coeffs_df <- bind_rows(post_coeffs, .id = "state")

print(post_coeffs_df)

#REMOVES STATES THAT DON't HAVE TREATMENT, will try with simple regression tomorrow.

write.csv(Sev3,"Severance_Cap_mutate.csv")
