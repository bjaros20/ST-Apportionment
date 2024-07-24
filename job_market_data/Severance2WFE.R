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


