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

#Change full Date to Year
#Pop$DATE <- as.Date(Pop$DATE)
#transform(Pop, date=format(DATE "%d")),
#           month= format(DATE, "%m"), year=format(DATE, "Y"))
#Pop$Year <-substr(Pop$DATE, 1 ,4)

#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

#ChatGPT Start



# Reshape Pop dataframe to long format
Pop_long <- Pop %>% gather(key = "State", value = "population", -DATE)

# Extract year from DATE column in Pop_long dataframe
Pop_long$DATE <- substr(Pop_long$DATE, 1, 4)

# Merge dataframes on 'Year' and 'State' columns
merged_df <- merge(Real_Rev, Pop_long, by.x = c("Year", "State"), by.y = c("DATE", "State"), all.x = TRUE)



#Merge with population data

pop <- read.csv("rev_population.csv")

Rpop <-pop %>%
  select()


#Simple Reg (w/o rate) (a) (i)
Simple_reg <- lm(log_totrev ~ Post + factor(year) + factor(state), Res)
summary(Simple_reg)
#Result Post              0.01614    0.01386   1.165 0.244183 


#Simple reg with rate, ran this and positive result for post went away (a) (ii)
factor_rate <- lm(log_totrev ~ Post + factor(year) + factor(state) + rates, Res)
summary(factor_rate)


#He just wants to see, do these states increase their revenue post
# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Res3)
summary(Interaction_reg)

# It is close, just trying to get Alabama
# Set Alabama as the reference category
Res3$State_Name <- relevel(factor(Res3$State_Name), ref = "Alabama")

# Interaction Reg (with interaction between Post and state)
Interaction_reg2 <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Res3)
summary(Interaction_reg2)



#Extract coefficients for each state using Broom
tidy_model <- tidy(Interaction_reg2)

# Filter the coefficients to get only those related to Post
post_coefficients <- tidy_model %>%
  filter(grepl("Post", term))

print(post_coefficients)

#Extracting the post coefficient for each state
fit_state_model <- function(df) {
  lm(log_totrev ~ Post + factor(year), data = df)
}

# Split data by state and fit model for each state
state_models <- Res3 %>%
  group_by(State_Name) %>%
  group_map(~ fit_state_model(.x))

# Extract coefficients for Post from each model
post_coeffs <- lapply(state_models, function(model) {
  tidy(model) %>% filter(term == "Post")
})

# Combine results into a single data frame
post_coeffs_df <- bind_rows(post_coeffs, .id = "State_Name")

print(post_coeffs_df)