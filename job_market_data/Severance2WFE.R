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

#Replace Sales NA with 0
Sev3$sales[is.na(Sev3$sales)] <- 0

Sev3 <- Sev3 %>% select(-Sev_sales)

#Create log Severance Tax Variable
Sev3 <- Sev3 %>%
  mutate(Sev_log = log(SVRNCTAX))

# Sev log going to 0
Sev3$Sev_log[is.na(Sev3$Sev_log)] <- 0




#Just try simple regression, Sev per capita
Reg_cap <- lm(Sev_cap ~ sales + factor(year) + factor(state), Sev3)
summary(Reg_cap) 

#interaction between State and Sales Factor Weight
Int_reg_cap <- lm(Sev_cap~ sales * factor(state) + factor(year), data = Sev3)
summary(Int_reg_cap)

#Regression for Log Revenue
Reg_log <- lm(Sev_log ~ sales + factor(year) + factor(state), Sev3)

#Diagnostics for why it wasn't running
sum(is.na(Sev3$Sev_log))
sum(is.nan(Sev3$Sev_log))
sum(is.infinite(Sev3$Sev_log))


#WANT SUMMARY STATISTICS ON Sev_cap and Sev_log
summary(Sev3$Sev_cap)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -Inf   0.000   8.227    -Inf  11.204  16.208 

summary(Sev3$Sev_log)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000     0.000     0.885   105.557    19.155 10093.810 

#Simple log revenue wouldn't run, have to eliminate zero results. Saved those as dataframe.
#Running as Sev4.
infinite_values_df <- Sev3 %>%
  filter(is.infinite(Sev_log))
write.csv(infinite_values_df, "No_Sev_revenue.csv")

#Run as Sev 4, removing those values
Sev4 <- Sev3 %>%
  filter(!is.infinite(Sev_log))

#Run again
Reg_log <- lm(Sev_log ~ sales + factor(year) + factor(state), Sev4)
summary(Reg_log)


#Going to try this again with just a switch dummy
Sev5 <- Sev4 %>%
  group_by(state) %>%
  mutate(switch=ifelse(any(post_eff == 1), 1, 0))%>%
  ungroup()

#Any switch with NA, replace with 0
Sev5$switch[is.na(Sev3$switch)] <- 0

Sev5 <- Sev5 %>%
  mutate(switch = ifelse(is.na(switch), 0, switch))

#Average Sev_Cap Revenue whether switch or not
avg_Sev_cap_by_switch <- Sev5 %>%
  group_by(switch) %>%
  summarize(avg_Sev_cap = mean(Sev_cap, na.rm = TRUE))

# View the result
print(avg_Sev_cap_by_switch)


Sev5$state <- as.factor(Sev5$state)

# Get states with switch = 1
states_switch_1 <- Sev5 %>%
  filter(switch == 1) %>%
  distinct(state) %>%
  pull(state)

# Get states with switch = 0
states_switch_0 <- Sev5 %>%
  filter(switch == 0) %>%
  distinct(state) %>%
  pull(state)

# Create a dataframe
states_switch_df <- data.frame(
  switch_1 = states_switch_1,
  switch_0 = c(states_switch_0, rep(NA, length(states_switch_1) - length(states_switch_0)))
)

# View the result
print(states_switch_0)

# How likely is a state to switch, dependent upon their Sev tax rev per person
# Fit a logistic regression model
logistic_model <- glm(switch ~ Sev_cap, data = Sev5, family = binomial)

# Summary of the model
summary(logistic_model)

exp(coef(logistic_model))


#TRY with LOG
# How likely is a state to switch, dependent upon their Sev tax rev per person
# Fit a logistic regression model
logistic_model1 <- glm(switch ~ Sev_log, data = Sev5, family = binomial)

# Summary of the model
summary(logistic_model1)

exp(coef(logistic_model1))

