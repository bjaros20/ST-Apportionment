#Attempt to Get Cross Partial of State CIT Collections, Other Rev w.r.t Sales Factor Weight
# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(readxl)
library(tidyr)
library(conflicted)
library(dplyr)
library(tidyverse)
library(stringr)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/CrossPartial")

#Read in Df
Rev <- read.csv("FRED_SALT_Rev_Feb_2024.csv")

App <- read_xlsx("Average_SF_Weight_By_Region.xlsx")

# create a vector of state acronyms
state_acronyms <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", 
                    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                    "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
                    "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
                    "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

#Need to Pair State Acronyms to States and Merge
state_df <- data.frame(
  State_Acronym = state_acronyms,
  State_Name = c("Alaska", "Alabama", "Arkansas", "Arizona", "California",
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Iowa", "Idaho", "Illinois", "Indiana", "Kansas",
                 "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine",
                 "Michigan", "Minnesota", "Missouri", "Mississippi", "Montana",
                 "North Carolina", "North Dakota", "Nebraska", "New Hampshire",
                 "New Jersey", "New Mexico", "Nevada", "New York", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Vermont",
                 "Washington", "Wisconsin", "West Virginia", "Wyoming")
)

#Rename Column for Merge
state_df <- state_df %>%
      rename("state"="State")
App_Merge <- merge(App,state_df,by="state")

#Rename Acronym on App Df for merge with Revenue Df
Rev <- Rev %>%
    rename ("State_Acronyn"="state")
#Mistake
Rev <- Rev %>%
  rename ("State_Acronym"="State_Acronyn")

#Edit Date of Rev to remove last 6 characters,
Rev$DATE <- str_sub(Rev$DATE,end=-6)
Rev$DATE <- str_sub(Rev$DATE,end=-2)

#convert Date from Character to Numeric,
Final= as.numeric(Rev$DATE)
print(Final)
class(Final)

#and change column name to year
Rev <- Rev %>%
  rename ("year"="DATE")

#and Trim Rev DF to only Include 1976 onwards

#Subset columns in App_Merge df to make for cleaner merge
App_Merge2 = subset(App_Merge,select = -c(Number,Region))

#year in Rev column is still "character"
Rev$year <- as.numeric(Rev$year)
print(Rev)

# Merge on State_Acronym and year
Rev2 <- Rev %>%
  full_join(App_Merge2, by=c("State_Acronym","year"))

#save CSV from tonight
write.csv(Rev2,file="SALT_App_Merge_2024.csv")
