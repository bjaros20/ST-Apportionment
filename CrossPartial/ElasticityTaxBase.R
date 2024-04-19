# Get rate of change for tax bases and find elasticity of tax base w.r.t apportionment and rate changes.
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#New Directory
setwd("~/Documents/GitHub/ST-Apportionment/CrossPartial")

# Read in DF
Rev <- read.csv("Clean_Rev_Event_4_16_24.csv")
App <- read_xlsx("SalesFactorWeights_ALLStates_Apri_2024.xlsx")
AllYears <- read.csv("SALT_App_Merge_2024.csv")

#Clean Up All Years



# Have different Match.  Need to match on State Acronym and State
#Create Acronym Dataframe
state_data <- data.frame(
  acronym = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)
# Print the dataframe
print(state_data)

#Merge Acronym to State Names
Merge <- left_join(Rev,state_data, by = c("State_Acronym" = "acronym"))

#Merge Apportionment on Year and State,  Want to 

AppRev <-left_join(Merge,App,by = c("state_name"="state","year"))

#filter out State, sales.x and Number
filterAppRev<-AppRev %>%
  select(-state,-sales.x,-Number)

#Create Percentage Change Columns
filterAppRev2<- filterAppRev %>%
  group_by(state_name)%>%
  arrange(state_name,year)%>%
  mutate(perc_change_Sales = ((sales.y-lag(sales.y))/((sales.y+lag(sales.y))/2)))


SalesChange <-filterAppRev2 %>%
  filter(perc_change_Sales != 0 | lag(perc_change_Sales) != 0) %>%
  slice(c(which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0) - 1,
          which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0)))
