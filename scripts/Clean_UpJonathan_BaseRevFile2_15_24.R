#Second Week Cleaning Attempt, want to eliminate TX: 
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Apportionment Document
All_App_df <-read_csv("SalesFactorWeights_ALLStates.csv")

CIT_Rev_df <-read.csv("SCIT_Rev_Long.csv")

#Make Revenue Log Column
CIT_Rev_df$Log_Revenue <-log(CIT_Rev_df$Revenue)

# Create a mapping of state abbreviations to full names
state_mapping <- data.frame(StateAbbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                            StateFullName = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))

# Merge CIT_Rev_Year with the state mapping to add a StateFullName column
CIT_Rev_aLL <- merge(CIT_Rev_df,state_mapping, by.x = "State", by.y = "StateAbbrev", all.x = TRUE)

# Assuming All_App_Year has a year column
# If not, replace it with your actual column name

colnames(All_App_df)[colnames(All_App_df) == "year"] <- "Year"

colnames(All_App_df)[colnames(All_App_df) == "state"] <- "StateFullName"


# If not, replace them with your actual column names

merged_df <- merge(CIT_Rev_aLL, All_App_df, by = c("StateFullName", "Year"))

#Clean Up Panel
#Remove State abbreviation
merged_df <- subset(merged_df, select = -State)
# Assuming "StateFullName" is the column you want to rename
colnames(merged_df)[colnames(merged_df) == "StateFullName"] <- "State"

#Remove Texas, it is not a Corporate Income Tax State, the df only has 8 years and the revenue for each of those years is 0
merged_df1<-merged_df%>%
  filter(State !="Texas")

# Assuming merged_df is the data frame you want to save
write.csv(merged_df1, file = "LogRev_Appt_Merge_1976-2021.csv", row.names = FALSE)

