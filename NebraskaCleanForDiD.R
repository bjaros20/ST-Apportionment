#Attempt to Clean before DiD_ Nebraska
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("did")
install.packages("tidyverse")
install.packages("lmtest")
library(readxl)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Apportionment Document
All_App_df <-read_csv("SalesFactorWeights_ALLStates.csv")

CIT_Rev_df <-read.csv("SCIT_Rev_Long.csv")

#Make Revenue Log Column
CIT_Rev_df$Log_Revenue <-log(CIT_Rev_df$Revenue)

#Year Sections
CIT_Rev_Year <-subset(CIT_Rev_df,Year >=1976 & Year <=2007)

All_App_Year <-subset(All_App_df,year >=1976 & year <=2007)

# Assuming CIT_Rev_Year has a State column with U.S. state abbreviations
# If not, replace it with your actual column name

# Create a mapping of state abbreviations to full names
state_mapping <- data.frame(StateAbbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                            StateFullName = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))

# Merge CIT_Rev_Year with the state mapping to add a StateFullName column
CIT_Rev_Year <- merge(CIT_Rev_Year, state_mapping, by.x = "State", by.y = "StateAbbrev", all.x = TRUE)

# Assuming All_App_Year has a year column
# If not, replace it with your actual column name

colnames(All_App_Year)[colnames(All_App_Year) == "year"] <- "Year"

colnames(All_App_Year)[colnames(All_App_Year) == "state"] <- "StateFullName"

# If not, replace them with your actual column names

merged_df <- merge(CIT_Rev_Year, All_App_Year, by = c("StateFullName", "Year"))

#Clean Up Panel
#Remove State abbreviation
merged_df <- subset(merged_df, select = -State)
# Assuming "StateFullName" is the column you want to rename
colnames(merged_df)[colnames(merged_df) == "StateFullName"] <- "State"

# Assuming merged_df is the data frame you want to save
write.csv(merged_df, file = "NE_DiD_Set_up.csv", row.names = FALSE)

