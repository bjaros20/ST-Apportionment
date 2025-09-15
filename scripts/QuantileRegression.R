#Run Quantile Regression
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
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")

#Load Files
Pop <-read_excel("State_Resident_Population2.xls",sheet=1)

Real_Rev <-read.csv("Real_SCIT_Rev.csv")

#Clean Up Year for Pop
install.packages("lubridate")
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


#back to me: Make it Real Revenue Per capita
#Adjust for Inflation
# Convert columns to numeric
merged_df$RealRev <- as.numeric(merged_df$RealRev)
merged_df$population <- as.numeric(merged_df$population)

# Perform division to calculate RealRevPerCapita
merged_df$RealRevPerCapita <- merged_df$RealRev / merged_df$population

write.csv(merged_df,"Real_SCIT_per_Capita.csv",row.names = FALSE)
