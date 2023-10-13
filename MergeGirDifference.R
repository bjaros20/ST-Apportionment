# Merge Clean years w/ Rest of Giroud, Plot Formula over Time

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#loadpackages, giroud and your apportionment rates
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

#Load Documents
Wide_app2 <-read_csv("My_App_Wide2.csv")

Giroud <- read_csv("Giroud.csv")

Giroud_States <-read_csv("Giroud_Just_Appt_States.csv")


#Convert Clean Wide panel to Clean Long Panel
Long_app <- Wide_app2 %>%
  pivot_longer(cols= "Alabama":"Wyoming",names_to = "State", values_to ="sales")

Long_App2 <-Long_app[order(Long_app$State),]

#save New Long panel
write.csv(Long_App2,file="Oct_App_Long.csv",row.names=FALSE)


#First, condense Giroud to Just Apportionment
Giroud_Just_Appt_Pre85 <- Giroud %>%
  select(state_name,year,sales) %>%
  filter(year<=1985)

#rename state column
Giroud_Just_Appt_Pre85 <- Giroud_Just_Appt_Pre85 %>%
  rename(state=state_name)

#change year columns
My_App <- Long_App2 %>%
  rename(year=Year)

My_App <- My_App %>%
  rename(state=State)

#Cut DC from Giroud
Just_States_Giroud <- Giroud_Just_Appt_Pre85 %>%
  filter(state !="District of Columbia")

#filter out 1985
Pre85 <- Just_States_Giroud %>%
  filter(year<1985)


#Merge Giroud and Long App
merged_df <-merge(Just_States_Giroud,My_App, by = c("year","state","sales"))

#^ Found out that 1985 was only year that merged on, so changed Giroud to Pre85.  Want a complete panel

#Create an Added Long Panel
combined_df <- rbind(Pre85,My_App)

#Eliminate NAs
combined_clean_df <-na.omit(combined_df)


#save as CSVs
write.csv(combined_df,file="Long_Gir_My_App_Comb.csv",row.names=FALSE)

write.csv(combined_clean_df,file="Long_Gir_My_App_Comb_clean.csv",row.names=FALSE)


