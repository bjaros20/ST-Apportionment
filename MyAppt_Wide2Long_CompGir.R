#Convert My appt from Wide to Long and Compare to Giroud

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#loadpackages, giroud and your apportionment rates
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)

Wide_app <-read_csv("My_App_Wide.csv")

Giroud <- read_csv("Giroud.csv")

#Convert Wide to Long Panel
Long_app <- Wide_app %>%
  pivot_longer(cols= "Alabama":"Wyoming",names_to = "State", values_to ="sales")

Long_App2 <-Long_app[order(Long_app$State),]

#save Long panel
write.csv(Long_App2,file="My_App_Long.csv",row.names=FALSE)

#First, condense Giroud to Just Apportionment
Giroud_Just_Appt <- Giroud %>%
  select(state_name,year,sales) %>%
  filter(year>=1985,year <=2013)

#rename state column
Giroud_Just_Appt <- Giroud_Just_Appt %>%
  rename(state=state_name)

#cut years after 2012 from My app
My_App_Pre13 <- Long_App2 %>%
  filter(Year>=1985,Year <=2012)

#change year columns
Pre13 <- My_App_Pre13 %>%
  rename(year=Year)

Pre13 <- Pre13 %>%
  rename(state=State)


#Cut DC from Giroud
Just_States_Giroud <- Giroud_Just_Appt %>%
  filter(state !="District of Columbia")

#Compare Differences between Giroud and My_App_Long
differences_df <-anti_join(Giroud_Just_Appt,Pre13)

different_sales2 <- anti_join(Just_States_Giroud,Pre13, by =c("year","state"))%>%
  select(year,state,sales.x=sales,sales.y=sales)

#Trying Different Approach
different_sales2 <- Just_States_Giroud %>%
  filter(!(year %in% Pre13$year & state %in% Pre13$state & sales %in% Pre13$sales))

merged_df <-merge(Just_States_Giroud,Pre13, by = c("year","state"),suffixes = c(".Gir",".MyAp"))

#Different Sales DF
different_sales_df <- merged_df[merged_df$sales.Gir != merged_df$sales.MyAp, ]
rownames(different_sales_df) <- NULL

#Eliminate NAs

cleaned_Differences <-na.omit(different_sales_df)

#Sort by State
cleaned_Differences <-cleaned_Differences %>%
  arrange(state)


#Save CSVs from this session

write.csv(cleaned_Differences,file="Giroud_MyApp_differences.csv",row.names = FALSE)

write.csv(Just_States_Giroud,file="Giroud_Just_Appt_States.csv",row.names = FALSE)

write.csv(Pre13,file="My_App_Pre2013.csv",row.names=FALSE)
