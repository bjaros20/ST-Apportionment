#Check Our Apportionment to Girouds, then merge them

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#loadpackages, giroud and your apportionment rates
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)

Giroud <- read_csv("Giroud.csv")

My_app <-read_csv("SalesFactor_Weight_BJ.csv")

#Clean My apportionment Table
App_clean <- My_app %>% filter(Year >=1985,Year <=2023)

#Convert All Columns to Numeric
App_cleanNum <- App_clean %>% mutate_if(is.character, as.numeric)

#convert Apportionment Ratios to numbers like Giroud
Num_App3 <- App_cleanNum %>%
    mutate(across(!Year,~.*100))

#round to 2 Decimal Places
round_App2 <-round(round_App,digits = 2)

#Replace Sales factor weight 33.33 with 33.34 to match Giroud
match_round <- round_App2 %>%
  mutate(across(everything(), ~ifelse(. == 33.33, 33.34, .)))

#SaveCleaned Wide table
write.csv(match_round,file="My_App_Wide.csv",row.names=FALSE)




