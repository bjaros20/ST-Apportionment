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
Num_app <- App_clean %>%
  mutate(across(-Year,~ifelse(is.numeric(.),.*100,.)))

#check Alabama Column
is.numeric(Num_App2$Alabama)
Num_App2 <-transform(Num_app,Alabama=as.numeric(Alabama))

Num_App2$Alabama <- Num_App2$Alabama * 100

#In morning, try and figure out what happened to the original weights from My_app


