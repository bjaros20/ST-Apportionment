#Create Adoption chart

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
combined_clean_df <-read.csv("Long_Gir_My_App_Comb_clean.csv")

Master_df <- read.csv("MasterMerge_RealRev_EventDates.csv")

RegionNumber <- read_xlsx("RNChart.xlsx")

StateNumber <- read_xlsx("RegionNumber.xlsx")


# Sort Sales Factor By Just Year
ggplot(combined_clean_df, aes(x=year,y=sales,color=state))+geom_line()+
  labs(x="Year",y="Sales")+
  theme_minimal() +
  scale_color_discrete(name="State")






