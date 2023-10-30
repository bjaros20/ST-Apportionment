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





ggplot(combined_clean_df, aes(x=year,y=sales,color=state))+geom_line()+
  labs(x="Year",y="Sales")+
  theme_minimal() +
  scale_color_discrete(name="State")

grouped <- combined_clean_df
mutate(Group = case_when(
  # Group 1: States that change from 33.34 to 50 to 100
  between(sales, 33.33, 33.35) & lag(sales) == 50 & lead(sales) == 100 ~ "Group 1",
  
  # Group 2: States that gradually change from 33.34 to 100
  sales == 100 & lag(sales) == 33.34 & !lead(sales) == 100 ~ "Group 2",
  
  # Group 3: States that switch directly from 33.34 to 100
  sales == 100 & lag(sales) == 33.34 ~ "Group 3",
  
  # Group 4: States that always have a sales value of 33.34
  all(sales == 33.34) ~ "Group 4",
  
  # Default category for other cases
  TRUE ~ "Other"
))




