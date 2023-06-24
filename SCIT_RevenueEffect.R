#Sant Anna and Callaway- State CIT Revenue 1942-2021
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

SCIT_Rev <- read_csv("new_state_year_panel.csv")

EventYear <- read_excel("EffectiveYearSSFA.xlsx",sheet=1)

# Remove the columns named 1942, 1943, 1944, ..., 1949
SCIT_Rev <- SCIT_Rev %>% select(-matches("^194[2-9]$"))

#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

EventYear$Abbreviation <- state_abbreviations

merged_df <- merge(EventYear, SCIT_Rev, by.x = "Abbreviation",by.y="State")


#pause, name of file at pause: TMT_state_year_panel_6_19_23



#Replace Nas with zeros
merged_df[is.na(merged_df$EffectiveYear)]<- 0

# Set treated values for the Control group
merged_df$treated[merged_df$EffectiveYear >=merged_df$year]<-1


#merged_df created, now need treatment group and control group.
# Set treatment variable and time variable
treat_var <- "treated"
time_var <- "year"



# Estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(yname = "SCIT_Revenue",
                        tname = "year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = merged_df)
summary(example_attgt)