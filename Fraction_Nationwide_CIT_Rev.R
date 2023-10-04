#Sant Anna and Callaway- State CIT Revenue 1942-2021, For Share of CIT Revenue
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

#load documents
SCIT_Rev <- read_csv("new_state_year_panel.csv")

EventYear <- read_excel("EffectiveYearSSFA.xlsx",sheet=1)

# Remove the columns named 1942, 1943, 1944, ..., 1949
SCIT_Rev <- SCIT_Rev %>% select(-matches("^194[2-9]$"))

CIT_Rev<-read_csv("Fraction_CIT_Rev.csv")


#convert to from wide to long panel:

CIT_Rev_Long <- CIT_Rev %>%
  pivot_longer(cols = -State, names_to = "Year",values_to = "Revenue")

print(SCIT_Rev_Long)
write.csv(CIT_Rev_Long, file="CIT_Rev_Long.csv",row.names=FALSE)




#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

EventYear$Abbreviation <- state_abbreviations

SCIT_Rev_EventYear <- merge(EventYear, CIT_Rev_Long, by.x = "Abbreviation",by.y="State")

#Exclude no CIT States
# List of states to exclude
excluded_states <- c("Nevada", "Texas", "South Dakota", "Washington", "Wyoming")

# Filter out the excluded states
SCIT_Rev_EventYear2 <- SCIT_Rev_EventYear[!SCIT_Rev_EventYear$State %in% excluded_states, ]

# Assuming your dataframe is named "SCIT_Rev_EventYear2"
SCIT_Rev_EventYear2 <- na.omit(SCIT_Rev_EventYear2)





# Set treatment variable and time variable
treat_var <- "EffectiveYear"
time_var <- "Year"

# convert Effective column to numeric
SCIT_Rev_EventYear2$Year <- as.numeric(SCIT_Rev_EventYear2$Year)

# convert Effective column to numeric
SCIT_Rev_EventYear2$Year <- as.numeric(SCIT_Rev_EventYear2$Year)

#Another try
SCIT_Rev_EventYear2$State <- as.numeric(factor(SCIT_Rev_EventYear2$State))



# Estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(yname = "Revenue",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = SCIT_Rev_EventYear2)

summary(example_attgt)

#Weighted Average of all Group-time Treatment effects, with weights proportional to group size
#Downside(Will overweight the effect of early treated groups)
agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple)

#Event Study Plot- Group-time average treatment effects can be averaged into average treatment effects at different lengths of exposure
#(Called Overall ATT)
agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#Group-Specific Treatment effects, looks at average effects for each group (Called Group Specific)
agg.gs <-aggte(example_attgt,type="group")
summary(agg.gs)
ggdid(agg.gs)

#Calendar Time Effects, aggregation across different time periods
agg.ct <-aggte(example_attgt,type="calendar")
summary(agg.ct)
ggdid(agg.ct)

mean(SCIT_Rev_EventYear2$Revenue)
median(SCIT_Rev_EventYear2$Revenue)
count(SCIT_Rev_EventYear2$Revenue)
summary(SCIT_Rev_EventYear2$Revenue)
sd(SCIT_Rev_EventYear2$Revenue)
