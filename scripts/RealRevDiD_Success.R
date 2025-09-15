#RealRev_DiD Success
library(readxl)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#Set working directory:
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")

#Load Files
RealRev_PerCap <-read_csv("Real_SCIT_per_Capita.csv")

EventYear <- read_xlsx("EffectiveYearSSFA.xlsx",sheet=1)

Real_Rev_EventYear

#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

EventYear$Abbreviation <- state_abbreviations

Real_Rev_EventYear <- merge(EventYear, RealRev_PerCap, by.x = "Abbreviation",by.y="State")

#FullMerge
write.csv(Real_Rev_EventYear,"MasterMerge_RealRev_EventDates.csv",row.names = FALSE)

# Set treatment variable and time variable
treat_var <- "EffectiveYear"
time_var <- "Year"

# convert Effective column to numeric
Real_Rev_EventYear$Year <- as.numeric(Real_Rev_EventYear$Year)



#Another try
Real_Rev_EventYear$State <- as.numeric(factor(Real_Rev_EventYear$State))



#filterout California
filter_Real_Rev_EventYear <- Real_Rev_EventYear %>%
          filter(Abbreviation !="CA")


# List of states to exclude
excluded_states <- c("NV", "TX", "SD", "WA", "WY")

#Exclude NO CIT States
Real_Rev_EventYear <- Real_Rev_EventYear[!(Real_Rev_EventYear$Abbreviation %in% excluded_states), ]


# Estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(yname = "RealRev",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = Real_Rev_EventYear)

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

#descriptive Statistics
mean(Real_Rev_EventYear$RealRev)
median(Real_Rev_EventYear$RealRev)
summary(Real_Rev_EventYear$RealRev)
sd(Real_Rev_EventYear$RealRev)


#Try Again with Just Real Revenue
# Estimate group-time average treatment effects using att_gt method
RealRev_attgt <- att_gt(yname = "RealRevPerCapita",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = Real_Rev_EventYear)

summary(RealRev_attgt)

#Weighted Average of all Group-time Treatment effects, with weights proportional to group size
#Downside(Will overweight the effect of early treated groups)
agg.simple <- aggte(RealRev_attgt, type = "simple")
summary(agg.simple)

#Event Study Plot- Group-time average treatment effects can be averaged into average treatment effects at different lengths of exposure
#(Called Overall ATT)
agg.es <- aggte(RealRev_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#Group-Specific Treatment effects, looks at average effects for each group (Called Group Specific)
agg.gs <-aggte(RealRev_attgt,type="group")
summary(agg.gs)
ggdid(agg.gs)

#Calendar Time Effects, aggregation across different time periods
agg.ct <-aggte(RealRev_attgt,type="calendar")
summary(agg.ct)
ggdid(agg.ct)

#descriptive Statistics
mean(Real_Rev_EventYear$RealRevPerCapita)
median(Real_Rev_EventYear$RealRevPerCapita)
summary(Real_Rev_EventYear$RealRevPerCapita)
sd(Real_Rev_EventYear$RealRevPerCapita)


#Real Rev, No California
# Estimate group-time average treatment effects using att_gt method
RealRevNOCAL_attgt <- att_gt(yname = "RealRev",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = filter_Real_Rev_EventYear)

summary(RealRevNOCAL_attgt)

#Weighted Average of all Group-time Treatment effects, with weights proportional to group size
#Downside(Will overweight the effect of early treated groups)
agg.simple <- aggte(RealRevNOCAL_attgt, type = "simple")
summary(agg.simple)

#Event Study Plot- Group-time average treatment effects can be averaged into average treatment effects at different lengths of exposure
#(Called Overall ATT)
agg.es <- aggte(RealRevNOCAL_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#Group-Specific Treatment effects, looks at average effects for each group (Called Group Specific)
agg.gs <-aggte(RealRevNOCAL_attgt,type="group")
summary(agg.gs)
ggdid(agg.gs)

#Calendar Time Effects, aggregation across different time periods
agg.ct <-aggte(RealRevNOCAL_attgt,type="calendar")
summary(agg.ct)
ggdid(agg.ct)













#Try Again with Just Real Revenue, time period 1, 2, 3 years.
# Estimate group-time average treatment effects using att_gt method
RealRev_attgt1 <- att_gt(yname = "RealRevPerCapita",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = Real_Rev_EventYear)

summary(RealRev_attgt1)

#Weighted Average of all Group-time Treatment effects, with weights proportional to group size
#Downside(Will overweight the effect of early treated groups)
agg.simple1 <- aggte(RealRev_attgt1, type = "simple")
summary(agg.simple1)

#Event Study Plot- Group-time average treatment effects can be averaged into average treatment effects at different lengths of exposure
#(Called Overall ATT)
agg.es <- aggte(RealRev_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

#Group-Specific Treatment effects, looks at average effects for each group (Called Group Specific)
agg.gs <-aggte(RealRev_attgt,type="group")
summary(agg.gs)
ggdid(agg.gs)

#Calendar Time Effects, aggregation across different time periods
agg.ct <-aggte(RealRev_attgt,type="calendar")
summary(agg.ct)
ggdid(agg.ct)


