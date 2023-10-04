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

#set working directory
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")

#load documents
SCIT_Rev <- read_csv("new_state_year_panel.csv")

EventYear <- read_excel("EffectiveYearSSFA.xlsx",sheet=1)

# Remove the columns named 1942, 1943, 1944, ..., 1949
SCIT_Rev <- SCIT_Rev %>% select(-matches("^194[2-9]$"))

#Attempt to make total CIT Column
# Calculate row sums and add a new row to the data frame
SCIT_Rev2 <- rbind(SCIT_Rev, Total = colSums(SCIT_Rev[, -1]))  # Exclude the 'State' column

# Calculate column totals
column_totals <- colSums(SCIT_Rev[, -1])  # Exclude the 'State' column

total_row <- c("Total", column_totals)
SCIT_Rev3 <- rbind(SCIT_Rev, total_row)

write.csv(SCIT_Rev3, "CIT_Rev_Total.csv", row.names = TRUE)

# Extract the "Total" row as a separate vector
total_row <- SCIT_Rev3[nrow(SCIT_Rev3), -1]  # Exclude the 'Total' and 'State' columns

# Extract the "Total" row as a separate data frame
total_row <- as.data.frame(SCIT_Rev3[nrow(SCIT_Rev3), -1])

# Calculate the fraction for each observation by dividing by the total
fractions <- SCIT_Rev3[, -1]
fractions[-nrow(SCIT_Rev3), ] <- fractions[-nrow(SCIT_Rev3), ] / total_row


#convert to from wide to long panel:

SCIT_Rev_Long <- SCIT_Rev %>%
    pivot_longer(cols = -State, names_to = "Year",values_to = "Revenue")

print(SCIT_Rev_Long)
write.csv(SCIT_Rev_Long, file="SCIT_Rev_Long.csv",row.names=FALSE)




#Get abbreviations for states
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

EventYear$Abbreviation <- state_abbreviations

SCIT_Rev_EventYear <- merge(EventYear, SCIT_Rev_Long, by.x = "Abbreviation",by.y="State")


# Set treatment variable and time variable
treat_var <- "EffectiveYear"
time_var <- "Year"

# convert Effective column to numeric
SCIT_Rev_EventYear$Year <- as.numeric(SCIT_Rev_EventYear$Year)

# convert Effective column to numeric
SCIT_Rev_EventYear$Year <- as.numeric(SCIT_Rev_EventYear$Year)

#Another try
SCIT_Rev_EventYear$State <- as.numeric(factor(SCIT_Rev_EventYear$State))



# Estimate group-time average treatment effects using att_gt method
example_attgt <- att_gt(yname = "Revenue",
                        tname = "Year",
                        idname = "State",
                        gname = "EffectiveYear",
                        data = SCIT_Rev_EventYear)

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

#Summary Statistics
mean(SCIT_Rev_EventYear$Revenue)
median(SCIT_Rev_EventYear$Revenue)