#Nebraska or "Direct Switchers DiD Approach"
#Run DiD with all the Direct Switchers (NE, MO, MS, CO, RI, ND, DE)
#Control Group is all States that Did not Switch by 2007.  
#this includes: AK, HI, KS, NM, OK.  AL and MT between 2008-now became "Step
#-wise" switchers.  FL and MA had DWS over the whole period 1976-2022.

library(tidyr)
library(dplyr)
library(tidyverse)
# for robust standard error estimation
library(lmtest) 
# To calculate correct vcov matrix with 2WFE
library(multiwayvcov) 
# For a package way to do FE
library(plm)
#Fixest
install.packages("fixest")
library(fixest)
#Load Data Tables in R
install.packages("data.table")
library(data.table)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/Direct_Switch_DiD")

# Read in DF
Rev <- read.csv("SALT_App_Merge_2024.csv")

Rev2 <- Rev %>%
  filter(c(year >=1976 & year <=2022))

Rev3 <- Rev2 %>%
  filter(c(year >=1976 & year <=2007))

#Create Log Revenue
Rev2 <- Rev2 %>%
  mutate(Log_CORPINCTX=log(CORPINCTX))

Rev3 <- Rev3 %>%
  mutate(Log_CORPINCTX=log(CORPINCTX))



#Filter Rev 2 to just be NE with Control Group States
NE_DiD2 <- Rev3 %>%
  filter(State_Acronym=="NE"|State_Acronym=="AK"| State_Acronym=="HI"|State_Acronym=="KS"|State_Acronym=="NM"|State_Acronym=="OK"|State_Acronym=="AL"|State_Acronym=="MT")

#Create Treatment Column for NE
NE_DiD2['treatment'] <- 0
NE_DiD2[which(NE_DiD2$state=="Nebraska" & NE_DiD2$year>1988),"treatment"]<-1

#Fit TW Fixed Effect
fit_tw <- lm(CORPINCTX ~ treatment + factor(state) + factor(year), 
             data = NE_DiD2)
summary(fit_tw) 


#Fit TW FE Log, but better
fit_tw_Log <- feols(Log_CORPINCTX ~ 
                   treatment |
                   state + year,
             data = NE_DiD2)

summary(fit_tw_Log) 


#Event Study Plot
NE_Plot2 <- coefplot(fit_tw_Log)


#Create a Time to Treatment Column for NE all periods, negative for 1976 through 1987 and positive for 1989 through 2007
NE_DiD2['treatment'] <- 0
NE_DiD2[which(NE_DiD2$state=="Nebraska" & NE_DiD2$year>1988),"treatment"]<-1



#Create a time to Treat variable to Create Event Study Plot:

NE_DiD[, time_to_treat := ifelse(treatment==1, year - `_nfd`, 0)]

## Assuming NE_DiD is a data.frame
NE_DiD <- as.data.table(NE_DiD)

# Now you can use := for assignment
NE_DiD[, time_to_treat := ifelse(treatment == 1, year - 1988, 0)]
