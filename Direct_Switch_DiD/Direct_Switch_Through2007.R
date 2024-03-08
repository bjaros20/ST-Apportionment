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
  filter(c(year >=1976 & year <=2007))

RevMo<- Rev %>%
  filter(c(year>=1979 & year <=2019))


#Create Log Revenue
Rev2 <- Rev2 %>%
  mutate(Log_CORPINCTX=log(CORPINCTX))

RevMo <- RevMo %>%
  mutate(Log_CORPINCTX=log(CORPINCTX))

#Filter Rev 2 to just be NE with Control Group States
NE_DiD <- Rev2 %>%
  filter(State_Acronym=="NE"|State_Acronym=="AK"| State_Acronym=="HI"|State_Acronym=="KS"|State_Acronym=="NM"|State_Acronym=="OK"|State_Acronym=="AL"|State_Acronym=="MT")

#Create Treatment Column for NE
NE_DiD['treatment'] <- 0
NE_DiD[which(NE_DiD$state=="Nebraska" & NE_DiD$year>1988),"treatment"]<-1

#Fit TW Fixed Effect
fit_tw <- lm(CORPINCTX ~ treatment + factor(state) + factor(year), 
             data = NE_DiD)
summary(fit_tw) 


#Fit TW FE Log, but better, Wards Package
fit_tw_Log <- feols(Log_CORPINCTX ~ 
                   treatment |
                   state + year,
             data = NE_DiD)

summary(fit_tw_Log) 


#Event Study Plot
NE_Plot <- coefplot(fit_tw_Log)


#Create a Time to Treatment Column for NE all periods, negative for 1976 through 1987 and positive for 1989 through 2007
NE_DiD['time_to_treatment'] <- 0
NE_DiD[which(NE_DiD$state=="Nebraska" & NE_DiD$year>1988),"treatment"]<-1


#Create a time to treatment column for all states and all periods and set default to 0
NE_DiD$time_to_treatment <- 0

#Set Treatment Year of 0 in time to treatment column
NE_DiD$time_to_treatment[NE_DiD$state == "Nebraska" & NE_DiD$year == 1988] <- 0

#Get Decreasing Negative Numbers for time Prior to treatment for Nebraska
NE_DiD$time_to_treatment[NE_DiD$state == "Nebraska" & NE_DiD$year < 1988] <-
  (1988 - NE_DiD$year[NE_DiD$state == "Nebraska" & NE_DiD$year < 1988])*-1

#Get increasing Positive Numbers for Nebrask after Treatment
NE_DiD$time_to_treatment[NE_DiD$state == "Nebraska" & NE_DiD$year > 1988] <-
  NE_DiD$year[NE_DiD$state == "Nebraska" & NE_DiD$year > 1988] - 1988


#Run the Modified 2WFE w/ feols
mod_twfe_NE=feols(Log_CORPINCTX ~ 
                     i(time_to_treatment,treatment, ref=-1) |
                     state + year,
                   data = NE_DiD,
                collin.rm=FALSE)

#Error Message that excluded the time to treatment variables due to collinearity
#The variables 'time_to_treatment::-12:treatment', 'time_to_treatment::-11:treatment' and ten others have been removed because of collinearity (see $collin.var).
#Plot
iplot(mod_twfe_NE,
      xlab='Time to treatment',
      main='Event Study Plot: Nebraska Vs. Never & Not-Yet Treated Control (TWFE)')

#Attempt to Run twfe w/o i notation
mod_twfe_NE6 <- feols(Log_CORPINCTX ~
                        i(time_to_treatment, ref=-1) |
                        state + year,
                     data = NE_DiD,
                     collin.tol=vcov<0)
#Plot
iplot(mod_twfe_NE6,
      xlab='Time to treatment',
      main='Event Study Plot: Nebraska Vs. Never & Not-Yet Treated Control (TWFE)')


#Now to Get the Same Coefficient for all of the Direct Switchers
#Missouri is the next state.  Switch occurs in 1999
#Filter Rev 2 to just be NE with Control Group States
MO_DiD <- Rev2 %>%
  filter(State_Acronym=="MO"|State_Acronym=="AK"| State_Acronym=="HI"|State_Acronym=="KS"|State_Acronym=="NM"|State_Acronym=="OK"|State_Acronym=="AL"|State_Acronym=="MT")

#Run this again with the larger range of years for the plot:
MO_DiD <- RevMo %>%
  filter(State_Acronym=="MO"|State_Acronym=="AK"| State_Acronym=="HI"|State_Acronym=="KS"|State_Acronym=="NM"|State_Acronym=="OK"|State_Acronym=="AL"|State_Acronym=="MT")



#Create Treatment Column for MO
MO_DiD['treatment'] <- 0
MO_DiD[which(MO_DiD$state=="Missouri" & MO_DiD$year>1999),"treatment"]<-1


#Fit TW FE Log, but better, Wards Package
MO_fit_tw_Log <- feols(Log_CORPINCTX ~ 
                      treatment |
                      state + year,
                    data = MO_DiD)

summary(MO_fit_tw_Log) 

#Create a time to treatment column for all states and all periods and set default to 0
MO_DiD$time_to_treatment <- 0

#Set Treatment Year of 0 in time to treatment column
MO_DiD$time_to_treatment[MO_DiD$state == "Missouri" & MO_DiD$year == 1999] <- 0

#Get Decreasing Negative Numbers for time Prior to treatment for Missouri
MO_DiD$time_to_treatment[MO_DiD$state == "Missouri" & MO_DiD$year < 1999] <-
  (1999 - MO_DiD$year[MO_DiD$state == "Missouri" & MO_DiD$year < 1999])*-1

#Get increasing Positive Numbers for Nebrask after Treatment
MO_DiD$time_to_treatment[MO_DiD$state == "Missouri" & MO_DiD$year > 1999] <-
  MO_DiD$year[MO_DiD$state == "Missouri" & MO_DiD$year > 1999] - 1999


#Run the Modified 2WFE w/ feols
mod_twfe_MO=feols(Log_CORPINCTX ~ 
                    i(time_to_treatment,treatment, ref=-1) |
                    state + year,
                  data = MO_DiD)

#Error Message that excluded the time to treatment variables due to collinearity
#The variables 'time_to_treatment::-12:treatment', 'time_to_treatment::-11:treatment' and ten others have been removed because of collinearity (see $collin.var).
#Plot
iplot(mod_twfe_MO,
      xlab='Time to treatment',
      main='Event Study Plot: Missouri Vs. Never & Not-Yet Treated Control (TWFE)')

#Attempt to Run twfe w/o variables dropping
mod_twfe_MO2 <- feols(Log_CORPINCTX ~
                        i(time_to_treatment, ref=-1) |
                        state + year,
                      data = MO_DiD)
#Plot
iplot(mod_twfe_MO2,
      xlab='Time to treatment',
      main='Event Study Plot: Missouri Vs. Never & Not-Yet Treated Control (TWFE)')

#Time for Mississippi



