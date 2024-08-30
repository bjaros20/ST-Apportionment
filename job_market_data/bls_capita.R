#Feedback from Fleck, get jobs per capita.
##Detrend employment by state

#Care about standard errors, bootstrap up 100 for each state


#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
library(synthdid)
library(fixest)
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data, bls data
Rev <- read.csv("detrend_per_capita.csv")
filter_bls <-read.csv("bls_post.csv")

#Want jobs per capita, so going to get population by state from Rev and merge
pop <- Rev %>%
  select(State_Name,population,year)

#Left join
bls_pop <- filter_bls %>%
  left_join(pop,by=c("State_Name","year"))

bls_pop <- bls_pop %>%
  mutate(population=population*1000)

#Get jobs by per capita, keeps original column names, 
bls_per_cap <- bls_pop %>%
  mutate(across(c(Totalemploymentnumberofjobs, Wageandsalaryemployment, Proprietorsemployment,
                  Farmproprietorsemployment, Nonfarmproprietorsemployment2, Farmemployment,
                  Nonfarmemployment, Privatenonfarmemployment, Forestryfishingandrelatedactivities,
                  Miningquarryingandoilandgasextraction, Utilities, Construction, Manufacturing,
                  Wholesaletrade, Retailtrade, Transportationandwarehousing, Information,
                  Financeandinsurance, Realestateandrentalandleasing,
                  Professionalscientificandtechnicalservices, Managementofcompaniesandenterprises,
                  Administrativeandsupportandwastemanagementandremediationservices, Educationalservices,
                  Healthcareandsocialassistance, Artsentertainmentandrecreation, Accommodationandfoodservices,
                  Otherservicesexceptgovernmentandgovernmententerprises, Governmentandgovernmententerprises,
                  Federalcivilian, Military, Stateandlocal, Stategovernment, Localgovernment),
                ~ suppressWarnings(as.numeric(.) / as.numeric(population))))

write.csv(bls_per_cap,"jobs_per_cap.csv")

#detrend results
safe_detrend <- function(x) {
  print(length(na.omit(x)))  # Debugging: Print length of non-NA values
  if (length(na.omit(x)) > 1) {
    return(detrend(x, tt = "linear"))
  } else {
    return(rep(NA, length(x)))
  }
}

bls_detrend <- bls_per_cap %>%
  mutate(across(c(Totalemploymentnumberofjobs, Wageandsalaryemployment, Proprietorsemployment,
                  Farmproprietorsemployment, Nonfarmproprietorsemployment2, Farmemployment,
                  Nonfarmemployment, Privatenonfarmemployment, Forestryfishingandrelatedactivities,
                  Miningquarryingandoilandgasextraction, Utilities, Construction, Manufacturing,
                  Wholesaletrade, Retailtrade, Transportationandwarehousing, Information,
                  Financeandinsurance, Realestateandrentalandleasing,
                  Professionalscientificandtechnicalservices, Managementofcompaniesandenterprises,
                  Administrativeandsupportandwastemanagementandremediationservices, Educationalservices,
                  Healthcareandsocialassistance, Artsentertainmentandrecreation, Accommodationandfoodservices,
                  Otherservicesexceptgovernmentandgovernmententerprises, Governmentandgovernmententerprises,
                  Federalcivilian, Military, Stateandlocal, Stategovernment, Localgovernment),
                ~ safe_detrend(as.numeric(.))))

#detrend is not working for now, try result without detrend

Simple_reg <- lm(Totalemploymentnumberofjobs ~ Post + factor(year) + factor(State_Name), data = bls_per_cap)
summary(Simple_reg) 

#Result   Post                               151661      43038   3.524 0.000446 ***

# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg <- lm(Totalemploymentnumberofjobs~ Post * factor(State_Name) + factor(year), data = bls_per_cap)
summary(Interaction_reg)

safe_detrend <- function(x) {
  if (length(x) > 1) {
    return(pracma::detrend(x, tt = "linear"))
  } else {
    return(rep(NA, length(x)))  # Return NA for groups that can't be detrended
  }
}

bls_detrend <-bls_per_cap %>%
  filter(!is.na(Wageandsalaryemployment) & Wageandsalaryemployment != "(D)" &
           !is.na(Manufacturing) & Manufacturing != "(D)" & 
           !is.na(Totalemploymentnumberofjobs) & !is.na(Post) & 
           !is.na(year) & !is.na(State_Name)) %>%
  mutate(Totalemploymentnumberofjobs = as.numeric(Totalemploymentnumberofjobs),
         Manufacturing = as.numeric(Manufacturing),
         Wageandsalaryemployment = as.numeric(Wageandsalaryemployment)) %>%
  group_by(State_Acronym) %>%
  mutate(de_tot_jobs = safe_detrend(Totalemploymentnumberofjobs),
         de_man_jobs = safe_detrend(Manufacturing),
         de_salary = safe_detrend(Wageandsalaryemployment)) %>%
  ungroup()

#Estimate these regressions via Simple Regression
Simple_reg_de<- lm(de_tot_jobs ~ Post + factor(year) + factor(State_Name), data = bls_detrend)
summary(Simple_reg_de)

#Result   Post                               151661      43038   3.524 0.000446 ***

# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg_de<- lm(de_tot_jobs~ Post * factor(State_Name) + factor(year), data = bls_detrend)
summary(Interaction_reg_de)
