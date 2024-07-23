# Severance Tax- Two Way Fixed Effects
# will need to include all states, even those with ZERO SEVERANCE TAX, following dougan Input
#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(readxl) #load ssfa switch date
library(did) # for running DiD
library(caret) 
library(plm) # for Two way FE
library(fixest) # multiple FE
library(broom) #extract coefficients for each state

# set seed
set.seed(26)

#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Revenue CIT File
Rev <- read.csv("filled_data_jmp.csv")
#Need to merge back our Non CIT states and their data
NoCIT <- read.csv("Non_CIT_states_FRED_OH.csv")



#Merge NoCIT with Rev2, then select Relevant variables
Merge2 <- NoCIT %>%
  bind_rows(Rev)

SSFA <- read_xlsx("ssfa_data_jmp.xlsx")



#Load rates dataframe
Rates<- read.csv("clean_rates_1976-2022.csv")

Rates2 <- read.csv("rates_jmp.csv")

#don't need to load log_Rev or Rev2, will delete them.  The main difference between 
#filled_data_jmp and detrend_per_capita about 15 logged and detrended columns.
#log revenue includes 3 more log columns.  The difference that should be merged into a default
#dataset is that that Rev alone contains all states
# Even Rev doesn't contain Texas.  Will need to go earlier.



