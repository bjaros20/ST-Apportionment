# Point estimate results for Individual Income Tax
# Need to filter out states without an income tax

#States with no individual income tax: AK, FL, NV, SD, TN, TX, WY

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


#Laod data
naive_ci<-read.csv("naive_ci.csv")
Rev <- read.csv("detrend_per_capita.csv")

#Wyoming and other no inc tax states are not in the dataframe.   Will need to retrace
