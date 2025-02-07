# 7.1 Descriptive Plots
#Trying to establish Correlations
# Examine via plots four main outcomes: 
# a. Corporate Tax Revenue
# b. Non-corporate tax revenue
# c. tax rates
# d. Proxy Corporate Income.

# Plot it for the individual state, log and un log it with vertical line for treatment

#load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#load data
data <- read.csv("real_log_nci.csv")

