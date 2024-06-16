#Plot of Log Total Revenue Coefficients & DiD plots

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom) #extract coefficients for each state

#set directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Data, Coefficient Plot & Rev Data for estimates
Coef <-read.csv("state_post_dummy_total_rev._6_15_24.csv")

Rev <-read.csv("revenue_panel_total_rev_state_coef_result_6_15_24.csv")

#Eliminate the X.  For future reference, when saving the CSV
#read.csv("dataframe.csv",row.names=FALSE)
Rev <-Rev %>%
  select(-X)
Coef <-Coef %>%
  select(-X)

#Load Region Dataframe for plot. Also, could use year of switch, trying to find a pattern


