#Get REAL Log Total Revenue effects, and plot the Coefficients due 6/19
#try and do a simple DiD after 2WFE for the 2007 adopters for tight 3 year window
#

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
install.packages("readxl")
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

#Load Data, Coefficient Plot & Rev Data for estimates
Coef <-read.csv("state_post_dummy_total_rev._6_15_24.csv")

Rev <-read.csv("revenue_panel_total_rev_state_coef_result_6_15_24.csv")

CPI <- read_excel("/Users/ben/Documents/GitHub/ST-Apportionment/job_market_data/cpi.xlsx")

#Remove X
#remove the X column, must be something that happens when saving CSV
Rev <-Rev %>%
  select(-X)

#Merge CPI and Rev
Rev1 <- Rev %>%
  full_join(CPI,by=c("year"="Year"))

#save Revenue with CPI
write.csv(Rev1,"revenues_cpi_6_17_24.csv")

#Just create real revenue for Total Revenue
Rev2 <- Rev1 %>%
mutate(real_totRev = (TOTLTAX/Annual)*100)

#rename annual column to CPI
Rev3 <- Rev2 %>%
  rename(CPI_def=Annual)

write.csv(Rev3,"revenue_cpi_total.csv")

#Completed Real total Post coefficients for 2WFE in previous script.
#now complete simple DiD

write.csv(Rev4,"logTotRev_cpi.csv",row.names = FALSE)
#save post_interactions_coefficients
write.csv(post_coefficients_combined,"twoWFE_LogTotRev_result.csv",row.names = FALSE)

