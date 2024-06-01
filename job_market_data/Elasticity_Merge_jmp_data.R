# Measure the Elasticity Results for All the Tax Bases
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#New Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load App Weights Dataframe from Earlier Elasticity Plots
AppWeights <-read.csv("AppWeights_4_21_24_Rev copy.csv")

SalesFac_jmp <-read_xlsx("sales_factor_jmp_may_24.xlsx")

#difference between AppWeights and SaleFac_jmp.  Need to create a master JMP elasticity sheet to become public
#2023 data is one difference.

#Going to use antijoin to find out what rows are in AppWeights but aren't in SalesFac_jmp
diff_App_Sal <- anti_join(AppWeights,SalesFac_jmp,by=c("state","year"))

#^ ran it, these are the non corporate income tax states (NV, SD, TX, WA, and WY) + OH, excluded because of the CAT
#save this to be added back when needed for control group

write_csv(diff_App_Sal,"Non_CIT_states_FRED_OH.csv")


#Now going to run anti_join to figure out what is in SaleFac but is not in AppWeights, should be zero, but checking
diff_Sal_App <- anti_join(SalesFac_jmp,AppWeights, by= c("state","year"))
#^it was zero observations

#Create a dataframe
common_rows <-inner_join(AppWeights,SalesFac_jmp, by=c("state","year"))

#find out what sales don't match between the dataframes
differences_in_sales <- common_rows %>% filter(sales.x != sales.y)
 #^No differences in the sales factor weights, only differences in industry, election, and phase in dummies.

#make the common_rows df the job market dataframe for elasticity
common_rows2 <-common_rows %>%
  select(-sales.x) %>% # Remove the sales.x column
  rename(sales = sales.y)

write_csv(common_rows2,"elasticity_data_jmp.csv")


#then Merge the rates to run the Three Way FE:




