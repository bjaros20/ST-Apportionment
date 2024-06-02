# Create Dataframe for Elasticity results for All the Tax Bases
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
rates <-read_xlsx("stateCIT_rates_1976_2023 copy.xlsx")

#There are a couple of issues with the rate right now.  1976-2002 are in a different format than 2003 onwards.
#There are also missing values for years.  For now,  I will use the historic rate over the more recent rate

#start with formatting
class(rates)
str(rates)

#The dates between 1976-2002 are characters. They also have a space before the first number

#remove the space first

#install stingr package
install.packages("stringr")
library(stringr)

#Remove the space
str_remove_all(rates," ")
#Need to create a separate dataframe and merge back

rates_1976_2002 <- rates %>% select("1976 ":"2002 ")
colnames(rates)

#Space after the year for 1976 through 2002.  
nospace_rate <- as.data.frame(
  apply(rates,2, function(x) gsub("\ ", "", x)))

#it ran, so will try again.
str(nospace_rate)
