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

