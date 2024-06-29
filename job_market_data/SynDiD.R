# make correlation matrix, https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
#and Synthetic DiD, https://synth-inference.github.io/synthdid/

#Data, will load this csv "detrend_per_capita.csv", contains detrended, per capita, real revenue

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
devtools::install_github("synth-inference/synthdid")
library(synthdid)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")

#Remove X and X.1, REMOVE COLUMN when saving
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X,-X.1)

#Create just real total revenue per capita, detrended dataframe (or just total revenue)

