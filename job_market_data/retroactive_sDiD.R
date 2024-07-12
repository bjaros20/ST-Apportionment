#Retroactive Switchers
#going to really commit to the sDiD rout.  That requires no anticipation and no spillovers
# I will estimate effects for each individual retroactive switcher, then aggregate
# see what the process yields from there.

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
# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")