#Log CI result for Hoover draft.
#implement and understand as much as possible from sDiD used in Rauh,Ryu 2024

#going to log CI, get the point estimates for SR, LR,

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

#log CI
log_nci <- naive_ci %>%
  mutate(log_ci = log(naive_ci))

#save as new naive_ci because I do these steps each start anyways
write.csv(log_nci,"naive_ci.csv",row.names=FALSE)



