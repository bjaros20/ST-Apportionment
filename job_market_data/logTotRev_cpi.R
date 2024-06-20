#Simple DiD result with Log total real revenue 6-20-24
#also note to self, after getting this result, will need to consider saving results
#in a notebook, like Micah recommended.

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
install.packages("sjlabelled")
library(sjlabelled) # req by https://rpubs.com/phle/r_tutorial_difference_in_differences
install.packages("ggrepel")
library(ggrepel) # req by ^
install.packages("scales")
library(scales) # req by ^
install.packages("ggpubr")
library(ggpubr) # req by ^
library(plm)
library(lmtest)

#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

# set seed
set.seed(26)

#Load Revenue panel
Rev <- read.csv("logTotRev_cpi.csv")
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X)

#Estimate the simple DiD with the 2007 switchers
#Anyone before needs to be filtered out (a, i)
#anyone who switches within 3 years 2008-2010, needs to be filtered out (a, ii)
#leaves treatment (a,iii) 2007 Switchers and control anyone who switches after 2010 (a, iv)
#(b) https://rpubs.com/phle/r_tutorial_difference_in_differences



