#Attempt to Get Cross Partial of State CIT Collections, Other Rev w.r.t Sales Factor Weight
library(tidyr)
library(dplyr)
library(tidyverse)
install.packages("bacondecomp")
library(bacondecomp) 
# for robust standard error estimation
library(lmtest) 
install.packages("multiwayvcov")
# To calculate correct vcov matrix with 2WFE
library(multiwayvcov) 

install.packages("plm")
# For a package way to do FE
library(plm)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/CrossPartial")

# Read in DF
Rev <- read.csv("SALT_App_Merge_2024.csv")

Rev2 <- Rev %>%
    filter(c(year >=1976 & year <=2023))

Rev2['treatment'] <- 0
Rev2[which(Rev2$state=="Nebraska" & Rev2$year>1991),"treatment"]<-1

Reg <- lm(CORPINCTAX~sales)

fit_tw <- lm(CORPINCTX ~ treatment + factor(state) + factor(year), 
             data = Rev2)

summary(fit_tw)


