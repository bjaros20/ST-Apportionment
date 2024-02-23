#Attempt to Get Cross Partial of State CIT Collections, Other Rev w.r.t Sales Factor Weight
Library()
library(tidyr)
library(dplyr)
library(tidyverse)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/CrossPartial")

#Read in Df
Rev <- read.csv("FRED_SALT_Rev_Feb_2024.csv")