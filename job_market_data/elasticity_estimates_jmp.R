#Elasticity Results for All the Tax Bases
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load data for Elasticity JMP
Elas <-read.csv("elasticity_data_jmp.csv")