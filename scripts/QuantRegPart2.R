#Quantile Regression
install.packages("readxl")
install.packages("did")
install.packages("tidyverse")
install.packages("lmtest")
install.packages("quantreg")
install.packages("caret")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

library(readxl)
library(did)
library(tidyverse)
library(lmtest)
library(quantreg)
library(caret)
library(ggplot2)
library(tidyr)
library(dplyr)

#Set working directory:
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")

#Load Files
RealRev_PerCap <-read_csv("Real_SCIT_per_Capita.csv",FALSE)

EffectiveYear <- read_xlsx("EffectiveYearSSFA.xlsx",sheet=1)
