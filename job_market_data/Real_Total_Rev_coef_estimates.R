#Get REAL Log Total Revenue effects, and plot the Coefficients due 6/19
#try and do a simple DiD after 2WFE for the 2007 adopters for tight 3 year window
#

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(readxl) #load ssfa switch date
library(did) # for running DiD
library(caret) 
library(plm) # for Two way FE
library(fixest) # multiple FE
library(broom) #extract coefficients for each state