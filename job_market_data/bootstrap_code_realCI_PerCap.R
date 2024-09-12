# New Bootstrap

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

#Load data
naive_ci<-read.csv("naive_ci.csv")


# Fit the synthdid model using current_sDiD
estimate <- synthdid_estimate(Y = current_sDiD$Y, N0 = current_sDiD$N0, T0 = current_sDiD$T0)

vcov_bootstrap <- vcov(estimate, method = "bootstrap", replications = 500)


# Calculate standard errors by taking the square root of the diagonal of the variance-covariance matrix
se_result_bootstrap <- sqrt(diag(vcov_bootstrap))

# View the results
print(se_result_bootstrap)




