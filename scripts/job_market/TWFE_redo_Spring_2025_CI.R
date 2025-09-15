#redo TWFE Regressions- Section 6.2- Jonathan Feedback post Jan 17 workshop

# Examine TWFE four main outcomes: 
# a. Corporate Tax Revenue
# b. Non-corporate tax revenue
# c. tax rates
# d. Proxy Corporate Income.

#load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(did) # for running DiD
library(plm)
library(lmtest)
library(fixest)
library(boot)


# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")


#load data
data <- read.csv("real_log_nci.csv")

#use this as guide https://libguides.princeton.edu/R-Panel
fixed <-plm(logRealCitRev ~ Post, data = data, index=c("State_Acronym","year"), model = "within") #fixed effects
random <-plm(logRealCitRev ~ Post, data = data, index=c("State_Acronym","year"), model = "random")
phtest(fixed,random)

#Results of hausman test
#data:  logRealCitRev ~ Post
#chisq = 0.79719, df = 1, p-value = 0.3719
#alternative hypothesis: one model is inconsistent. 


# Estimate Two-Way Fixed Effects Model, CIT Revenue
twfe_model <- plm(logRealCitRev ~ Post + factor(year), 
                  data = data, 
                  index = c("State_Acronym", "year"), 
                  model = "within")  # Fixed effects model

# Summary of the model
summary(twfe_model)
#Coefficients:
#Estimate Std. Error t-value  Pr(>|t|)    
#Post             -0.0238689  0.0281037 -0.8493 0.3958078   
# Clustered SE at the state level
coeftest(twfe_model, vcov = vcovHC(twfe_model, type = "HC0", cluster = "group"))

#cluster Standard errors at state level
#t test of coefficients:
# Estimate Std. Error t value  Pr(>|t|)    
# Post             -0.0238689  0.0686282 -0.3478 0.7280271   

# TWFE using fixest
twfe_fixest <- feols(logRealCitRev ~ Post | State_Acronym + year, data = data)

# Summary with robust SE
summary(twfe_fixest, cluster = "State_Acronym")

# Extract coefficient estimate and standard error
coef_df <- as.data.frame(coeftable(twfe_fixest, cluster = "State_Acronym"))  # Clustered SEs
coef_df$Variable <- rownames(coef_df)

# Plot using ggplot2
ggplot(coef_df, aes(x = Variable, y = Estimate, ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  labs(title = "Effect of Post on Log Corporate Tax Revenue",
       x = "Variable",
       y = "Coefficient Estimate") +
  coord_flip() + theme_fivethirtyeight()



#TWFE on NonCorporate Tax Revnue
twfe_fixest_RNCTR <- feols(log_RNCTR ~ Post | State_Acronym + year, data = data)

# Summary with robust SE
summary(twfe_fixest_RNCTR, cluster = "State_Acronym")

#     Estimate Std. Error  t value Pr(>|t|) 
# Post 0.020601   0.040766 0.505354  0.61583 


# Extract coefficient estimate and standard error
coef_df <- as.data.frame(coeftable(twfe_fixest_RNCTR, cluster = "State_Acronym"))  # Clustered SEs
coef_df$Variable <- rownames(coef_df)

# Plot using ggplot2
ggplot(coef_df, aes(x = Variable, y = Estimate, ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  labs(title = "Effect of Post on Real Log Non Corporate Income",
       x = "Variable",
       y = "Coefficient Estimate") +
  coord_flip() + theme_fivethirtyeight()


#TWFE on Proxy Corporate Income
twfe_fixest_CI <- feols(real_log_nci ~ Post | State_Acronym + year, data = data)

# Summary with robust SE
summary(twfe_fixest_CI, cluster = "State_Acronym")

#     Estimate Std. Error  t value Pr(>|t|) 
#Post -0.020521   0.079491 -0.258159  0.79749 


# Extract coefficient estimate and standard error
coef_df <- as.data.frame(coeftable(twfe_fixest_CI, cluster = "State_Acronym"))  # Clustered SEs
coef_df$Variable <- rownames(coef_df)

# Plot using ggplot2
ggplot(coef_df, aes(x = Variable, y = Estimate, ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  labs(title = "Effect of Post on Log Corporate Income",
       x = "Variable",
       y = "Coefficient Estimate") +
  coord_flip() + theme_fivethirtyeight()



#Check for Event study style.
# Convert rel_year to factor
data$rel_year <- as.factor(data$rel_year)

# TWFE event study model using plm
event_study_plm <- plm(real_log_nci ~i(rel_year, ref = -1)*Post, 
                       data = data, 
                       index = c("State_Acronym", "year"), 
                       model = "within", 
                       effect = "twoways")

# Summary
summary(event_study_plm)
