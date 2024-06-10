#Elasticity Results for All the Tax Bases
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)
install.packages("broom")
library(broom) #for tidying up the group by regressions

#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load data for Elasticity JMP
Elas <-read.csv("elasticity_rates_jmp.csv")

#remove the X column
Elas <-Elas %>%
  select(-X)

#check data type of Elas
str(Elas)

#need to convert the tax collections from integers to numeric
Elas_numeric <- Elas

# Convert integer columns to numeric in the new data frame
Elas_numeric$BUSLICTAX <- as.numeric(Elas_numeric$BUSLICTAX)
Elas_numeric$CORPINCTX <- as.numeric(Elas_numeric$CORPINCTX)
Elas_numeric$CORPLICTAX <- as.numeric(Elas_numeric$CORPLICTAX)
Elas_numeric$PROPTAX <- as.numeric(Elas_numeric$PROPTAX)
Elas_numeric$SLGRTAX <- as.numeric(Elas_numeric$SLGRTAX)
Elas_numeric$SVRNCTAX <- as.numeric(Elas_numeric$SVRNCTAX)
Elas_numeric$TLINCTAX <- as.numeric(Elas_numeric$TLINCTAX)
Elas_numeric$TLSLTAX <- as.numeric(Elas_numeric$TLSLTAX)
Elas_numeric$TOTLTAX <- as.numeric(Elas_numeric$TOTLTAX)
Elas_numeric$INCTAX <- as.numeric(Elas_numeric$INCTAX)
Elas_numeric$SALESTAX <- as.numeric(Elas_numeric$SALESTAX)

#check it
str(Elas_numeric)

#save without X column and numeric tax collections
write.csv(Elas_numeric,"elasticity_rates_jmp.csv")

#Some items are missing in the regression:
Elas_numeric %>%
  group_by(state) %>%
  summarize(
    n_missing_CORPINCTX = sum(is.na(CORPINCTX)),
    n_missing_sales = sum(is.na(sales))
  )

#Find the missing values
missing_values <- Elas_numeric %>%
  filter(is.na(CORPINCTX)) %>%
  select(state, CORPINCTX, sales,year)

#the missing values consists of Texas app years 1976-1984, and the remaining states in 2023.  
#so, will remove these values and save as a new dataframe
#Don't want to remove and save as a completely NEW df from the elasticity and rates data.
#that is because, may want this.  Though it does mean, you will need to filter out texas and other nas
#hold it does... don't want to keep 2023, don't have data on it for any tax bases will eliminate it

#Create new dataframe
Elas_numeric2 <- Elas_numeric %>% 
  filter(!is.na(CORPINCTX))

#save it without the 2023 values
write.csv(Elas_numeric2,"elasticity_rates_jmp.csv")


#Now to try and run the regression, again
#Set up and run log linear regression in R, group by state.

#(a) estimate a non-log result for each state on its own
#(i) with no fixed effects CITrev ~ App + e

fitted_models = Elas_numeric2 %>% group_by(state) %>% do(model = lm(CORPINCTX ~ sales, data = .))
fitted_models$model
fitted_models %>% tidy(model)

#Could not estimate a simple result, because there isn't enough variation in the sales factor.
#therefore, will try simple non-log result with fixed effects
#(ii) CITrev ~ state + factor(year) + sales + rates + E
fitted_models = Elas_numeric2 %>% group_by(state) %>% do(model = lm(CORPINCTX ~ state + factor(year) + sales + rates, data = .))
fitted_models$model
fitted_models %>% tidy(model)

#(iii) CITrev ~ state + factor(year) + sales + rate + industry_dum + election_dum + phase_in_dum +E

#exactly what howard said would happen, happened.  The the sales factor does not
#have enough variation that is distinguishable from a state fixed effect.

#can try a log transform, but for right now, this needs more work before I can send results to Dougan.

