#Elasticity Results for All the Tax Bases
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)
library(broom) #for tidying up the group by regressions
library(plm) # Two way fixed effects
library(purrr) # isolate model that can't be tidied
library(plyr)

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
write.csv(Elas_numeric,"elasticity_rates_jmp_edit.csv")

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
fitted_models = Elas_numeric %>% group_by(state) %>% do(model = lm(CORPINCTX ~ state + factor(year) + sales + rates, data = .))
fitted_models$model
fitted_models %>% tidy(model)

#(iii) CITrev ~ state + factor(year) + sales + rate + industry_dum + election_dum + phase_in_dum +E

#exactly what howard said would happen, happened.  The the sales factor does not
#have enough variation that is distinguishable from a state fixed effect.

#can try a log transform, but for right now, this needs more work before I can send results to Dougan.

#After some thought, for a non-log result, I think the best course of action is 
#go for log-linear regression

#Will check what factor is causing the problem

# Check unique levels for state
unique(Elas_numeric$state)

# Check unique levels for year
unique(Elas_numeric$year)

# Check unique levels for sales
unique(Elas_numeric$sales)

# Check unique levels for rates
unique(Elas_numeric$rates)

#It wasn't the sales factor, it was the NA for rates
na_rates <- Elas_numeric %>%
  filter(is.na(rates))

#going to impute missing rates by using tidyr package to use the preceding rate
Elas_numeric_filled <- Elas_numeric %>%
  group_by(state) %>%
  arrange(state, year) %>%
  fill(rates, .direction = "down") %>%
  ungroup()

# Verify all rates have been filled
print(Elas_numeric_filled %>% filter(is.na(rates)))

#convert year to factor
Elas_numeric_filled$year <- as.factor(Elas_numeric_filled$year)


# it worked, will run the regression again.
#(ii) CITrev ~ state + factor(year) + sales + rates + E
fitted_models = Elas_numeric_filled %>% group_by(state) %>% do(model = lm(CORPINCTX ~ state + factor(year) + sales + rates, data = .))
fitted_models$model
fitted_models %>% tidy(model)

#still getting factor with only 2 or more levels error

check_levels <- Elas_numeric_filled %>%
  group_by(state) %>%
  summarise(n_year_levels = n_distinct(year), n_state_levels = n_distinct(state))

print(check_levels)

#Remove the state from the regression because it is already present for grouping
fitted_models <- Elas_numeric_filled %>%
  group_by(state) %>%
  do(model = lm(CORPINCTX ~ factor(year) + sales + rates, data = .))

#received NA results for this.
fitted_models$model

#tidy package is not running.
fitted_models %>% tidy(model)

#Above wasn't working.  Added the plm package, will try to get the two way fixed effect result

#going to try and tidy the models with tidyverse, threw errors
tidy_results <- fitted_models %>%
  mutate(tidy_model = map(model, ~broom::tidy(.)))%>%
  unnest(cols = c(tidy_model))

#tidying the result still gave errors, check for NA coeff
problematic_models <- fitted_models %>%
  mutate(has_na_coeff = map_lgl(model,~any(is.na(coef(.)))))

#above still had errors, going to try locate models that can't be tidied
safe_tidy <-safely(broom::tidy)

#Apply safe tidy function to each model
tidied_models <- fitted_models %>%
  mutate(tidy_model = list(map(model,~safe_tidy(.))))


# still received an error, different strategy, use lm and plm on states
#break up the data by state, fit model to each piece and return

models <- dlply(Elas_numeric_filled, "state", function(df)
  lm(CORPINCTX~ sales, data = df))

#apply coef to each model and return a data frame, works if just do one i.e. sales
#need to find a way to do it for fixed effects. but this worked
# ie. the second one did: https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
ldply(models,coef)

# Print the summary of each model
l_ply(models, summary, .print = TRUE)





#(B) Estimate a simple Log transform result
#Log(Rev) ~ Bo + B1 Log (sales) + E

#Create a log CIt revenue column
Elas_log <- Elas_numeric_filled %>%
  mutate(log_CIT=log(CORPINCTX))%>%
  mutate(log_sales=log(sales))%>%
  mutate(log_rate=log(rates))


#(c) estimate a log transform with time and fixed effects
#state FE, year FE, rate FE, dummy FE




#(d) get a result for each state (i) chart and (ii) plot