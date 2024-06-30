# make correlation matrix, https://www.displayr.com/how-to-create-a-correlation-matrix-in-r/
#and Synthetic DiD, https://synth-inference.github.io/synthdid/

#Data, will load this csv "detrend_per_capita.csv", contains detrended, per capita, real revenue

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
devtools::install_github("synth-inference/synthdid")
library(synthdid)
library(fixest)
# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")

#Remove X and X.1, REMOVE COLUMN when saving
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X,-X.1)

#Block treatment Archangelsky SynDiD for just Indiana and all states that don't 
#switch before 2014.  Merge back together
Ind <-Rev %>% 
  filter(State_Acronym=="IN") 


Control <- Rev %>%
  filter(year_effective>2014 | is.na(year_effective))

df <- Ind %>%
  bind_rows(Control)

#Now want to filter the df to just be until 2014
df2 <- df %>%
  filter(year<=2014)

#Had a problem with the full df, therefore will filter down to something smaller
# their df just has "State", "year", "packspercapita" and "treated" will do the same

filter_total <- df2 %>%
  select(State_Acronym,year,real_totRev_capita,treatment)

#still getting an error, even without any is.na. will continue audit of panel with following:

unique_states <- unique(filter_total$State_Acronym)
year_range <- range(filter_total$year)
complete_grid <- expand.grid(
  State_Acronym = unique(filter_total$State_Acronym),
  year = seq(min(filter_total$year), max(filter_total$year))
)

# Merge with the complete grid
balanced_panel <- complete_grid %>%
  left_join(filter_total, by = c("State_Acronym", "year"))

# Fill missing values using LOCF
balanced_panel <- balanced_panel %>%
  group_by(State_Acronym) %>%
  arrange(State_Acronym, year) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  ungroup()

#check every state has an entry for every year
balance_check <- table(balanced_panel$State_Acronym, balanced_panel$year)
print(balance_check)

#Now I think it is ready for Archangelsky DiD
# Load tobacco sales in long panel format.
# data(df2), did not work

# Transform to N*T matrix format required for synthdid,
# where N is the number of units and T the time periods.
#setup <- panel.matrices(filter_total, unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")
setup <- panel.matrices(filter_total,unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

sum(is.na(filter_total))

#for some reason it isn't running.  Will head to sleep and work on it tomorrow.