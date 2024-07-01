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
  filter(year<=2013)

#Had a problem with the full df, therefore will filter down to something smaller
# their df just has "State", "year", "packspercapita" and "treated" will do the same

filter_total2 <- df2 %>%
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

data("filter_total")


# Transform to N*T matrix format required for synthdid,
# where N is the number of units and T the time periods.
#setup <- panel.matrices(filter_total, unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")
setup <- panel.matrices(as.data.frame(as_tibble(filter_total)),unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

sum(is.na(filter_total))

setup <-panel.matrices(filter_total)

#for some reason it isn't running.  Will head to sleep and work on it tomorrow.
pdata <- pdata.frame(filter_total, index = c("State_Acronym", "year"))

#check balance
is_balanced <- is.pbalanced(pdata)
print(is_balanced)
#FALSE, will try to balance it

# Create a complete grid of all state-year combinations
complete_panel <- expand.grid(State_Acronym = unique(filter_total$State_Acronym),
                              year = seq(min(filter_total$year), max(filter_total$year)))

# Merge with your data to find missing combinations
merged_data <- merge(complete_panel, filter_total, by = c("State_Acronym", "year"), all.x = TRUE)

# Identify missing values
missing_data <- merged_data[is.na(merged_data$real_totRev_capita), ]
print(missing_data)

#   State_Acronym year real_totRev_capita treatment
#897            OH 2014                 NA        NA

# Will proceed by removing 2013, because Ohio could be a good control for indiana

#check balance on filter_total2
pdata2 <- pdata.frame(filter_total2, index = c("State_Acronym", "year"))

#check balance
is_balanced2 <- is.pbalanced(pdata2)
print(is_balanced2)
#TRUE

#Full steam ahead with https://synth-inference.github.io/synthdid/
setup <- panel.matrices(filter_total2,unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Plot- Indiana = Treated")

#Summary stats for Indiana Syn control group
print(summary(tau.hat))

#control unit contribution plot
synthdid_units_plot(tau.hat, se.method='placebo') +labs(x="Control States") + ggtitle("Unit Contribution Plot- Syn DiD Indiana")

#checking for pre-treatment and parallel trends
plot(tau.hat, overlay=1,  se.method='placebo') +labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Pre-Treatment and Parallel Trends Plot- Syn DiD Indiana")

#shift control's trajectory towards Indiana
plot(tau.hat, overlay=.8, se.method='placebo') +labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Shift Control Trajectory Towards Indiana Plot- Syn DiD Indiana")

#Compare the SynDiD to other estimators, set-up
tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

#print estimate differences:
print(unlist(estimates))
# Diff-in-Diff      Synthetic Control Synthetic Diff-in-Diff 
# -73.47064               81.42596               29.68875 

#the plots relative to each other
synthdid_plot(estimates, se.method='placebo')+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("DiD v. Syn Control v. Syn DiD Plot- Syn DiD Indiana")

#Synthetic control units plot
synthdid_units_plot(estimates, se.method='placebo')


#Customize the plots
synthdid_plot(estimates, facet.vertical=FALSE,
              control.name='control', treated.name='Indiana',
              lambda.comparable=TRUE, se.method = 'none',
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.26,.07), legend.direction='horizontal',
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank())

#spaghetti plots, includes all states for just Indiana
estimate = synthdid_estimate(setup$Y, setup$N0, setup$T0)

top.controls = synthdid_controls(estimate)[1:10, , drop=FALSE]
plot(estimate, spaghetti.units=rownames(top.controls)) + labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Spaghetti Plot- Indiana")
                                                              
                                                              
                                                              
