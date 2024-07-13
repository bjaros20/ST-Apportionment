#Retroactive Switchers
#going to really commit to the sDiD rout.  That requires no anticipation and no spillovers
# I will estimate effects for each individual retroactive switcher, then aggregate
# see what the process yields from there.

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

write.csv(Rev,"detrend_per_capita.csv",row.names = FALSE)
#^hopefully that worked
library(readxl)
SSFA <- read_excel("ssfa_data_jmp.xlsx")

# Get just retroactive states
Retro <- SSFA %>%
  select(state,year_effective,type,retroactive) %>%
  filter(retroactive=="yes")%>%
  arrange(year_effective)



# Run sDiD package -Archangelsky- Just Maine, and none of Maine's neighbors in 
#the control group (New Hampshire and Massachusetts)
#switch before 1991, 3 years post switch.  Tight window
Maine <-Rev %>% 
  filter(State_Acronym=="ME") 

Control <- Rev %>%
  filter(year_effective>2010 | is.na(year_effective))%>%
  filter(State_Acronym != "NH" & State_Acronym != "MA" & State_Acronym != "AK" & State_Acronym != "HI")

df <- Maine %>%
  bind_rows(Control)

#Now want to filter the df to just be until 2010 (switch was in 2007)
df2 <- df %>%
  filter(year<=2010)
 

#filter down to smaller df
ME_rtr_cap <- df2 %>%
  select(State_Acronym,year,real_totRev_capita,treatment)
#check data is balanced
is.pbalanced(ME_rtr_cap)


#sDiD
Maine_totrev <- panel.matrices(ME_rtr_cap,unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

tau.hat = synthdid_estimate(Maine_totrev$Y, Maine_totrev$N0, Maine_totrev$T0)

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
#[1] "point estimate: -25.10"
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
#[1] "95% CI (-288.61, 238.40)"
plot(tau.hat)+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Plot- Maine= Treated")

#Summary of control group
print(summary(tau.hat))


#Compare the SynDiD to other estimators, set-up
tau.sc   = sc_estimate(Maine_totrev$Y, Maine_totrev$N0, Maine_totrev$T0)
tau.did  = did_estimate(Maine_totrev$Y, Maine_totrev$N0, Maine_totrev$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

#print estimate differences:
print(unlist(estimates))
# Diff-in-Diff      Synthetic Control Synthetic Diff-in-Diff 
#  38.40427              -38.69163              -25.10493 

#the plots relative to each other
synthdid_plot(estimates, se.method='placebo')+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("DiD v. Syn Control v. Syn DiD Plot- Syn Maine")

#spaghetti plots, includes all states for just Maine
estimate = synthdid_estimate(Maine_totrev$Y, Maine_totrev$N0, Maine_totrev$T0)

top.controls = synthdid_controls(estimate)[1:10, , drop=FALSE]

plot(estimate, spaghetti.units=rownames(top.controls)) + labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Spaghetti Plot- Maine")






#CIT for Maine
#filter down to smaller df
ME_cit_cap <- df2 %>%
  select(State_Acronym,year,real_cit_capita,treatment)
#check data is balanced
is.pbalanced(ME_cit_cap)

ME_treat2 <- ME_cit_cap %>%
  mutate(
    State_Acronym = as.character(State_Acronym),
    year = as.integer(year),
    real_totRev_capita = as.numeric(real_cit_capita),
    treatment = as.integer(treatment)
  )



#sDiD
Maine_citrev <- panel.matrices(ME_treat2,unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "treatment")

tau.hat = synthdid_estimate(Maine_citrev$Y, Maine_citrev$N0, Maine_citrev$T0)

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
#[1] "point estimate: 2.92"
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
#[1] "95% CI (-25.66, 31.49)"
plot(tau.hat)+labs(x="Year",y="Real CIT Revenue per Capita") + ggtitle("Synthetic DiD Plot- Maine= Treated- Corporate Inc")

#Summary of control group
print(summary(tau.hat))


#Compare the SynDiD to other estimators, set-up
tau.sc   = sc_estimate(Maine_citrev$Y, Maine_citrev$N0, Maine_citrev$T0)
tau.did  = did_estimate(Maine_citrev$Y, Maine_citrev$N0, Maine_citrev$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

#print estimate differences:
print(unlist(estimates))
# Diff-in-Diff      Synthetic Control Synthetic Diff-in-Diff 
#  2.390487               4.846076               2.915748 

#the plots relative to each other
synthdid_plot(estimates, se.method='placebo')+labs(x="Year",y="Real CIT Revenue per Capita") + ggtitle("DiD v. Syn Control v. Syn DiD Plot- Syn Maine CI")

#spaghetti plots, includes all states for just Maine
estimate = synthdid_estimate(Maine_citrev$Y, Maine_citrev$N0, Maine_citrev$T0)

top.controls = synthdid_controls(estimate)[1:10, , drop=FALSE]

plot(estimate, spaghetti.units=rownames(top.controls)) + labs(x="Year",y="Real CIT Revenue per Capita") + ggtitle("Synthetic DiD Spaghetti Plot- Maine CI")




#NEXT STATE Connecticut in 2016
Conn <-Rev %>% 
  filter(State_Acronym=="CT") 

Control1 <- Rev %>%
  filter(year_effective>2019 | is.na(year_effective))%>%
  filter(State_Acronym != "RI" & State_Acronym != "MA" & State_Acronym != "OH" & State_Acronym != "AK" & State_Acronym != "HI" & State_Acronym != "NY")

Conn2 <- Conn %>%
  bind_rows(Control1)

#Now want to filter the df to just be until 2019 (switch was in 2016)
Conn3 <- Conn2 %>%
  filter(year<=2019)


#filter down to smaller df
CT_rtr_cap <- Conn3 %>%
  select(State_Acronym,year,real_totRev_capita,treatment)
#check data is balanced
is.pbalanced(CT_rtr_cap)
#[ NOT BALANCED]
complete_panel <- CT_rtr_cap %>%
  select(State_Acronym, year) %>%
  distinct() %>%
  complete(State_Acronym, year)

# Identify rows in the complete panel that are missing in the original data
missing_rows <- complete_panel %>%
  anti_join(CT_rtr_cap, by = c("State_Acronym", "year"))

# Print the missing rows
print(missing_rows)

#ELIMINATE OH after 2014 phase out

# For some reason the treatment column doesn't have CT, but it has it in the post column
#need Iowa to be treated
CT_treat <-CT_rtr_cap %>%
  mutate(treatment = if_else(State_Acronym == "CT" & year >= 2016, 1, treatment))

CT_treat <- CT_treat %>%
  mutate(
    State_Acronym = as.character(State_Acronym),
    year = as.integer(year),
    real_totRev_capita = as.numeric(real_totRev_capita),
    treatment = as.integer(treatment)
  )

#CLEAN THIS FOR TREATMENT GOING FORWARD. TREATMENT IN REVE is NUM, needs to be INTEGER

#sDiD
CT_totrev <- panel.matrices(CT_treat,unit = "State_Acronym", time = "year", outcome = "real_totRev_capita", treatment = "treatment")

tau.hat = synthdid_estimate(CT_totrev$Y, CT_totrev$N0, CT_totrev$T0)

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
#[1] "point estimate: 112.53"
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
#[1] "95% CI (-29.63, 254.70)"
plot(tau.hat)+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Plot- Connecticut= Treated")

#Summary of control group
print(summary(tau.hat))


#Compare the SynDiD to other estimators, set-up
tau.sc   = sc_estimate(CT_totrev$Y, CT_totrev$N0, CT_totrev$T0)
tau.did  = did_estimate(CT_totrev$Y, CT_totrev$N0, CT_totrev$T0)
estimates = list(tau.did, tau.sc, tau.hat)
names(estimates) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')

#print estimate differences:
print(unlist(estimates))
# Diff-in-Diff      Synthetic Control Synthetic Diff-in-Diff 
#  380.2032               141.1199               112.5326 

#the plots relative to each other
synthdid_plot(estimates, se.method='placebo')+labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("DiD v. Syn Control v. Syn DiD Plot- Syn Connecticut")

#spaghetti plots, includes all states for just Maine
estimate = synthdid_estimate(CT_totrev$Y, CT_totrev$N0, CT_totrev$T0)

top.controls = synthdid_controls(estimate)[1:10, , drop=FALSE]

plot(estimate, spaghetti.units=rownames(top.controls)) + labs(x="Year",y="Real Total Revenue per Capita") + ggtitle("Synthetic DiD Spaghetti Plot- Connecticut")




