# Loop for Dataframes- Short Run CI percentage change
#going to try and run the percentage change using Synthetic Control, not sDiD


#Load Packages
install.packages("cli")
library(cli)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(boot)
library(ggthemes)
install.packages("Synth")
install.packages("kernlab")
install.packages("kernlab", type = "binary")
library(kernlab)
library(Synth)
install.packages('tidysynth')
library(tidysynth)
# install.packages("devtools")
devtools::install_github("edunford/tidysynth")

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Laod data
naive_ci<-read.csv("naive_ci.csv")
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)
real_CI_cap <- real_CI %>%
  mutate(real_ci_cap = real_ci/population)


#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI_cap %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci_cap,Post,logRealCitRev,population)

not_yet_treated <- Filter_frac %>%
  group_by(State_Name)%>%
  filter(Post == 1 & year_effective > 2021)

never_treated <- Filter_frac %>%
  group_by(State_Name)%>%
  filter(Post == 0 & year_effective >2021 | is.na( year_effective)) %>%
  filter(!(State_Acronym == "OH"))

control <- rbind(never_treated,not_yet_treated) %>%
  distinct(State_Name)

#control_noAK_HA <-rbind(never_treated,not_yet_treated)%>%
#  filter(!(State_Name == "Alaska" | State_Name == "Hawaii")) %>%
#  distinct(State_Name)


#Create Treatment dataframe for just Illinois
#Create Illinois Panel from Illinois and control
Illinois <- Filter_frac %>%
  filter(State_Name == "Illinois" | State_Name %in% control$State_Name)

# synthetic control
illinois_sc <- Illinois %>%
  synthetic_control(outcome = real_ci_cap,
                    unit = State_Name,
                    time= year,
                    i_unit = "Illinois",
                    i_time = 1999,
                    generate_placebos = TRUE) %>%
  # Generate predictors (you can adjust these as needed)
  generate_predictor(time_window = 1976:1999,
                     population = mean(population, na.rm = TRUE)) %>%  # required to put one predictor. population
    # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 1976:1999,  # Time to use in the optimization task
                 margin_ipop = .02,
                 sigf_ipop = 7,
                 bound_ipop = 6  # Optimizer options
) %>%
  generate_control()

summary(illinois_sc)


illinois_sc %>% plot_trends()

illinois_sc %>% plot_differences()

illinois_sc %>% plot_weights()
