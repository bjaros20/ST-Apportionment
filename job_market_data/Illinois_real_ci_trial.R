# Create a 3 year sDiD for each state.
#panel to just be Not-yet-treated and Never Treated States

#Get the time and unit weights to recreate and extend the plot of the 
#synthetic state.

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
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Laod data
naive_ci<-read.csv("naive_ci.csv")
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)


#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci,Post)

not_yet_treated <- Filter_frac %>%
  group_by(State_Name)%>%
  filter(Post == 1 & year_effective > 2021)

never_treated <- Filter_frac %>%
  group_by(State_Name)%>%
  filter(Post == 0 & year_effective >2021 | is.na( year_effective)) %>%
  filter(!(State_Acronym == "OH"))

control <- rbind(never_treated,not_yet_treated) %>%
  distinct(State_Name)

control_noAK_HA <-rbind(never_treated,not_yet_treated)%>%
  filter(!(State_Name == "Alaska" | State_Name == "Hawaii")) %>%
  distinct(State_Name)


#Create Illinois Panel from Illinois and control
Illinois <- Filter_frac %>%
  filter(State_Name == "Illinois" | State_Name %in% control$State_Name)

#Set treatment state
treatment_state_name <- "Illinois"

# Get the treatment year for Illinois
treatment_year <- treatment_state$year_effective[1]

# Arrange by year_effective and select Illinois as the treatment state
treatment_state <- df %>% filter(State_Name == treatment_state_name)

# Filter out rows with year <= 2 years after treatment_year
df <- Illinois %>%
  filter(year <= 1999 + 2)


#Create Illinois without  Alaska or Hawaii... too volatile for revenue
Illinois_noAK_HA <- Filter_frac %>%
  filter(State_Name == "Illinois" | State_Name %in% control_noAK_HA$State_Name)

# Filter out rows with year <= 2 years after treatment_year
df <- Illinois_noAK_HA %>%
  filter(year <= 1999 + 2)

#From this dataframe- compute sDiD, get sDiD summary, and plot it

## sDiD estimate
# Create the panel matrices for sDiD using synthdid
current_sDiD <- panel.matrices(df, unit = "State_Acronym", time = "year", outcome = "real_ci", treatment = "Post")

# Calculate the synthetic difference-in-differences estimate
current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
se <- sqrt(vcov(current_tau_hat, method = 'placebo'))

# Calculate the t-statistic
t_statistic <- (as.numeric(current_tau_hat)-0) / se

# Calculate the p-value
p_value_two_tail <- 2 * pt(-abs(t_statistic), df = nrow(df) - 1)


# Plots for Illinois against Never and Not yet Treated

# Plot and save the sDiD estimate
plot <- plot(current_tau_hat) +
  labs(x = "Year", y = "Real, Naive Corporate Income") +
  ggtitle(paste("sDiD Illinois and Never + Not Yet Treated Control")) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )

# Print the plot
print(plot)


summary(current_tau_hat)


top.controls = synthdid_controls(current_tau_hat)[1:2, , drop=FALSE]
plot(current_tau_hat, spaghetti.units=rownames(top.controls)) +
  labs(x = "Year", y = "Real, Naive Corporate Income") +
  ggtitle(paste("Spaghetti Plot sDiD Illinois Top Controls")) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )


# Create Synthetic Illinois
FL_values <- Filter_frac %>%
  filter(State_Acronym == "FL") %>%
  select(State_Acronym,year,real_ci)
  
  
  # Replace with FL values for 1999-2022
MA_values <- Filter_frac %>%
  filter(State_Acronym == "MA") %>%
  select(State_Acronym,year,real_ci)


# Weights from the Summary output
FL_weight <- 0.896
MA_weight <- 0.104

# Compute the synthetic control for Illinois
syn_Illinois <- c((FL_weight * FL_values$real_ci) + (MA_weight * MA_values$real_ci))

# View the synthetic control values
Illinois_plot <- Filter_frac %>%
  filter(State_Acronym == "IL") %>%
  select(State_Acronym,year,real_ci) %>%
  mutate(syn_Illinois = syn_Illinois)


# Create the plot
ggplot(Illinois_plot, aes(x = year)) +
  geom_line(aes(y = real_ci, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Illinois, color = "Synthetic"), size = 1.2) +
  labs(title = "Actual vs. Synthetic Naive Corporate Income for Illinois",
       x = "Year",
       y = "Real CI") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real IL", "Synthetic" = "Synthetic IL")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_fivethirtyeight()


