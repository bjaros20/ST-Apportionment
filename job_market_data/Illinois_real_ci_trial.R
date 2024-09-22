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

real_CI_cap <- real_CI %>%
  mutate(real_ci_cap = real_ci/population)

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



# WILL TRY AGAIN BUT WITH real_ci per capita
#load data
naive_ci<-read.csv("naive_ci.csv")

#Create per capita
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)

real_CI_cap <- real_CI %>%
  mutate(real_ci_cap = real_ci/population)

#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI_cap %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci_cap,Post)


#Create not-yet treated and never treated.  Create two control groups, one with AK/HA, one without
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

#Create Illinois without  Alaska or Hawaii... too volatile for revenue
#Illinois_noAK_HA <- Filter_frac %>%
#  filter(State_Name == "Illinois" | State_Name %in% control_noAK_HA$State_Name)

# Filter out rows with year <= 2 years after treatment_year
df <- Illinois %>%
  filter(year <= 1999 + 2)

## sDiD estimate
# Create the panel matrices for sDiD using synthdid
current_sDiD <- panel.matrices(df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")

# Calculate the synthetic difference-in-differences estimate
current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)

summary(current_tau_hat)

#Controls for spaghetti plot
top.controls = synthdid_controls(current_tau_hat)[1:10, , drop=FALSE]
plot(current_tau_hat, spaghetti.units=rownames(top.controls)) +
  labs(x = "Year", y = "Real, Naive Corporate Income per Capita") +
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


# now create the synthetic Illinois
# Filter the values for each control state for each time weight
# Filter each state's data for specific years and select real_ci_cap for that year
NH_values_1998 <- NH_values %>% filter(year == 1998) %>% pull(real_ci_cap)
ID_values_1998 <- ID_values %>% filter(year == 1998) %>% pull(real_ci_cap)
FL_values_1998 <- FL_values %>% filter(year == 1998) %>% pull(real_ci_cap)
MA_values_1998 <- MA_values %>% filter(year == 1998) %>% pull(real_ci_cap)
MT_values_1998 <- MT_values %>% filter(year == 1998) %>% pull(real_ci_cap)
NM_values_1998 <- NM_values %>% filter(year == 1998) %>% pull(real_ci_cap)

NH_values_1990 <- NH_values %>% filter(year == 1990) %>% pull(real_ci_cap)
ID_values_1990 <- ID_values %>% filter(year == 1990) %>% pull(real_ci_cap)
FL_values_1990 <- FL_values %>% filter(year == 1990) %>% pull(real_ci_cap)
MA_values_1990 <- MA_values %>% filter(year == 1990) %>% pull(real_ci_cap)
MT_values_1990 <- MT_values %>% filter(year == 1990) %>% pull(real_ci_cap)
NM_values_1990 <- NM_values %>% filter(year == 1990) %>% pull(real_ci_cap)

NH_values_1987 <- NH_values %>% filter(year == 1987) %>% pull(real_ci_cap)
ID_values_1987 <- ID_values %>% filter(year == 1987) %>% pull(real_ci_cap)
FL_values_1987 <- FL_values %>% filter(year == 1987) %>% pull(real_ci_cap)
MA_values_1987 <- MA_values %>% filter(year == 1987) %>% pull(real_ci_cap)
MT_values_1987 <- MT_values %>% filter(year == 1987) %>% pull(real_ci_cap)
NM_values_1987 <- NM_values %>% filter(year == 1987) %>% pull(real_ci_cap)


# Weights from the sDiD summary output
NH_weight <- 0.237
ID_weight <- 0.199
FL_weight <- 0.149
MA_weight <- 0.145
MT_weight <- 0.126
NM_weight <- 0.114

# Period weights from sDiD summary
weight_1998 <- 0.644
weight_1990 <- 0.213
weight_1987 <- 0.129


# Compute the synthetic control for each period by summing the weighted real_ci_cap values from each control state
syn_Illinois_1998 <- (NH_weight * NH_values_1998) + 
  (ID_weight * ID_values_1998) + 
  (FL_weight * FL_values_1998) + 
  (MA_weight * MA_values_1998) + 
  (MT_weight * MT_values_1998) + 
  (NM_weight * NM_values_1998)

syn_Illinois_1990 <- (NH_weight * NH_values_1990) + 
  (ID_weight * ID_values_1990) + 
  (FL_weight * FL_values_1990) + 
  (MA_weight * MA_values_1990) + 
  (MT_weight * MT_values_1990) + 
  (NM_weight * NM_values_1990)

syn_Illinois_1987 <- (NH_weight * NH_values_1987) + 
  (ID_weight * ID_values_1987) + 
  (FL_weight * FL_values_1987) + 
  (MA_weight * MA_values_1987) + 
  (MT_weight * MT_values_1987) + 
  (NM_weight * NM_values_1987)

# Combine the period weights to calculate the final synthetic Illinois
syn_Illinois <- (weight_1998 * syn_Illinois_1998) + 
  (weight_1990 * syn_Illinois_1990) + 
  (weight_1987 * syn_Illinois_1987)



## ATTEMPT TO INCORPORATE THE TIME AND UNIT WEIGHTS OVER TIME
# Define the time weights
time_weights <- c(1998 == 0.644, 1990 == 0.213, 1987 == 0.129) # Example time weights

# Unit weights for the states
unit_weights <- list(
  NH = 0.237,
  ID = 0.199,
  FL = 0.149,
  MA = 0.145,
  MT = 0.126,
  NM = 0.114
)

# Function to compute the synthetic Illinois for each year
compute_syn_illinois <- function(year) {
  syn_illinois <- (unit_weights$NH * NH_values %>% filter(year == !!year) %>% pull(real_ci_cap)) +
    (unit_weights$ID * ID_values %>% filter(year == !!year) %>% pull(real_ci_cap)) +
    (unit_weights$FL * FL_values %>% filter(year == !!year) %>% pull(real_ci_cap)) +
    (unit_weights$MA * MA_values %>% filter(year == !!year) %>% pull(real_ci_cap)) +
    (unit_weights$MT * MT_values %>% filter(year == !!year) %>% pull(real_ci_cap)) +
    (unit_weights$NM * NM_values %>% filter(year == !!year) %>% pull(real_ci_cap))
  
  return(syn_illinois)
}

# Compute synthetic Illinois for each year and apply time weights
years <- c(1998, 1990, 1987)
syn_illinois_over_time <- sapply(years, function(year) {
  time_weight <- time_weights[as.character(year)]
  syn_illinois_year <- compute_syn_illinois(year)
  return(time_weight * syn_illinois_year)
})


# Create the plot
ggplot(Illinois_plot_lr, aes(x = year)) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Illinois, color = "Synthetic"), size = 1.2) +
  labs(title = "Actual vs. Syn Naive CI per Cap- Illinois- inc. Time Weight-LR",
       x = "Year",
       y = "Real CI") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real IL", "Synthetic" = "Synthetic IL")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_fivethirtyeight()


##CREATE SDID per Capita for Illinois with the new output, I used the controls including AK and HA
unit_weights <- list(
  NH = 0.078,
  MA = 0.076,
  WV = 0.073,
  TN = 0.071,
  ID = 0.070,
  FL = 0.070,
  KS = 0.070,
  NM = 0.070,
  MS = 0.070,
  VA = 0.067,
  MT = 0.067,
  VT = 0.066,
  HI = 0.065
)

#Pull values for synthetic Illinois
NH_values <- Filter_frac %>%
  filter(State_Acronym == "NH") %>%
  select(State_Acronym, year, real_ci_cap)

MA_values <- Filter_frac %>%
  filter(State_Acronym == "MA") %>%
  select(State_Acronym, year, real_ci_cap)

WV_values <- Filter_frac %>%
  filter(State_Acronym == "WV") %>%
  select(State_Acronym, year, real_ci_cap)

TN_values <- Filter_frac %>%
  filter(State_Acronym == "TN") %>%
  select(State_Acronym, year, real_ci_cap)

ID_values <- Filter_frac %>%
  filter(State_Acronym == "ID") %>%
  select(State_Acronym, year, real_ci_cap)

FL_values <- Filter_frac %>%
  filter(State_Acronym == "FL") %>%
  select(State_Acronym, year, real_ci_cap)

KS_values <- Filter_frac %>%
  filter(State_Acronym == "KS") %>%
  select(State_Acronym, year, real_ci_cap)

NM_values <- Filter_frac %>%
  filter(State_Acronym == "NM") %>%
  select(State_Acronym, year, real_ci_cap)

MS_values <- Filter_frac %>%
  filter(State_Acronym == "MS") %>%
  select(State_Acronym, year, real_ci_cap)

VA_values <- Filter_frac %>%
  filter(State_Acronym == "VA") %>%
  select(State_Acronym, year, real_ci_cap)

MT_values <- Filter_frac %>%
  filter(State_Acronym == "MT") %>%
  select(State_Acronym, year, real_ci_cap)

VT_values <- Filter_frac %>%
  filter(State_Acronym == "VT") %>%
  select(State_Acronym, year, real_ci_cap)

HI_values <- Filter_frac %>%
  filter(State_Acronym == "HI") %>%
  select(State_Acronym, year, real_ci_cap)

#Now create the synthetic Illinois
syn_Illinois <- (unit_weights$NH * NH_values$real_ci_cap) + 
  (unit_weights$MA * MA_values$real_ci_cap) + 
  (unit_weights$WV * WV_values$real_ci_cap) + 
  (unit_weights$TN * TN_values$real_ci_cap) + 
  (unit_weights$ID * ID_values$real_ci_cap) + 
  (unit_weights$FL * FL_values$real_ci_cap) + 
  (unit_weights$KS * KS_values$real_ci_cap) + 
  (unit_weights$NM * NM_values$real_ci_cap) + 
  (unit_weights$MS * MS_values$real_ci_cap) + 
  (unit_weights$VA * VA_values$real_ci_cap) + 
  (unit_weights$MT * MT_values$real_ci_cap) + 
  (unit_weights$VT * VT_values$real_ci_cap) + 
  (unit_weights$HI * HI_values$real_ci_cap)


#Create df for plot
Illinois_plot <- Filter_frac %>%
  filter(State_Acronym == "IL") %>%
  select(State_Acronym, year, real_ci_cap) %>%
  mutate(syn_Illinois = syn_Illinois)


#Plot of synthetic and actual Illinois
ggplot(Illinois_plot, aes(x = year)) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Illinois, color = "Synthetic"), size = 1.2) +
  labs(title = "Actual vs. Synthetic Naive CI per Capita for Illinois",
            x = "Year",
            y = "Real CI") +
         scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                            labels = c("Actual" = "Real IL", "Synthetic" = "Synthetic IL")) +
         guides(color = guide_legend(title = "Corporate Income Type")) +
       theme_fivethirtyeight()


#try again, but remove Hawaii and Alaska for the control

#Create not-yet treated and never treated.  Create two control groups, one with AK/HA, one without
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

#Create Illinois without  Alaska or Hawaii... too volatile for revenue
Illinois_noAK_HA <- Filter_frac %>%
  filter(State_Name == "Illinois" | State_Name %in% control_noAK_HA$State_Name)

# Filter out rows with year <= 2 years after treatment_year
df <- Illinois_noAK_HA %>%
  filter(year <= 1999 + 2)

## sDiD estimate
# Create the panel matrices for sDiD using synthdid
current_sDiD <- panel.matrices(df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")

# Calculate the synthetic difference-in-differences estimate
current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)

summary(current_tau_hat)

#Controls for spaghetti plot
top.controls = synthdid_controls(current_tau_hat)[1:6, , drop=FALSE]
plot(current_tau_hat, spaghetti.units=rownames(top.controls)) +
  labs(x = "Year", y = "Real, Naive Corporate Income per Capita") +
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


#Creat plot for No AK or Hawaii list
# Weights from the sDiD summary output
NH_weight <- 0.237
ID_weight <- 0.199
FL_weight <- 0.149
MA_weight <- 0.145
MT_weight <- 0.126
NM_weight <- 0.114

#create synthetic Illinois
syn_Illinois_noAH <- (NH_weight * NH_values$real_ci_cap) + 
  (ID_weight * ID_values$real_ci_cap) + 
  (FL_weight * FL_values$real_ci_cap) + 
  (MA_weight * MA_values$real_ci_cap) + 
  (MT_weight * MT_values$real_ci_cap) + 
  (NM_weight * NM_values$real_ci_cap)


#Plot No AK or HA control
Illinois_noAH_plot <- Filter_frac %>%
  filter(State_Acronym == "IL") %>%
  select(State_Acronym, year, real_ci_cap) %>%
  mutate(syn_Illinois = syn_Illinois_noAH)


#Plot of synthetic and actual Illinois
ggplot(Illinois_plot, aes(x = year)) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Illinois_noAH, color = "Synthetic"), size = 1.2) +
  labs(title = "Actual vs. Synthetic Naive CI per Capita for Illinois- No AK or HA",
       x = "Year",
       y = "Real CI") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real IL", "Synthetic" = "Synthetic IL")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_fivethirtyeight()


#Now create a shift up
ill_diff <- Illinois_plot %>%
  filter(year == 1999) %>%
  summarize(shift = real_ci_cap - syn_Illinois)%>%
  mutate(syn_plus_shift = shift + syn_Illinois)


Illinois_shift <- Illinois_plot %>%
  mutate(syn_plus_shift = ill_diff$shift + syn_Illinois)

#Plot with shift
ggplot(Illinois_shift, aes(x = year)) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_plus_shift, color = "Synthetic Shift"), size = 1.2) +
  labs(title = "Actual vs. Synthetic Naive CI per Capita for Illinois- Shift",
       x = "Year",
       y = "Real CI") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real IL", "Synthetic" = "Synthetic IL")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_fivethirtyeight()



