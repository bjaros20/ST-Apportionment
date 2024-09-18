# Need to replicate Table 5 in Edmiston's Strategic Apportionment Paper.
# We have Corporate Income, Have Individual income tax collections, and total Tax Collections

# calculate short run percentage change in revenue Vs. Counterfactual.
#Independent action (only treatment state changes Vs. clean control)
#Calculate long run percentage change Vs. all change.


#Think I want to Use the Naive, Real Corporate Income.  Plot is just that NRCI for treatment
# synthetic control.

#Calculate the point estimate for sDiD, DiD and SC for 1 year, 2 year, and 3 year.  
# Use the point estimates to calculate percentage change.

#Modify initial dataframe creating process.

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


#Real Corporate Income

# Next thing to estimate, Real Corporate Income, base year 1983-1984
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)


#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci,Post)
filt_Corp <-Filter_frac

#Think about Illinois

result_list <- list()  # List to store all dataframes

# Make a copy of the original dataframe to work with
original_df <- filt_Corp

# Set treatment_state to Illinois
treatment_state_name <- "Illinois"

# Counter for tracking treatment year filtering
year_counter <- 0  # Initialize year increment

# Continue until we break manually or conditions are met
while (TRUE) {
  
  # Reset the dataframe to the original each time
  df <- original_df
  
  # Arrange by year_effective and select Illinois as the treatment state
  treatment_state <- df %>% filter(State_Name == treatment_state_name)
  
  # Get the treatment year for Illinois
  treatment_year <- treatment_state$year_effective[1]
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, 
  df <- df %>% 
    filter(is.na(year_effective) | is.na(State_Acronym) | year_effective >= treatment_year)
  
  #Remove prior treated states that are not the treatment state
  df <- df %>%
    filter(!(Post == 1 & State_Name != treatment_state_name))
  
  # removes states treated in same year
  #the year_effective=treatment_year are filtered out if they are not 'treatment_state'
  df <- df %>%
    filter(is.na(year_effective) | (!(State_Name != treatment_state_name & year_effective == treatment_year)))
  
  
  # Filter the dataframe to exclude rows based on treatment year and year_counter
  # Filter out rows with year <= treatment_year + year_counter
  df_filtered <- df %>%
    filter(year <= treatment_year + year_counter)
  
  # Filter out any states that get treated within year_counter years of treatment
  df_filtered <- df_filtered %>%
    filter(is.na(year_effective) | (!(year_effective > treatment_year & year_effective <= treatment_year + year_counter)))
  
  # Filter out Ohio after treatment_year >= 2012, as per your requirement
  df_filtered <- df_filtered %>%
    filter(!(treatment_year >= 2012 & State_Name == "Ohio"))
  
  # Create a unique name for each dataframe for Illinois with the year increment
  treatment_state_name_iteration <- paste0(treatment_state_name, "_", year_counter)
  
  # Store the dataframe for this iteration of Illinois in the result_list
  assign(treatment_state_name_iteration, df_filtered)
  result_list[[treatment_state_name_iteration]] <- df_filtered
  
  # Check if the treatment year has reached or exceeded 2022 and break
  if (treatment_year + year_counter >= 2022) {break}
  
  # Increment the year counter
  year_counter <- year_counter + 1
  
  # Break the loop if the dataframe is empty
  if (nrow(df_filtered) == 0) {break}
}


#Got 23 Years for Illinois

point_estimate_list <- list()
plot_list <- list()  # List to store plots

# Initialize a counter
counter <- 1

# Loop over each dataframe in result_list using a counter
for (df_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[df_name]]
  
  # Make the tibble from the result list a dataframe, easier for panel.matrices function
  current_df <- as.data.frame(current_df)
  
  # Extract the treatment year
  current_year <- current_df$year_effective[1]
  
  # Drop states that have NA for ratio
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci", "Post")])
  
  # Eliminate Inf lines and the state that has them for estimation
  states_with_inf <- current_df %>%
    group_by(State_Acronym) %>%
    filter(any(is.infinite(real_ci))) %>%
    pull(State_Acronym) %>%
    unique()
  
  # Filter out those states from the dataframe
  current_df <- current_df %>%
    filter(!State_Acronym %in% states_with_inf)
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  
  # Dynamically create a variable name like 'current_tau_hat_Illinois0'
  tau_hat_name <- paste0("current_tau_hat_", df_name)
  assign(tau_hat_name, current_tau_hat)
  
  # Calculate the t-statistic
  t_statistic <- (as.numeric(current_tau_hat)-0) / se
  
  # Calculate the p-value
  p_value_two_tail <- 2 * pt(-abs(t_statistic), df = nrow(current_df) - 1)
  
  # Print the results
  cat(sprintf('DataFrame Index: %s\n', df_name))
  cat(sprintf('Effective Year: %d\n', current_year))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('Std Error: %1.2f\n', se))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value_two_tail))

  
  # Increment the counter
  counter <- counter + 1
}

states_to_plot <- c("Illinois", "California", "New Jersey", "New York")

# Plot and save the sDiD estimate
plot <- plot(current_tau_hat_Illinois_23) +
  labs(x = "Year", y = "Share of Nationwide Corporate Income") +
  ggtitle(paste("Synthetic Difference in Difference -", df_name, "= treated")) +
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


top.controls = synthdid_controls(current_tau_hat_Illinois_0)[1:10, , drop=FALSE]
plot(current_tau_hat_Illinois_0, spaghetti.units=rownames(top.controls))



