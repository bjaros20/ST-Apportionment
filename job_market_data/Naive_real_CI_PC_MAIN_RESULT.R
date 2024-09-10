# Main result: Naive, Real, Corporate Income per capita
#Empirical Approach, sDiD
# Care about the t-statistic, need to BOOTSTRAP with replace for normal distribution
#Run a one-sided Significance test

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

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")
Corp <-read.csv("CIT_sDID_loop.csv")
naive_ci<-read.csv("naive_ci.csv")

naive_ci <- naive_ci %>%
  select(-X)

write.csv(naive_ci,"naive_ci.csv", row.names = FALSE)




#BELOW IS THE LOOP RUN FOR RESULT 3.8
# Next thing to estimate, Real Corporate Income, base year 1983-1984
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)

#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci,Post)

filt_Corp <-Filter_frac


#Create State Dataframes
#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1


while (TRUE) {
  #Reset to original each start
  df <-filt_Corp
  
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2022) {break}
  
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state$State_Name
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, 
  df <- df %>% 
    filter(is.na(year_effective) | is.na(State_Acronym) | year_effective >= treatment_year)
  
  # removes states treated in same year
  #the year_effective=treatment_year are filtered out if they are not 'treatment_state'
  df <- df %>%
    filter(is.na(year_effective) | (!(State_Name != treatment_state_name & year_effective == treatment_year)))
  
  
  # Filter out rows with year <= 2 years after treatment_year
  df <- df %>%
    filter(year <= treatment_year + 2)
  
  #filter out any states that get treated within 2 years of treatment
  df <- df %>%
    filter(is.na(year_effective) | (!(year_effective > treatment_year & year_effective<= treatment_year + 2)))
  
  #filter out Ohio after treatment_year >=2012, because OH eliminates CI in 2014
  df <- df %>%
    filter(!(treatment_year >= 2012 & State_Name == "Ohio"))
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2022) {break}
  
  #increment counter
  counter <-counter + 1
  
  #empty dataframe break
  if (nrow(df) == 0) {break}
}



#STEP 2, create just a point estimate loop with t-stat and confidence intervals

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  #Make the tibble from the result list a dataframe, easier for panel.matrics function
  current_df <- as.data.frame(current_df)
  
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci", "Post")])
  
  #eliminate Inf lines and the state that has them for estimation, like Ohio 2009-2013
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
  
  # Calculate the t-statistic
  t_statistic <- as.numeric(current_tau_hat) / se
  
  # Calculate the p-value
  p_value <- 2 * pt(abs(t_statistic), df = nrow(current_df) - 1, lower.tail = FALSE)
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
  #  print(summary(current_tau_hat))
}


#Summary Statistics for real_ci
summary_stats <- Filter_frac %>%
  filter(!is.na(real_ci) & is.finite(real_ci)) %>%  # Remove rows with NA in nat_share
  group_by(State_Name) %>%
  summarize(
    mean = mean(real_ci, na.rm = TRUE),
    median = median(real_ci, na.rm = TRUE),
    IQR = IQR(real_ci, na.rm = TRUE),
    min = min(real_ci, na.rm = TRUE),
    max = max(real_ci, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))

#Want plots for Michigan, Ill, NY, NJ, and CA

# List of states to plot
states_to_plot <- c("Michigan","Illinois", "California", "New Jersey", "New York")

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Only proceed if the state is in the states_to_plot list
  if (state_name %in% states_to_plot) {
    
    # Access the dataframe
    current_df <- result_list[[state_name]]
    
    # Convert tibble to dataframe
    current_df <- as.data.frame(current_df)
    
    # Drop rows with NA for nat_share_ci and filter out states with Inf values
    current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci", "Post")])
    states_with_inf <- current_df %>%
      group_by(State_Acronym) %>%
      filter(any(is.infinite(real_ci))) %>%
      pull(State_Acronym) %>%
      unique()
    current_df <- current_df %>%
      filter(!State_Acronym %in% states_with_inf)
    
    # Create the panel matrices for sDiD
    current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci", treatment = "Post")
    
    # Calculate the synthetic difference-in-differences estimate
    current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
    
    # Plot and save the sDiD estimate
    plot <- plot(current_tau_hat) +
      labs(x = "Year", y = "Real Naive Corporate Income") +
      ggtitle(paste("Synthetic Difference in Difference -", state_name, "= treated")) +
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
    
    # Save each plot as an image
    ggsave(filename = paste0(state_name, "_sDiD_plot_real_naive_CI.png"), plot = plot)
  }
}
