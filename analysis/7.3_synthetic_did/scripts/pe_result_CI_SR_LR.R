#Redo Main result script.
# main difference here is that I am going to compute the 3 year window explicitly as a short run for CI
#Then I will create a long run run loop.  I will save the plots for both.
#save the synthetic units in a dataframe.  Those units will be used to create the normalized shift data

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
#get per capita
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)
real_CI_cap <- real_CI %>%
  mutate(real_ci_cap = real_ci/population)

#save as new naive_ci because I do these steps each start anyways
write.csv(real_CI_cap,"naive_ci.csv",row.names=FALSE)

#lines 44- 88 run the dataframe filtering loop for short run Point Estimate,
#lines 96-152 calculate the point estimate -short run and other summary stats, create SR plot and save it.


#Create base dataframe that has real_ci_cap as dependent variable.
Filter_frac <-real_CI_cap %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci_cap,Post)
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
  current_year <- current_df$year_effective[1]
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci_cap", "Post")])
  #eliminate Inf lines and the state that has them for estimation, like Ohio 2009-2013
  states_with_inf <- current_df %>%
    group_by(State_Acronym) %>%
    filter(any(is.infinite(real_ci_cap))) %>%
    pull(State_Acronym) %>%
    unique()
  # Filter out those states from the dataframe
  current_df <- current_df %>%
    filter(!State_Acronym %in% states_with_inf)
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  # can increase replications by doing the following: se <- sqrt(vcov(current_tau_hat, method = 'placebo', replications=500))
  #default is 200.
  # Calculate the t-statistic, null hypothesis is zero
  t_statistic <- (as.numeric(current_tau_hat)-0) / se
  # Calculate the p-value
  p_value_two_tail <- 2 * pt(-abs(t_statistic), df = nrow(current_df) - 1)
  p_value_left_tail <-pt(t_statistic, df = nrow(current_df) - 1)
  p_value_right_tail <-pt(t_statistic, df = nrow(current_df) - 1, lower.tail = FALSE)
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Effective Year: %d\n', current_year))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value_two_tail))
  cat(sprintf('p-value-"left-tail": %1.4f\n', p_value_left_tail))
  cat(sprintf('p-value-"right-tail": %1.4f\n', p_value_right_tail))
  
  plot <- plot(current_tau_hat) +
    geom_vline(xintercept = current_year, linetype = "dotted", color = "black", linewidth = 1) +  # Checkered line at current_year
    labs(x = "Year", y = "Real Corporate Income per Capita") +
    ggtitle(paste("sDiD", state_name, "- Short Run Point Estimate")) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.line = element_line(linewidth = 0.5, colour = "black")
    )
  # Save each plot as an image
  ggsave(filename = paste0(state_name, "_sDiD_PE_short_run.png"), plot = plot)
}



#For the long run, I order the list and create a dataframe for each treatment state
#that dataframe just consists of the treated state and units in the control.

#create result list to store dataframe
result_list_long_run <- list()
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
  #keep just treatment df
  treatment_df <- df %>% filter(State_Name == treatment_state_name)
  #never treated df
  never_treated <- df %>%
    group_by(State_Name) %>%
    filter((Post == 0 & year_effective > 2021) | is.na(year_effective)) %>%
    filter(!(State_Acronym == "OH"))  # Exclude "OH" if necessary
  #not yet treated df
  not_yet_treated <- df %>%
    group_by(State_Name) %>%
    filter(Post == 1 & year_effective > 2021) %>%
    mutate(Post = 0) # need to do this so that they will be included in sDiD later
  
  #create full control group
  control <- rbind(never_treated, not_yet_treated) %>%
    distinct(State_Name)
  #final df step
  df <- df %>%
    filter(State_Name == treatment_state_name | State_Name %in% control$State_Name)%>%
    mutate(Post = ifelse(State_Name %in% not_yet_treated$State_Name, 0, Post))
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list_long_run[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2022) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}


#now each dataframe is just the never treated and the control group.
#initialize and empty dataframe to store the long run results
sDiD_point_estimate_list_CI_long_run <- data.frame(
  State = character(),
  Effective_Year = numeric(),
  Point_Estimate = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  T_Statistic = numeric(),
  P_Value_Two_Tail = numeric(),
  P_Value_Left_Tail = numeric(),
  P_Value_Right_Tail = numeric(),
  stringsAsFactors = FALSE
)


# Loop over each dataframe in result_list_long_run
for (state_name in names(result_list_long_run)) {
  # Access the dataframe
  current_df <- result_list_long_run[[state_name]]
  #Make the tibble from the result list a dataframe, easier for panel.matrics function
  current_df <- as.data.frame(current_df)
  current_year <- current_df$year_effective[1]
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci_cap", "Post")])
  #eliminate Inf lines and the state that has them for estimation, like Ohio 2009-2013
  states_with_inf <- current_df %>%
    group_by(State_Acronym) %>%
    filter(any(is.infinite(real_ci_cap))) %>%
    pull(State_Acronym) %>%
    unique()
  # Filter out those states from the dataframe
  current_df <- current_df %>%
    filter(!State_Acronym %in% states_with_inf)
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  # can increase replications by doing the following: se <- sqrt(vcov(current_tau_hat, method = 'placebo', replications=500))
  #default is 200.
  # Calculate the t-statistic, null hypothesis is zero
  t_statistic <- (as.numeric(current_tau_hat)-0) / se
  # Calculate the p-value
  p_value_two_tail <- 2 * pt(-abs(t_statistic), df = nrow(current_df) - 1)
  p_value_left_tail <-pt(t_statistic, df = nrow(current_df) - 1)
  p_value_right_tail <-pt(t_statistic, df = nrow(current_df) - 1, lower.tail = FALSE)
  
  #extract summary for controls
  tau_summary <- summary(current_tau_hat)
  
  # Extract controls
  controls <- if (!is.null(tau_summary$controls)) {
    control_data <- tau_summary$controls
    paste(row.names(control_data), round(control_data[, 1], 3), collapse = "; ")
  } else {
    NA
  }
  
  # Extract periods
  periods <- if (!is.null(tau_summary$periods)) {
    period_data <- tau_summary$periods
    paste(row.names(period_data), round(period_data[, 1], 3), collapse = "; ")
  } else {
    NA
  }
  
  # Extract dimensions
  dimensions <- if (!is.null(tau_summary$dimensions)) {
    dimension_data <- tau_summary$dimensions
    paste(row.names(dimension_data), round(dimension_data, 3), collapse = "; ")
  } else {
    NA
  }
  # Save the results to the dataframe
  sDiD_point_estimate_list_CI_long_run <- rbind(
    sDiD_point_estimate_list_CI_long_run,
    data.frame(
      State = state_name,
      Effective_Year = current_year,
      Point_Estimate = as.numeric(current_tau_hat),
      CI_Lower = as.numeric(current_tau_hat) - 1.96 * se,
      CI_Upper = as.numeric(current_tau_hat) + 1.96 * se,
      T_Statistic = t_statistic,
      P_Value_Two_Tail = p_value_two_tail,
      P_Value_Left_Tail = p_value_left_tail,
      P_Value_Right_Tail = p_value_right_tail,
      Controls = controls,      
      Periods = periods,        
      Dimensions = dimensions,
      stringsAsFactors = FALSE
    )
  )
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Effective Year: %d\n', current_year))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value_two_tail))
  cat(sprintf('p-value-"left-tail": %1.4f\n', p_value_left_tail))
  cat(sprintf('p-value-"right-tail": %1.4f\n', p_value_right_tail))
  

}

plot <- plot(current_tau_hat) +
  geom_vline(xintercept = current_year, linetype = "dotted", color = "black", linewidth = 1) +  # Checkered line at current_year
  labs(x = "Year", y = "Real Corporate Income per Capita") +
  ggtitle(paste("sDiD", state_name, "- Long Run Point Estimate")) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )
# Save each plot as an image
ggsave(filename = paste0(state_name, "_sDiD_PE_long_run.png"), plot = plot)

write.csv(sDiD_point_estimate_list_CI_long_run, "sDiD_point_estimate_list_CI_long_run.csv", row.names = FALSE)
