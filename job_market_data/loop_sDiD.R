# Create sDiD loop for Corporate income Tax collections
#can re-run loop for no spillovers

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

install.packages('boot')
library(boot)

install.packages("ggthemes")
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X,-X.1)

#Load all rev
#All_rev <-read.csv("FRED_rev_all_states.csv")

#for Corporate, will just need CIT_Rev, currently in the "Rev" dataframe. NO WY, WA, TX, SD, OH, NV
Corp <- Rev %>%
  select(State_Acronym,year,CORPINCTX,sales:log_CIT,year_effective,Post:rel_year,State_Name,real_cit,logRealCitRev,real_cit_capita,detrended_CIT_capita)
write.csv(Corp,"CIT_sDID_loop.csv",row.names=FALSE)

Corp <-read.csv("CIT_sDID_loop.csv")

#filter Corporate
filt_Corp <- Corp %>%
  select(State_Acronym,year,year_effective,Post,real_cit_capita,State_Name)


#Step 1- Create Dataframe for the state
#group_by(State_Acronym)
#select state for treatment- start with earliest filt_Corp$year_effective
#filter- remove all State_Acronym < year_effecive for treatment state

#1a create a dataframe named after treatment state, call dataframe value in "State_Name"

#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1

#the break on 2022 as the treatment year is right before the counter after df created

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
  #first half of filter keeps no treatment states in, emoves states that were already treated
  df <- df %>% 
  filter(is.na(year_effective) | year_effective >= treatment_year)
  
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




# Step 2- Use that dataframe to create point estimate, se, CI, & summary statistics for state

#Point estimate and summary statistics list, used as background for loop below.
point_estimate_list <- list()

#sDiD panel Matrice
Iowa_sDiD <- panel.matrices(Iowa,unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "Post")
#sDiD
Iowa_tau.hat = synthdid_estimate(Iowa_sDiD$Y, Iowa_sDiD$N0, Iowa_sDiD$T0)

se = sqrt(vcov(Iowa_tau.hat, method='placebo'))
#Integrating this step into loop below
boot(Iowa_tau.hat,Iowa_sDiD,100)

#point Estimate
sprintf('point estimate: %1.2f', Iowa_tau.hat)
#Confidence Interval
sprintf('95%% CI (%1.2f, %1.2f)', Iowa_tau.hat - 1.96 * se, Iowa_tau.hat + 1.96 * se)
#summary statistics
print(summary(Iowa_tau.hat))



#Try point estimate loop
point_estimate_list <- list()


# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  
  # Print the point estimate and confidence interval
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  
  # Summary statistics
  print(summary(current_tau_hat))
  
  # Compute summary statistics for real_cit_capita
  mean_real_cit_capita <- mean(current_df$real_cit_capita, na.rm = TRUE)
  median_real_cit_capita <- median(current_df$real_cit_capita, na.rm = TRUE)
  IQR_real_cit_capita <- IQR(current_df$real_cit_capita, na.rm = TRUE)
  min_real_cit_capita <- min(current_df$real_cit_capita, na.rm = TRUE)
  max_real_cit_capita <- max(current_df$real_cit_capita, na.rm = TRUE)
  
  # Print the summary statistics for real_cit_capita
  cat(sprintf('Summary statistics for real_cit_capita in %s:\n', state_name))
  cat(sprintf('Mean: %1.2f\n', mean_real_cit_capita))
  cat(sprintf('Median: %1.2f\n', median_real_cit_capita))
  cat(sprintf('IQR: %1.2f\n', IQR_real_cit_capita))
  cat(sprintf('Min: %1.2f\n', min_real_cit_capita))
  cat(sprintf('Max: %1.2f\n', max_real_cit_capita))
  
  # Store the results in point_estimate_list
  point_estimate_list[[state_name]] <- list(
    point_estimate = current_tau_hat,
    se = se,
    CI_lower = current_tau_hat - 1.96 * se,
    CI_upper = current_tau_hat + 1.96 * se,
    summary = summary(current_tau_hat),
    mean_real_cit_capita = mean_real_cit_capita,
    median_real_cit_capita = median_real_cit_capita,
    IQR_real_cit_capita = IQR_real_cit_capita,
    min_real_cit_capita = min_real_cit_capita,
    max_real_cit_capita = max_real_cit_capita
  )
  
  plot <- plot(current_tau_hat) +
    labs(x = "Year", y = "Real Corporate Income Tax Revenue per Capita") +
    ggtitle(paste("Synthetic Difference in Difference -", state_name, "= treated")) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12,),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.line = element_line(linewidth = 0.5, colour = "black")
    )
  
  # Save each plot as an image
  ggsave(filename = paste0(state_name, "_sDiD_plot.png"), plot = plot)
  
}



# Earlier loop with P-values and T-stat, and no plot

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_cit_capita", treatment = "Post")
  
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
  cat(sprintf('t-statistic: %1.2f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
  print(summary(current_tau_hat))
  
  # Compute summary statistics for real_cit_capita
  mean_real_cit_capita <- mean(current_df$real_cit_capita, na.rm = TRUE)
  median_real_cit_capita <- median(current_df$real_cit_capita, na.rm = TRUE)
  IQR_real_cit_capita <- IQR(current_df$real_cit_capita, na.rm = TRUE)
  min_real_cit_capita <- min(current_df$real_cit_capita, na.rm = TRUE)
  max_real_cit_capita <- max(current_df$real_cit_capita, na.rm = TRUE)
  
  # Print the summary statistics for real_cit_capita
  cat(sprintf('Summary statistics for real_cit_capita in %s:\n', state_name))
  cat(sprintf('Mean: %1.2f\n', mean_real_cit_capita))
  cat(sprintf('Median: %1.2f\n', median_real_cit_capita))
  cat(sprintf('IQR: %1.2f\n', IQR_real_cit_capita))
  cat(sprintf('Min: %1.2f\n', min_real_cit_capita))
  cat(sprintf('Max: %1.2f\n', max_real_cit_capita))
  
  # Store the results in point_estimate_list
  point_estimate_list[[state_name]] <- list(
    point_estimate = current_tau_hat,
    se = se,
    CI_lower = current_tau_hat - 1.96 * se,
    CI_upper = current_tau_hat + 1.96 * se,
    t_statistic = t_statistic,
    p_value = p_value,
    summary = summary(current_tau_hat),
    mean_real_cit_capita = mean_real_cit_capita,
    median_real_cit_capita = median_real_cit_capita,
    IQR_real_cit_capita = IQR_real_cit_capita,
    min_real_cit_capita = min_real_cit_capita,
    max_real_cit_capita = max_real_cit_capita
  )
}




#remove plot for speed of loop



#Step 3- Syn DiD vs. DiD vs Synthetic Control Method Estimates and Plots





#Step 4- Plot Loop, able to get the loop plotted and saved as part of point estimates
plot(Iowa_tau.hat)+labs(x="Year",y="Real CIT Revenue per Capita") + ggtitle("Synthetic DiD Plot- Iowa = Treated") +theme_fivethirtyeight()


for (state_name in names(point_estimate_list)) {
  # Get the corresponding sDiD estimate
  tau_hat <- point_estimate_list[[state_name]]
  
  # Plot the sDiD estimate using synthdid_plot
  plot <- synthdid_plot(tau_hat) +
    labs(x = "Year", y = "Real CIT Revenue per Capita") +
    ggtitle(paste("Synthetic DiD Plot -", state_name, "= Treated")) +
    theme_fivethirtyeight()
  
  # Optionally, save each plot as an image
  ggsave(paste0(state_name, "_sDiD_plot.png"), plot = plot)
}

