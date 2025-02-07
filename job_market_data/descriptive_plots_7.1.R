# 7.1 Descriptive Plots
#Trying to establish Correlations
# Examine via plots four main outcomes: 
# a. Corporate Tax Revenue
# b. Non-corporate tax revenue
# c. tax rates
# d. Proxy Corporate Income.

# Plot it for the individual state, log and un log it with vertical line for treatment

#load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#load data
data <- read.csv("real_log_nci.csv")

#want to create this plot for each state, so will create a loop
#Create base dataframe that has real_cit
filt_data <-data %>%
  select(State_Acronym,year,year_effective,State_Name,real_cit,logRealCitRev,Post)

#Create State Dataframes
#create result list to store dataframe
result_list <- list()
# Make a copy of the original dataframe to work with
original_df <- filt_data
#counter variable, so as loop progresses, drops first state
counter <- 1

#Just trying to create a state specific dataframe for each state
while (TRUE) {
  #Reset to original each start
  df <-filt_data
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2020) {break}
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state %>% pull(State_Name)
  
  #Just keep the treatment state
  df <- df %>%
    filter(State_Name == treatment_state_name)
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2020) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}


# Example state dataframe (replace with the correct one from result_list)
state_df <- result_list[["Oregon"]]  # Adjust for actual list

# Extract treatment year for the state
treatment_year <- unique(state_df$year_effective)  # Should be a single value

# Generate the plot
plot <- ggplot(state_df, aes(x = year, y = real_cit)) +
  geom_line(color = "blue", linewidth = 1) +  # Line plot
  geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +  # Vertical dashed line
  labs(
    x = "Year",
    y = "Corporate Income Tax Revenue",
    title = paste(state_df$State_Name[1], "Corp Income Tax Revenue")
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )

# Display plot
print(plot)

# Generate the plot
logRealCitRev_plot <- ggplot(state_df, aes(x = year, y = logRealCitRev)) +
  geom_line(color = "blue", linewidth = 1) +  # Line plot
  geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +  # Vertical dashed line
  labs(
    x = "Year",
    y = "Ln(Corporate Income Tax Revenue)",
    title = paste(state_df$State_Name[1], "Ln(Corp Income Tax Revenue)")
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )

plot(logRealCitRev_plot)


# Now just 3 years pre and post
# Extract treatment year for the state
treatment_year <- unique(state_df$year_effective)

# Filter data to keep only (treatment_year - 3) to (treatment_year + 3)
state_df_filtered <- state_df %>%
  filter(year >= (treatment_year - 3) & year <= (treatment_year + 3))

# Generate the filtered plot
plot <- ggplot(state_df_filtered, aes(x = year, y = real_cit)) +
  geom_line(color = "blue", linewidth = 1) +  # Line plot
  geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +  # Vertical dashed line
  labs(
    x = "Year",
    y = "Corporate Income Tax Revenue",
    title = paste(state_df$State_Name[1], "Corp Income Tax Rev: ±3 Years from Treatment")
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )

# Display plot
print(plot)


#plot Log 3 year pre and post
# Generate the filtered plot
log_plot3 <- ggplot(state_df_filtered, aes(x = year, y = logRealCitRev)) +
  geom_line(color = "blue", linewidth = 1) +  # Line plot
  geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +  # Vertical dashed line
  labs(
    x = "Year",
    y = "Ln(Corporate Income Tax Revenue)",
    title = paste(state_df$State_Name[1], "Ln(Corp Income Tax Rev): ±3 Years from Treatment")
  ) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.line = element_line(linewidth = 0.5, colour = "black")
  )
  
plot(log_plot3)



#__________
# Loop for those 4 plots for each state.

# Create an empty list to store plots
plot_list <- list()

for (state_name in names(result_list)) {
  # Access the dataframe for the current state
  current_df <- result_list[[state_name]]
  
  # Extract treatment year (assumes one per state)
  treatment_year <- unique(current_df$year_effective)
  
  # Add log-transformed Corporate Income Tax Revenue
  current_df <- current_df %>% mutate(logRealCitRev = log(real_cit))
  
  # Filter data for ±3 years from treatment year
  current_df_filtered <- current_df %>%
    filter(year >= (treatment_year - 3) & year <= (treatment_year + 3))
  
  ## ------ PLOT 1: Full Corporate Income Tax Revenue over time ------
  CIT_full <- ggplot(current_df, aes(x = year, y = real_cit)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Corporate Income Tax Revenue",
      title = paste(state_name, "Corp Income Tax Revenue")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 2: Full Log Corporate Income Tax Revenue over time ------
  LogCIT_full <- ggplot(current_df, aes(x = year, y = logRealCitRev)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Corporate Income Tax Revenue)",
      title = paste(state_name, "Ln(Corp Income Tax Revenue)")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 3: ±3 Years from Treatment - Corporate Income Tax Revenue ------
  CIT_3yr <- ggplot(current_df_filtered, aes(x = year, y = real_cit)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Corporate Income Tax Revenue",
      title = paste(state_name, "Corp Income Tax Rev: ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 4: ±3 Years from Treatment - Log Corporate Income Tax Revenue ------
  LogCIT_3yr <- ggplot(current_df_filtered, aes(x = year, y = logRealCitRev)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Corporate Income Tax Revenue)",
      title = paste(state_name, "Ln(Corp Income Tax Rev): ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Store plots in a named list under the state key
  plot_list[[state_name]] <- list(
    "CIT_full" = CIT_full,
    "LogCIT_full" = LogCIT_full,
    "CIT_3yr" = CIT_3yr,
    "LogCIT_3yr" = LogCIT_3yr
  )
  
  ggsave(filename = paste0(state_name, "_real_CIT_full.png"), plot = CIT_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_real_LogCIT_full.png"), plot = LogCIT_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_real_CIT_3yr.png"), plot = CIT_3yr, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_real_LogCIT_3yr.png"), plot = LogCIT_3yr, width = 6.5, height = 3.75)
  
}

# Example: Display plots for a specific state (e.g., "Oregon")
print(plot_list[["Oregon"]][["CIT_full"]])  # Full CIT plot for Oregon
print(plot_list[["Oregon"]][["LogCIT_full"]])  # Full Log CIT plot for Oregon
print(plot_list[["Oregon"]][["CIT_3yr"]])  # ±3 Year CIT plot for Oregon
print(plot_list[["Oregon"]][["LogCIT_3yr"]])  # ±3 Year Log CIT plot for Oregon



# Non-corporate tax Revenue
NC_data <- data %>%
  mutate(NCTR = TOTLTAX - CORPINCTX) %>%
  mutate(real_NCTR = (NCTR / CPI_def) * 100) %>%
  mutate(log_RNCTR = log(real_NCTR))


# Filter data
#want to create this plot for each state, so will create a loop
#Create base dataframe that has real_cit
filt_data <-NC_data %>%
  select(State_Acronym,year,year_effective,State_Name,real_NCTR,log_RNCTR,Post)

#Create State Dataframes
#create result list to store dataframe
result_list <- list()
# Make a copy of the original dataframe to work with
original_df <- filt_data
#counter variable, so as loop progresses, drops first state
counter <- 1

#Just trying to create a state specific dataframe for each state
while (TRUE) {
  #Reset to original each start
  df <-filt_data
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2020) {break}
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state %>% pull(State_Name)
  
  #Just keep the treatment state
  df <- df %>%
    filter(State_Name == treatment_state_name)
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2020) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}


### Loop to produce plots for Non corporate Revenue


# Create an empty list to store plots for Real Non-Corporate Tax Revenue
real_NCTR_plots <- list()

for (state_name in names(result_list)) {
  # Access the dataframe for the current state
  current_df <- result_list[[state_name]]
  
  # Extract treatment year (assumes one per state)
  treatment_year <- unique(current_df$year_effective)
  
  # Ensure log transformation does not include negative/zero values
  current_df <- current_df %>%
    mutate(log_RNCTR = ifelse(real_NCTR > 0, log(real_NCTR), NA))
  
  # Filter data for ±3 years from treatment year
  current_df_filtered <- current_df %>%
    filter(year >= (treatment_year - 3) & year <= (treatment_year + 3))
  
  ## ------ PLOT 1: Full Real Non-Corporate Tax Revenue over time ------
  real_NCTR_full <- ggplot(current_df, aes(x = year, y = real_NCTR)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Real Non-Corp Tax Revenue",
      title = paste(state_name, "Real Non-Corp Tax Revenue")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 2: Full Log Real Non-Corporate Tax Revenue over time ------
  log_real_NCTR_full <- ggplot(current_df, aes(x = year, y = log_RNCTR)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Real Non-Corp Tax Revenue)",
      title = paste(state_name, "Ln(Real Non-Corp Tax Revenue)")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 3: ±3 Years from Treatment - Real Non-Corporate Tax Revenue ------
  real_NCTR_3yr <- ggplot(current_df_filtered, aes(x = year, y = real_NCTR)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Real Non-Corp Tax Revenue",
      title = paste(state_name, "Real Non-Corp Tax Rev: ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 4: ±3 Years from Treatment - Log Real Non-Corporate Tax Revenue ------
  log_real_NCTR_3yr <- ggplot(current_df_filtered, aes(x = year, y = log_RNCTR)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Real Non-Corp Tax Revenue)",
      title = paste(state_name, "Ln(Real Non-Corp Tax Rev): ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Store plots in a named list under the state key
  real_NCTR_plots[[state_name]] <- list(
    "RealNCTR_full" = real_NCTR_full,
    "LogRealNCTR_full" = log_real_NCTR_full,
    "RealNCTR_3yr" = real_NCTR_3yr,
    "LogRealNCTR_3yr" = log_real_NCTR_3yr
  )

  ggsave(filename = paste0(state_name, "_RealNCTR_full.png"), plot = real_NCTR_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_LogRealNCTR_full.png"), plot = log_real_NCTR_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_RealNCTR_3yr.png"), plot = real_NCTR_3yr, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_LogRealNCTR_3yr.png"), plot = log_real_NCTR_3yr, width = 6.5, height = 3.75)
  
}


### Part 3 Plot Tax Rates by State- over full time period and pre/post 3 years
#Create base dataframe that has real_cit
filt_data <-data %>%
  select(State_Acronym,year,year_effective,State_Name,rates,Post)

#Create State Dataframes
#create result list to store dataframe
result_list <- list()
# Make a copy of the original dataframe to work with
original_df <- filt_data
#counter variable, so as loop progresses, drops first state
counter <- 1

#Just trying to create a state specific dataframe for each state
while (TRUE) {
  #Reset to original each start
  df <-filt_data
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2020) {break}
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state %>% pull(State_Name)
  
  #Just keep the treatment state
  df <- df %>%
    filter(State_Name == treatment_state_name)
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2020) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}

##Loop for rates only includes over time and pre/post 3 years

# Create an empty list to store Tax Rates plots
tax_rate_plots <- list()

for (state_name in names(result_list)) {
  # Access the dataframe for the current state
  current_df <- result_list[[state_name]]
  
  # Extract treatment year (assumes one per state)
  treatment_year <- unique(current_df$year_effective)
  
  # Filter data for ±3 years from treatment year
  current_df_filtered <- current_df %>%
    filter(year >= (treatment_year - 3) & year <= (treatment_year + 3))
  
  ## ------ PLOT 1: Full Tax Rates over time ------
  TaxRates_full <- ggplot(current_df, aes(x = year, y = rates)) +
    geom_line(color = "red", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Tax Rates",
      title = paste(state_name, "Tax Rates")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 2: ±3 Years from Treatment - Tax Rates ------
  TaxRates_3yr <- ggplot(current_df_filtered, aes(x = year, y = rates)) +
    geom_line(color = "red", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Tax Rates",
      title = paste(state_name, "Tax Rates: ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Store plots in a named list under the state key
  tax_rate_plots[[state_name]] <- list(
    "TaxRates_full" = TaxRates_full,
    "TaxRates_3yr" = TaxRates_3yr
  )
  ggsave(filename = paste0(state_name, "_TaxRates_full.png"), plot = TaxRates_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_TaxRates_3yr.png"), plot = TaxRates_3yr, width = 6.5, height = 3.75)
  
}

# Example: Display plots for a specific state (e.g., "Oregon")
print(tax_rate_plots[["Oregon"]][["TaxRates_full"]])  # Full Tax Rates plot for Oregon
print(tax_rate_plots[["Oregon"]][["TaxRates_3yr"]])  # ±3 Year Tax Rates plot for Oregon


### Part 4 Plot Proxy Corporate Income over full time period, pre/post, and log
# Filter data
#want to create this plot for each state, so will create a loop
#Create base dataframe that has real_cit
filt_data <-data %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci,real_log_nci,Post)

#Create State Dataframes
#create result list to store dataframe
result_list <- list()
# Make a copy of the original dataframe to work with
original_df <- filt_data
#counter variable, so as loop progresses, drops first state
counter <- 1

#Just trying to create a state specific dataframe for each state
while (TRUE) {
  #Reset to original each start
  df <-filt_data
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2020) {break}
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state %>% pull(State_Name)
  
  #Just keep the treatment state
  df <- df %>%
    filter(State_Name == treatment_state_name)
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2020) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}

# Loop for Proxy Corporate Income

# Create an empty list to store plots for Real Naive Corporate Income
real_NCI_plots <- list()

for (state_name in names(result_list)) {
  # Access the dataframe for the current state
  current_df <- result_list[[state_name]]
  
  # Extract treatment year (assumes one per state)
  treatment_year <- unique(current_df$year_effective)
  
  # Ensure log transformation does not include negative/zero values
  current_df <- current_df %>%
    mutate(real_log_nci = ifelse(real_ci > 0, log(real_ci), NA))
  
  # Filter data for ±3 years from treatment year
  current_df_filtered <- current_df %>%
    filter(year >= (treatment_year - 3) & year <= (treatment_year + 3))
  
  ## ------ PLOT 1: Full Real Naive Corporate Income over time ------
  real_NCI_full <- ggplot(current_df, aes(x = year, y = real_ci)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Real Naive Corporate Income",
      title = paste(state_name, "Real Naive Corporate Income")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 2: Full Log Real Naive Corporate Income over time ------
  log_real_NCI_full <- ggplot(current_df, aes(x = year, y = real_log_nci)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Real Naive Corporate Income)",
      title = paste(state_name, "Ln(Real Naive Corporate Income)")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 3: ±3 Years from Treatment - Real Naive Corporate Income ------
  real_NCI_3yr <- ggplot(current_df_filtered, aes(x = year, y = real_ci)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Real Naive Corporate Income",
      title = paste(state_name, "Real Naive Corp Inc: ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  ## ------ PLOT 4: ±3 Years from Treatment - Log Real Naive Corporate Income ------
  log_real_NCI_3yr <- ggplot(current_df_filtered, aes(x = year, y = real_log_nci)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_vline(xintercept = treatment_year, linetype = "dotted", color = "black", linewidth = 1) +
    labs(
      x = "Year",
      y = "Ln(Real Naive Corporate Income)",
      title = paste(state_name, "Ln(Real Naive Corp Inc): ±3 Years from Treatment")
    ) +
    theme_fivethirtyeight() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  
  # Store plots in a named list under the state key
  real_NCI_plots[[state_name]] <- list(
    "RealNCI_full" = real_NCI_full,
    "LogRealNCI_full" = log_real_NCI_full,
    "RealNCI_3yr" = real_NCI_3yr,
    "LogRealNCI_3yr" = log_real_NCI_3yr
  )
  ggsave(filename = paste0(state_name, "_RealNCI_full.png"), plot = real_NCI_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_LogRealNCI_full.png"), plot = log_real_NCI_full, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_RealNCI_3yr.png"), plot = real_NCI_3yr, width = 6.5, height = 3.75)
  ggsave(filename = paste0(state_name, "_LogRealNCI_3yr.png"), plot = log_real_NCI_3yr, width = 6.5, height = 3.75)
  
}

# Example: Display plots for a specific state (e.g., "Oregon")
print(real_NCI_plots[["Oregon"]][["RealNCI_full"]])  # Full Real Naive Corporate Income plot for Oregon
print(real_NCI_plots[["Oregon"]][["LogRealNCI_full"]])  # Full Log Real Naive Corporate Income plot for Oregon
print(real_NCI_plots[["Oregon"]][["RealNCI_3yr"]])  # ±3 Year Real Naive Corporate Income plot for Oregon
print(real_NCI_plots[["Oregon"]][["LogRealNCI_3yr"]])  # ±3 Year Log Real Naive Corporate Income plot for Oregon

