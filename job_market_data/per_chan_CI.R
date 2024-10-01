#percent change for corporate income
#use the the weights created from using the never treated.  

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
controls <-read.csv("sDiD_point_estimate_list_CI_long_run.csv")

#controls filter for ease of loop
controls <- controls %>%
  select(State,Effective_Year,Controls)

#Will need to recreate what occurred in the Illinois Example
filt_Corp <-naive_ci %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci_cap,Post)
 

#PART 1, create the Dataframes that just consist of the treated states, never and not yet treated states.
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
  if(df$year_effective[counter] >= 2021) {break}
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
  result_list[[treatment_state_name]] <- df
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2021) {break}
  #increment counter
  counter <-counter + 1
  #empty dataframe break
  if (nrow(df) == 0) {break}
}

#PART 2 Extract CONTROLS DFs

result_list_controls <- list()
# Loop over each dataframe in result_list_long_run
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  #Make the tibble from the result list a dataframe, easier for panel.matrics function
  current_df <- as.data.frame(current_df)
  #state and effective year
  current_state <- state_name
  current_year <- current_df$year_effective[1]
  
  current_weights <- controls[controls$State == current_state, "Controls"]
  
  
  # Split the weights string into individual state-weight pairs
  weights_list <- strsplit(as.character(current_weights), ";")[[1]]
  # Initialize an empty dataframe for storing state acronyms and weights
  weights_df <- data.frame(State = character(), Weight = numeric(), stringsAsFactors = FALSE)
  # Loop through each state-weight pair and extract the state and weight
  for (weight_pair in weights_list) {
    # Trim any leading/trailing spaces before extracting the state acronym
    cleaned_pair <- trimws(weight_pair)
    
    # Now split each pair into the state acronym (first two characters) and the weight (numeric)
    state_acronym <- substr(cleaned_pair, 1, 2)  # First two characters after trimming
    weight_value <- as.numeric(trimws(substring(cleaned_pair, 4)))  # Numeric value starting after the space
    
    # Append the state and weight to the weights_df dataframe
    weights_df <- rbind(weights_df, data.frame(State = state_acronym, Weight = weight_value, stringsAsFactors = FALSE))
  }
  
  #lost treatment state... need to modify this step
  #filter original df
  filtered_df <- current_df[current_df$State_Acronym %in% weights_df$State, ]
  #filter for treatment state separately
  treatment_df <- current_df[current_df$State_Name == current_state, ]
  #combine
  filtered_df <- rbind(filtered_df, treatment_df)
  
  # left join to get unit weights
  filtered_df <- left_join(filtered_df, weights_df, by = c("State_Acronym" = "State"))
  
  # If the treatment state is included, set its weight to 1 (or any other appropriate value)
  filtered_df$Weight[filtered_df$State_Acronym == current_state] <- 1
  
  # Save the filtered dataframe as "state_name_control" and add it to the result_list_controls
  control_name <- paste0(state_name, "_control")
  
  control_name <- paste0(state_name, "_control")
  result_list_controls[[control_name]] <- filtered_df # Store it in the list
}



# PART 3 LOOP JUST CREATES THE SYNTHETIC STATE DF AND SAVES IT.
result_list_syn_state <- list()

# Create an empty dataframe to store the state names and first sum_weight values
sum_weight_df <- data.frame(State_Name = character(), First_Sum_Weight = numeric(), stringsAsFactors = FALSE)


# Loop over each dataframe in result_list_long_run
for (state_name in names(result_list_controls)) {
  # Remove "_control" to get the actual state name
  treatment_state_name <- sub("_control", "", state_name)
#pull treated df
  treated_df <- result_list_controls[[state_name]]
#Create synthetic values for the treated df
  synthetic_df <- treated_df %>%
    mutate(weighted_ci = real_ci_cap * Weight)
  #get just treatment state
  actual_state_data <- synthetic_df %>%
    filter(State_Name == treatment_state_name)
#create the synthetic treated state
  synthetic_sum <- synthetic_df %>%
    filter(State_Acronym != state_name) %>%
    group_by(year) %>%
    summarize(sum_weight = sum(Weight, na.rm = TRUE),
              syn_state = sum(weighted_ci, na.rm = TRUE))
  #save sum weight
  first_sum_weight <- synthetic_sum$sum_weight[1]
  # Append the state name and the first sum_weight value to sum_weight_df
  sum_weight_df <- rbind(sum_weight_df, data.frame(State_Name = state_name, First_Sum_Weight = first_sum_weight))
  # Combine the synthetic state and actual state data
  synthetic_combined <- synthetic_sum %>%
    left_join(actual_state_data, by = c("year")) %>%
    select(-Weight, -weighted_ci)
  # Save the result for the current state in the result list
  result_list_syn_state[[paste0(treatment_state_name, "_synthetic")]] <- synthetic_combined
  
  # Save each dataframe as a CSV file with the desired name format
  csv_filename <- paste0("synthetic_", treatment_state_name, ".csv")
  write.csv(synthetic_combined, csv_filename, row.names = FALSE)
}

#save the sum_weight_df
write.csv(sum_weight_df,"sDiD_weights_SUMMED_by_state.csv", row.names = FALSE)


#notes from the Iowa run
#Sum weights, note the synthetic control weights do not sum to 1.  NOTE IN DATA OR EMP APPROACH SECTION
#sum_weight <- weights_df %>%
# summarize(Total = sum(Weight, na.rm = TRUE))


#PART 4- create the base year 1976 column, estimate the shift, and create the values
#filter to minimize what's in df ? , could do it.
# Initialize an empty list to store the results
results_percentage <- list()
# Initialize a dataframe to store the optimal shift values for each treatment state
optimal_shifts_df <- data.frame(State_Name = character(), Optimal_Shift = numeric(), stringsAsFactors = FALSE)

# Loop through each synthetic state in result_list_syn_state
for (state_name in names(result_list_syn_state)) {
  # Pull the treated dataframe for the current state
  syn_state <- result_list_syn_state[[state_name]]
  # Create base year 1976 plot
  base76_state <- syn_state %>%
    group_by(State_Acronym) %>%
    mutate(
      real_ci_cap_76 = (real_ci_cap / real_ci_cap[year == 1976]) * 100,
      syn_state_76 = (syn_state / syn_state[year == 1976]) * 100  # Using syn_state now
    ) %>%
    ungroup()
  # Create the shift
  diff_base76 <- base76_state %>%
    group_by(State_Acronym) %>%
    mutate(diff = abs(real_ci_cap_76 - syn_state_76)) %>%
    ungroup()
  # Objective function for optimization
  objective_function <- function(shift) {
    # Filter the data where Post == 0
    filtered_data <- base76_state[base76_state$Post == 0, ]
    # Calculate the absolute difference with the shift applied
    difference <- abs(filtered_data$real_ci_cap_76 - (filtered_data$syn_state_76 + shift))
    # Calculate the average difference
    average_difference <- mean(difference)
    return(average_difference)
  }
  # Use optim to find the shift that minimizes the average absolute difference
  initial_guess <- 0  # Start with an initial guess for shift
  result <- optim(par = initial_guess, fn = objective_function, method = "Brent", lower = -100, upper = 100)
  # Get the optimal shift value
  optimal_shift <- result$par
  # Save the optimal shift in the dataframe
  optimal_shifts_df <- rbind(optimal_shifts_df, data.frame(State_Name = gsub("_synthetic", "", state_name), Optimal_Shift = optimal_shift))
  # Mutate the dataframe to apply the shift
  shift_base_76 <- base76_state %>%
    mutate(syn_state_76_shift = syn_state_76 + optimal_shift)
  # Save the dataframe as CSV with the appropriate name
  csv_filename <- paste0(gsub("_synthetic", "", state_name), "_base76_shift.csv")
  write.csv(shift_base_76, csv_filename, row.names = FALSE)
  
  # Save the shifted dataframe in the results list for further use
  results_percentage[[paste0(state_name, "_shift")]] <- shift_base_76
}

write.csv(optimal_shifts_df,"optimal_shift_by_state.csv", row.names = FALSE)
# After the loop optimal_shifts_df will contain the optimal shift values for each state



#PART 5- PLOTS- 3 types, i. Raw Synthetic v Actual, ii. Base year 1976 v Actual, 
#iii. base year 1976, residual minimization v actual

year_effective_first <- syn_Iowa$year_effective[1]

#i Raw synthetic v Actual
ggplot(syn_Iowa, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Iowa, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = "Actual vs. Synthetic CI per Capita- Iowa",
       x = "Year",
       y = "Real CI per Capita ($)") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real Iowa", "Synthetic" = "Synthetic Iowa")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

# ii Base Year 1976 synthetic v actual
ggplot(Iowa_base76, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap_76, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Iowa_76, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = "Actual vs. Syn CI per Capita Iowa- Base Year 1976",
       x = "Year",
       y = "Real CI per Capita Normalized to 1976") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real Iowa", "Synthetic" = "Synthetic Iowa")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

#iii) Shift
ggplot(shift_Iowa_base_76, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap_76, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Iowa_76_shift, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = "Actual vs. Syn CI per Capita Iowa- Base 1976 & Shift",
       x = "Year",
       y = "Real CI per Capita Normalized to 1976") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real Iowa", "Synthetic" = "Synthetic Iowa")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()


#plots loop

for (state_name in names(results_percentage)) {

# Get the relevant dataframes for the state
syn_state <- results_percentage[[state_name]]  # Adjust this based on the correct data structure

#remove synthetic state
clean_state_name <- sub("_synthetic_shift", "", state_name)

# Extract the optimal shift for the current state from optimal_shifts_df
optimal_shift_value <- optimal_shifts_df %>%
  filter(trimws(State_Name) == trimws(clean_state_name)) %>%  # trimws to remove any spaces
  pull(Optimal_Shift)

# Assuming you have data like "shift_state_base_76" and "year_effective_first" available in each state's data:
year_effective_first <- syn_state$year_effective[1]  # Use the correct column here
# Raw synthetic vs Actual plot
plot1 <- ggplot(syn_state, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_state, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = paste("Actual vs. Synthetic CI per Capita -", clean_state_name),
       x = "Year",
       y = "Real CI per Capita ($)") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = paste("Real", clean_state_name), "Synthetic" = paste("Synthetic", clean_state_name))) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

# Save plot 1
ggsave(filename = paste0(clean_state_name, "_synthetic_vs_actual_CI.png"), plot = plot1)

# Base Year 1976 synthetic vs Actual plot
plot2 <- ggplot(syn_state, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap_76, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_state_76, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = paste("Actual vs. Syn CI per Capita", clean_state_name, "- Base Year 1976"),
       x = "Year",
       y = "Real CI per Capita Normalized to 1976") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = paste("Real", clean_state_name), "Synthetic" = paste("Synthetic", clean_state_name))) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

# Save plot 2
ggsave(filename = paste0(clean_state_name, "_base_1976_synthetic_vs_actual_CI.png"), plot = plot2)

# Shifted synthetic vs Actual plot and optimal shift in legend
plot3 <- ggplot(syn_state, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap_76, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_state_76_shift, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = paste("Actual vs. Syn CI per Capita", clean_state_name, "- Base 1976 & Shift"),
       x = "Year",
       y = "Real CI per Capita Normalized to 1976") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = paste("Real", clean_state_name), 
                                "Synthetic" = paste("Synthetic", clean_state_name, "\nOptimal Shift =", round(optimal_shift_value, 4)))) +  # Add shift value to legend
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()


# Save plot 3
ggsave(filename = paste0(state_name, "_shift76_synthetic_vs_actual_CI.png"), plot = plot3)
}


# Part 6 Calculate the Percentage Change
# Initialize a list to store the percentage change dataframes for each state
percent_change_revenue <- list()

# Loop through each state's shifted dataframe in results_percentage
for (state_name in names(results_percentage)) {
  
  # Get the shifted dataframe for the current state
  shift_state <- results_percentage[[state_name]]
  
  # Pull the year_effective[1] for the current state
  year_effective <- shift_state$year_effective[1] 
  
  # Define the short-run years (year_effective[1], year_effective[1] + 1, year_effective[1] + 2)
  short_run_years <- c(year_effective, year_effective + 1, year_effective + 2)
  
  # Calculate the percentage change for the short run (and multiply by 100)
  SR_perc <- shift_state %>%
    filter(year %in% short_run_years) %>%
    mutate(perc_chan = (real_ci_cap_76 - syn_state_76_shift) / ((real_ci_cap_76 + syn_state_76_shift) / 2) * 100) %>%
    summarize(avg_perc_chan_SR = mean(perc_chan, na.rm = TRUE))
  
  # Define the long-run period (you can adjust this as needed)
  # Assuming the long-run years are all available years after the short run
  LR_perc <- shift_state %>%
    filter(year > (year_effective + 2)) %>%
    mutate(perc_chan = (real_ci_cap_76 - syn_state_76_shift) / ((real_ci_cap_76 + syn_state_76_shift) / 2) * 100) %>%
    summarize(avg_perc_chan_LR = mean(perc_chan, na.rm = TRUE))
  
  # Save both SR_perc and LR_perc in the percent_change_revenue list
  percent_change_revenue[[paste0(state_name, "_SR_perc")]] <- SR_perc
  percent_change_revenue[[paste0(state_name, "_LR_perc")]] <- LR_perc
  
  # Print the state_name, year_effective, short-run percentage change, and long-run percentage change
  print(paste("State:", state_name))
  print(paste("Year Effective:", year_effective))
  print(paste("Short-Run Percentage Change in Revenue:", formatC(SR_perc$avg_perc_chan_SR, format = "f", digits = 4)))
  print(paste("Long-Run Percentage Change in Revenue:", formatC(LR_perc$avg_perc_chan_LR, format = "f", digits = 4)))
}

write.csv(percent_change_revenue,"percent_change_CI_cap.csv",row.names = FALSE)




