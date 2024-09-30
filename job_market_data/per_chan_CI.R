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
#pull treated df
Iowa_syn <- result_list_controls[["Iowa_control"]]

#Create synthetic values for the treated df
Iowa_res <- Iowa_syn %>%
  mutate(weighted_ci = real_ci_cap * Weight)

just_Iowa <- Iowa_res %>%
  filter(State_Name == "Iowa")

#create the synthetic Iowa
Iowa_sum <- Iowa_res %>%
  filter(State_Acronym != "Iowa") %>%
  group_by(year)%>%
  summarize(sum_weight = sum(Weight, na.rm = TRUE),
            syn_Iowa = sum(weighted_ci, na.rm = TRUE))


#Sum weights, note the synthetic control weights do not sum to 1.  NOTE IN DATA OR EMP APPROACH SECTION
sum_weight <- weights_df %>%
  summarize(Total = sum(Weight, na.rm = TRUE))

syn_Iowa <- Iowa_sum %>%
  left_join(just_Iowa, by = c("year"))%>%
  select(-Weight, -weighted_ci)


#PART 4- create the base year 1976 column, estimate the shift, and create the values
#filter to minimize what's in df ? , could do it.

#Create base year 1976 plot
Iowa_base76 <- syn_Iowa %>%
  group_by(State_Acronym) %>%
  mutate(
    real_ci_cap_76 = (real_ci_cap / real_ci_cap[year == 1976]) * 100,
    syn_Iowa_76 = (syn_Iowa / syn_Iowa[year == 1976]) * 100
  ) %>%
  ungroup()


#Create the shift



#PART 5- PLOTS- 3 types, i. Raw Synthetic v Actual, ii. Base year 1976 v Actual, 
#iii. base year 1976, residual minimization v actual

year_effective_first <- syn_Iowa$year_effective[1]

#i Raw synthetic v Actual
ggplot(syn_Iowa, aes(x = year)) +
  geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_ci_cap, color = "Actual"), size = 1.2) +
  geom_line(aes(y = syn_Iowa, color = "Synthetic"), linetype = "dotdash", size = 1.2) +
  labs(title = "Actual vs. Synthetic Naive CI per Capita- Iowa",
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
  labs(title = "Actual vs. Synthetic Naive CI per Capita Iowa- Base Year 1976",
       x = "Year",
       y = "Real CI per Capita Normalized to 1976") +
  scale_color_manual(values = c("Actual" = "red", "Synthetic" = "blue"),
                     labels = c("Actual" = "Real Iowa", "Synthetic" = "Synthetic Iowa")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

#iii) Shift
