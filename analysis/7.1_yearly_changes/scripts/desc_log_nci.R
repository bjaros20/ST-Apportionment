#SR Descriptive Log Corporate Income

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

#Load data
naive_ci<-read.csv("naive_ci.csv")

#Create base dataframe that has log_ci as dependent variable.
filt_Corp <-real_log_ci %>%
  select(State_Acronym,year,year_effective,State_Name,real_log_nci,Post)



#attempt to get just the descriptive statistics

# Sort the dataframe by year_effective and select the first 29 states
sorted_df <- filt_Corp %>%
  arrange(year_effective)

# Get the first 29 states and their year_effective
state_years <- sorted_df %>%
  distinct(State_Name, year_effective) %>%
  slice(1:29)

# Create an empty dataframe to store the results
result_df <- data.frame()

# Loop over the first 29 states
for (i in 1:nrow(state_years)) {
  state_name <- state_years$State_Name[i]
  year_eff <- state_years$year_effective[i]
  
  # Filter the dataframe for the current state and the 3 relevant years
  temp_df <- sorted_df %>%
    filter(State_Name == state_name, 
           year %in% c(year_eff - 1, year_eff, year_eff + 1)) %>%
    select(State_Name, year, real_log_nci)
  
  # Append the result to the result_df
  result_df <- rbind(result_df, temp_df)
}

# Display the final result
print(result_df)

result_df_with_effective <- result_df %>%
  left_join(sorted_df %>% select(State_Name, year_effective) %>% distinct(), 
            by = "State_Name")


# Now create wide DF
# Pivot the dataframe to wider format
transposed_df <- result_df_with_effective %>%
  mutate(year_relative = case_when(
    year == year_effective - 1 ~ "year(-1)",
    year == year_effective ~ "year(0)",
    year == year_effective + 1 ~ "year(1)"
  )) %>%
  select(State_Name, year_relative, real_log_nci) %>%
  pivot_wider(names_from = year_relative, values_from = real_log_nci)

# Display the transposed result
print(transposed_df)


#Now will mutate and create difference columns
diff <- transposed_df %>%
  mutate(diff1 = `year(0)`- `year(-1)`) %>%
  mutate(diff2 = `year(1)`- `year(-1)`)

year_diff <- diff %>%
  left_join(sorted_df %>% select(State_Name, year_effective) %>% distinct(), 
          by = "State_Name")

write.csv(year_diff, "sr_descriptive_log_nci.csv",row.names = FALSE)

print(
  year_diff %>%
    mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 4))),
  n = Inf
)
