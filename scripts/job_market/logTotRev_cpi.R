#Simple DiD result with Log total real revenue 6-20-24
#also note to self, after getting this result, will need to consider saving results
#in a notebook, like Micah recommended.

#Load Packages
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(sjlabelled) # req by https://rpubs.com/phle/r_tutorial_difference_in_differences
library(ggrepel) # req by ^
library(scales) # req by ^
library(ggpubr) # req by ^
library(plm)
library(lmtest)
library(lubridate) #convert date to year


# set seed
set.seed(26)

#Make the Log Real Collections per capita
#Get Population data from state_tax_files Directory
setwd("~/Documents/GitHub/state_tax_revenues/fred_excel_files_feb_2024")

population <- read_xls("State_Resident_Population.xls",sheet="Annual")

#convert the Date in the census sheet
population2 <- population %>%
  mutate(year=year(DATE))

# have dataframe panel with year and date

#load revenue dataframe
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Revenue panel
Rev <- read.csv("logTotRev_cpi.csv")

#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X)

# filter population dataframe to make it easier to use
population3 <- population2 %>%
  filter(year>=1976)

#Columns for each state are named "AKPOP" for Alaska population. Want to get just the acronym for each
population4 <- population3 %>%
  rename_with(.fn = ~ ifelse(grepl("^[A-Z]{2}POP$", .), substr(., 1, 2), .), 
              .cols = everything())

# Pivot population4 to long format to make it easier to join
population4_long <- population4 %>%
  pivot_longer(-c(DATE, year), names_to = "State_Acronym", values_to = "population")

# Merge with Rev and create the new "population" column
Rev1 <- Rev %>%
  left_join(population4_long, by = c("State_Acronym" = "State_Acronym", "year" = "year"))

Rev1 <- Rev1 %>%
  select(-DATE)

write.csv(Rev1,"rev_population.csv")

#Get real, log CIT Tax for Bodenhorn
RealCorp <- Rev1 %>%
  mutate(real_cit = (CORPINCTX/CPI_def)*100)

log_realCorp <- RealCorp %>%
  mutate(logRealCitRev=log(real_cit))

#Creates log real per capita, which I don't love.  Will try just real per capita
per_capita <- log_realCorp %>%
  mutate(Tot_logReal_capita=logRealTotRev/population) %>%
  mutate(CIT_logReal_capita=logRealCitRev/population)

real_perCapita <- per_capita %>%
  mutate(real_totRev_capita = real_totRev/population)%>%
  mutate(real_cit_capita = real_cit/population)

#SHOULD BE NOTED, do not need to convert revenue per capita numbers multiplying by 1000
#this is because the population numbers from the census are also in thousands


#plot real per capita
ggplot(real_perCapita2, aes(x=year,y=real_totRev_capita_conv, color=State_Acronym))+
  geom_point()

# PART I -Estimate the simple DiD with the 2007 switchers
#Anyone before needs to be filtered out (a, i)
#anyone who switches within 3 years 2008-2010, needs to be filtered out (a, ii)
#leaves treatment (a,iii) 2007 Switchers and control anyone who switches after 2010 (a, iv)
#(b) https://rpubs.com/phle/r_tutorial_difference_in_differences

#distribution of revenue by year

#Create a dataframe with just 2007 states
treated_states <- Rev %>%
  filter(year_effective==2007)

#create df for control group
control_states <- Rev %>%
  filter(year_effective >=2011 | is.na(year_effective))


#Create dataframe with states in control_states
states_1df <- control_states %>%
  distinct(State_Acronym)

print(states_1df)

states_2df <- treated_states %>%
  distinct(State_Acronym)

print(states_2df)


# find mean of control_States, pre & post
control_pre <-control_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(logRealTotRev, na.rm= TRUE))

control_post <-control_states %>%
  filter(year >2007 & year<2011) %>%
  summarize(avg_outcome = mean(logRealTotRev, na.rm= TRUE))

# calculate averages for treatment group
treatment_pre <- treated_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(logRealTotRev, na.rm = TRUE))

treatment_post <- treated_states %>%
  filter(year >2007 & year<2011) %>%
  summarise(avg_outcome = mean(logRealTotRev, na.rm = TRUE))

# Calculate differences
control_diff <- control_post$avg_outcome - control_pre$avg_outcome
treatment_diff <- treatment_post$avg_outcome - treatment_pre$avg_outcome

# Calculate Difference-in-Differences
did_estimate <- treatment_diff - control_diff

# Print the DiD estimate
did_estimate
#[1] -0.06694635


#Attempt to create plot
# Calculate average Log Total Tax by year for control group along with standard error
average_c <- control_states %>%
  group_by(year) %>%
  filter(!is.na(logRealTotRev)) %>%
  summarise(average_TOTLTX_c = mean(logRealTotRev),
            std_err_c = sd(logRealTotRev) / sqrt(n()))

# Calculate average logRealTotRev by year for treated states along with standard error
average_t <- treated_states %>%
  group_by(year) %>%
  filter(!is.na(logRealTotRev)) %>%
  summarise(average_TOTLTX_t = mean(logRealTotRev),
            std_err_t = sd(logRealTotRev) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Create the plot, standard bar plot with standard error bars
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Log Real Total Tax Revenue by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Log Real Total Tax Revenue") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()


# Now modify the plot to only have years 2004-2010
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Log Real Total Tax Revenue by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Log Real Total Tax Revenue") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  xlim(2004, 2010) +
  theme_minimal()


#PART II - Estimate the simple DiD with the 2007 switchers for the real revenue per capita.
#Anyone before needs to be filtered out (a, i)
#anyone who switches within 3 years 2008-2010, needs to be filtered out (a, ii)
#leaves treatment (a,iii) 2007 Switchers and control anyone who switches after 2010 (a, iv)
#(b) https://rpubs.com/phle/r_tutorial_difference_in_differences

#Create a dataframe with just 2007 states
treated_states <- real_perCapita %>%
  filter(year_effective==2007)

#create df for control group
control_states <- real_perCapita %>%
  filter(year_effective >=2011 | is.na(year_effective))

#Create dataframe with states in control_states
states_1df <- control_states %>%
  distinct(State_Acronym)

print(states_1df)

states_2df <- treated_states %>%
  distinct(State_Acronym)

print(states_2df)

# find mean of control_States, pre & post
control_pre <-control_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(real_totRev_capita, na.rm= TRUE))

control_post <-control_states %>%
  filter(year >2007 & year<2011) %>%
  summarize(avg_outcome = mean(real_totRev_capita, na.rm= TRUE))

# calculate averages for treatment group
treatment_pre <- treated_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(real_totRev_capita, na.rm = TRUE))

treatment_post <- treated_states %>%
  filter(year >2007 & year<2011) %>%
  summarise(avg_outcome = mean(real_totRev_capita, na.rm = TRUE))

# Calculate differences
control_diff <- control_post$avg_outcome - control_pre$avg_outcome
treatment_diff <- treatment_post$avg_outcome - treatment_pre$avg_outcome

# Calculate Difference-in-Differences
did_estimate <- treatment_diff - control_diff

# Print the DiD estimate
did_estimate
#[1] -131.2836


#Attempt to create plot
# Calculate averagereal_totRev_capita by year for control group along with standard error
average_c <- control_states %>%
  group_by(year) %>%
  filter(!is.na(real_totRev_capita)) %>%
  summarise(average_TOTLTX_c = mean(real_totRev_capita),
            std_err_c = sd(real_totRev_capita) / sqrt(n()))

# Calculate average real_totRev_capita by year for treated states along with standard error
average_t <- treated_states %>%
  group_by(year) %>%
  filter(!is.na(real_totRev_capita)) %>%
  summarise(average_TOTLTX_t = mean(real_totRev_capita),
            std_err_t = sd(real_totRev_capita) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Create the plot, standard bar plot with standard error bars
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real Total Tax Revenue per Capita by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Real Total Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()


# Now modify the plot to only have years 2004-2010
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real Total Tax Revenue per Capita by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Real Total Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  xlim(2004, 2010) +
  theme_minimal()


# PART III Corporate Income Per Capita
#Don't need to recreate dfs

# find mean of control_States, pre & post
control_pre <-control_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(real_cit_capita, na.rm= TRUE))

control_post <-control_states %>%
  filter(year >2007 & year<2011) %>%
  summarize(avg_outcome = mean(real_cit_capita, na.rm= TRUE))

# calculate averages for treatment group
treatment_pre <- treated_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(real_cit_capita, na.rm = TRUE))

treatment_post <- treated_states %>%
  filter(year >2007 & year<2011) %>%
  summarise(avg_outcome = mean(real_cit_capita, na.rm = TRUE))

# Calculate differences
control_diff <- control_post$avg_outcome - control_pre$avg_outcome
treatment_diff <- treatment_post$avg_outcome - treatment_pre$avg_outcome

# Calculate Difference-in-Differences
did_estimate <- treatment_diff - control_diff

# Print the DiD estimate, CIT per capita estimate
did_estimate
#[1] -14.29405


#Attempt to create plot
# Calculate real_cit_capita by year for control group along with standard error
average_c <- control_states %>%
  group_by(year) %>%
  filter(!is.na(real_cit_capita)) %>%
  summarise(average_TOTLTX_c = mean(real_cit_capita),
            std_err_c = sd(real_cit_capita) / sqrt(n()))

# Calculate average real_cit_capita by year for treated states along with standard error
average_t <- treated_states %>%
  group_by(year) %>%
  filter(!is.na(real_cit_capita)) %>%
  summarise(average_TOTLTX_t = mean(real_cit_capita),
            std_err_t = sd(real_cit_capita) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Create the plot, standard bar plot with standard error bars
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real Corporate Tax Revenue per Capita by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Real Corporate Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()


# Now modify the plot to only have years 2004-2010
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real CIT Tax Revenue per Capita by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Real CIT  Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  xlim(2004, 2010) +
  theme_minimal()

#Save the dataframe for per_capita
write.csv(real_perCapita,"real_per_capita_df.csv")

#PART IV- Load the df from last session, and run estimation for per capita with detrended data.
#detrend(x,tt,bp)
Rev <- read.csv("real_per_capita_df.csv")

#Detrend the data for all states.  Want to detrend: real_totRev_capita & real_cit_capita
install.packages("pracma")
library(pracma) #necessary for using detrend

Rev2 <- Rev %>%
  group_by(State_Acronym) %>%
  mutate(detrended_RTR_capita = detrend(real_totRev_capita,tt="linear")) %>%
  mutate(detrended_CIT_capita = detrend(real_cit_capita,tt="linear")) %>%
  ungroup()
#above caused an error, using chatGPT to filter out non trend

# Define a safe detrend function that handles empty or single-element groups
safe_detrend <- function(x) {
  if (length(x) > 1) {
    return(detrend(x, tt = "linear"))
  } else {
    return(rep(NA, length(x)))  # Return NA for groups that can't be detrended
  }
}

Rev2 <- Rev %>%
  group_by(State_Acronym) %>%
  mutate(detrended_RTR_capita = safe_detrend(real_totRev_capita),
         detrended_CIT_capita = safe_detrend(real_cit_capita)) %>%
  ungroup()




#Create a dataframe with just 2007 states
treated_states <- Rev2 %>%
  filter(year_effective==2007)

#create df for control group
control_states <- Rev2 %>%
  filter(year_effective >=2011 | is.na(year_effective))

#Create dataframe with states in control_states
states_1df <- control_states %>%
  distinct(State_Acronym)

states_2df <- treated_states %>%
  distinct(State_Acronym)

# find mean of control_States, pre & post
control_pre <-control_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(detrended_RTR_capita, na.rm= TRUE))

control_post <-control_states %>%
  filter(year >2007 & year<2011) %>%
  summarize(avg_outcome = mean(detrended_RTR_capita, na.rm= TRUE))

# calculate averages for treatment group
treatment_pre <- treated_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(detrended_RTR_capita, na.rm = TRUE))

treatment_post <- treated_states %>%
  filter(year >2007 & year<2011) %>%
  summarise(avg_outcome = mean(detrended_RTR_capita, na.rm = TRUE))

# Calculate differences
control_diff <- control_post$avg_outcome - control_pre$avg_outcome
treatment_diff <- treatment_post$avg_outcome - treatment_pre$avg_outcome

# Calculate Difference-in-Differences
did_estimate <- treatment_diff - control_diff

# Print the DiD estimate
did_estimate
#[1] -110.9996


#Attempt to create plot
# Calculate average detrended_RTR_capita by year for control group along with standard error
average_c <- control_states %>%
  group_by(year) %>%
  filter(!is.na(detrended_RTR_capita)) %>%
  summarise(average_TOTLTX_c = mean(detrended_RTR_capita),
            std_err_c = sd(detrended_RTR_capita) / sqrt(n()))

# Calculate average detrended_RTR_capita by year for treated states along with standard error
average_t <- treated_states %>%
  group_by(year) %>%
  filter(!is.na(detrended_RTR_capita)) %>%
  summarise(average_TOTLTX_t = mean(detrended_RTR_capita),
            std_err_t = sd(detrended_RTR_capita) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Create the plot, standard bar plot with standard error bars
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real Total Tax Revenue per Capita by Year - DETRENDED",
       x = "Year",
       y = "Average Real Total Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()


# Now modify the plot to only have years 2004-2010
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real Total Tax Revenue per Capita by Year -DETRENDED",
       x = "Year",
       y = "Average Real Total Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  xlim(2004, 2010) +
  theme_minimal()


#PART V- Detrended CIT, detrended_CIT_capita
#don't need to recreate control and treatment dataframes

# find mean of control_States, pre & post
control_pre <-control_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(detrended_CIT_capita, na.rm= TRUE))

control_post <-control_states %>%
  filter(year >2007 & year<2011) %>%
  summarize(avg_outcome = mean(detrended_CIT_capita, na.rm= TRUE))

# calculate averages for treatment group
treatment_pre <- treated_states %>%
  filter(year <= 2007) %>%
  summarize(avg_outcome = mean(detrended_CIT_capita, na.rm = TRUE))

treatment_post <- treated_states %>%
  filter(year >2007 & year<2011) %>%
  summarise(avg_outcome = mean(detrended_CIT_capita, na.rm = TRUE))

# Calculate differences
control_diff <- control_post$avg_outcome - control_pre$avg_outcome
treatment_diff <- treatment_post$avg_outcome - treatment_pre$avg_outcome

# Calculate Difference-in-Differences
did_estimate <- treatment_diff - control_diff

# Print the DiD estimate
did_estimate
#[1] -17.56599


#Attempt to create plot
# Calculate average detrended_CIT_capita by year for control group along with standard error
average_c <- control_states %>%
  group_by(year) %>%
  filter(!is.na(detrended_CIT_capita)) %>%
  summarise(average_TOTLTX_c = mean(detrended_CIT_capita),
            std_err_c = sd(detrended_CIT_capita) / sqrt(n()))

# Calculate average detrended_CIT_capita by year for treated states along with standard error
average_t <- treated_states %>%
  group_by(year) %>%
  filter(!is.na(detrended_CIT_capita)) %>%
  summarise(average_TOTLTX_t = mean(detrended_CIT_capita),
            std_err_t = sd(detrended_CIT_capita) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Create the plot, standard bar plot with standard error bars
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real CIT Tax Revenue per Capita by Year - DETRENDED",
       x = "Year",
       y = "Average Real CIT Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()


# Now modify the plot to only have years 2004-2010
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_TOTLTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_c, ymin = average_TOTLTX_c - std_err_c, ymax = average_TOTLTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_TOTLTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_TOTLTX_t, ymin = average_TOTLTX_t - std_err_t, ymax = average_TOTLTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Average Real CIT Tax Revenue per Capita by Year -DETRENDED",
       x = "Year",
       y = "Average Real CIT Tax Revenue per Capita") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  xlim(2004, 2010) +
  theme_minimal()


#save detrended dataframe
write.csv(Rev2,"detrend_per_capita.csv")
