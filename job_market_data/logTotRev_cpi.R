#Simple DiD result with Log total real revenue 6-20-24
#also note to self, after getting this result, will need to consider saving results
#in a notebook, like Micah recommended.

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
install.packages("sjlabelled")
library(sjlabelled) # req by https://rpubs.com/phle/r_tutorial_difference_in_differences
install.packages("ggrepel")
library(ggrepel) # req by ^
install.packages("scales")
library(scales) # req by ^
install.packages("ggpubr")
library(ggpubr) # req by ^
library(plm)
library(lmtest)



#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

# set seed
set.seed(26)

#Load Revenue panel
Rev <- read.csv("logTotRev_cpi.csv")
#note, row names =FALSE did not work, still have an X
Rev <- Rev %>%
  select(-X)




#Estimate the simple DiD with the 2007 switchers
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

# Attempt to make the standard DiD plot
agg.esNYTLog <- aggte(Log_C_NYT_attgt, type = "dynamic")
summary(agg.esNYTLog)

ggdid_plotNYTLog <- ggdid(agg.esNYTLog) +
  scale_x_continuous(limits = c(-10, 10)) +
  ggtitle("Event Study Plot (Dynamic ATT): Log Corporate Income Tax Revenue, Treatment = Control via NYT") +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5)) +
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  ylab("Log Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

print(ggdid_plotNYTLog)



