#Attempt to Get DiD_ Nebraska
install.packages("fixest")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("did")
install.packages("tidyverse")
install.packages("lmtest")
library(fixest) 
library(ggplot2)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Cleaned Set-up
Clean_Set_up <-read.csv("NE_DiD_Set_up.csv")

#Remove Region and Number
Clean_Set_up <- subset(Clean_Set_up, select = -c(Region, Number))

#Sort Into TreatMent and Control Groups
Just_NE <- subset(Clean_Set_up, State == "Nebraska")

#Create Control Group
Control_Df <- Clean_Set_up %>%
  group_by(State) %>%
  filter(all(sales == 33.34))

# Add a binary treatment indicator to both dataframes
Just_NE <- Just_NE %>%
  mutate(Treatment = ifelse(State == "Nebraska" & Year >= 1988 & Year <= 1992, 1, 0))

Control_df <- Control_Df %>%
  mutate(Treatment = 0)  # Control group has a Treatment indicator of 0

# Combine dataframes
combined_data <- bind_rows(Just_NE, Control_df)

# Run two-way fixed effects model
# Assuming "Revenue" is the outcome variable
did_model <- feols(Revenue ~ Treatment | State + Year, data = combined_data)

# Display the results
summary(did_model)

# Create DiD plot
did_plot <- ggplot(combined_data, aes(x = Year, y = Revenue, color = factor(Treatment), linetype = factor(State))) +
  geom_line(alpha = 0.5) +
  geom_smooth(aes(group = Treatment), method = "lm", se = FALSE) +
  labs(title = "Difference-in-Differences Plot",
       x = "Year",
       y = "Revenue") +
  theme_minimal()

# Display the plot
print(did_plot)


# Create DiD plot
did_plot2 <- ggplot(combined_data, aes(x = Year, y = Revenue, color = factor(Treatment), linetype = factor(ifelse(State == "NE", "NE", "Control")))) +
  geom_line(alpha = 0.5) +
  geom_smooth(aes(group = Treatment), method = "lm", se = FALSE) +
  labs(title = "Difference-in-Differences Plot",
       x = "Year",
       y = "Revenue") +
  theme_minimal()

# Display the plot
print(did_plot2)

# Create plot for Log_Revenue with separate colors for Nebraska and Control
log_revenue_plot <- ggplot(combined_data, aes(x = Year, y = Log_Revenue, color = factor(ifelse(State == "NE", "NE", "Control")))) +
  geom_line(aes(group = State), linetype = "dashed", alpha = 0.5) +
  geom_smooth(aes(group = Treatment), method = "lm", se = FALSE) +
  labs(title = "Log_Revenue Difference-in-Differences Plot",
       x = "Year",
       y = "Log_Revenue") +
  theme_minimal()

# Display the plot
print(log_revenue_plot)


# Create plot for Log_Revenue with a clear distinction between Nebraska and Control
plot_data <- combined_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

ggplot(plot_data, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  labs(title = "Log_Revenue Difference-in-Differences Plot",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()

# Create plot for Log_Revenue with a clear distinction between Nebraska and Control
plot_data <- combined_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

ggplot(plot_data, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Log_Revenue Difference-in-Differences Plot",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()

# Create plot for Revenue with a clear distinction between Nebraska and Control
plot_data_revenue <- combined_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Revenue = mean(Revenue),
            se_Revenue = sd(Revenue) / sqrt(n()),
            upper = mean_Revenue + (-1.96 * se_Revenue),
            lower = mean_Revenue + (1.96 * se_Revenue))

ggplot(plot_data_revenue, aes(x = Year, y = mean_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Revenue Difference-in-Differences Plot",
       x = "Year",
       y = "Mean Revenue") +
  theme_minimal()


#Create New Midwest Control Group
midwest_states <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")

# Filter the Clean_Set_Up df for Midwest states
midwest_data <- Clean_Set_up %>% 
  filter(State %in% midwest_states)

#Midwest DiD Plots:
# Create plot for Log_Revenue with a clear distinction between Nebraska and Control
plot_data_midwest <- midwest_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

ggplot(plot_data_midwest, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Log_Revenue Difference-in-Differences Plot",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()


#Another Log_Revenue with Sales Factor Weight

# Create plot for "sales" with a clear distinction between Nebraska and Control
plot_data_midwest_sales <- midwest_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_sales = mean(sales),
            se_sales = sd(sales) / sqrt(n()),
            upper = mean_sales + (-1.96 * se_sales),
            lower = mean_sales + (1.96 * se_sales))

ggplot(plot_data_midwest_sales, aes(x = Year, y = mean_sales, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Sales Difference-in-Differences Plot",
       x = "Year",
       y = "Mean Sales") +
  theme_minimal()

# Assuming "midwest_data" is the dataframe for Midwest states
midwest_data <- Clean_Set_Up %>%
  filter(State %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"))

# Create plot for Revenue with a clear distinction between Nebraska and Control
plot_data_midwest_revenue <- midwest_data %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Revenue = mean(Revenue),
            se_Revenue = sd(Revenue) / sqrt(n()),
            upper = mean_Revenue + (-1.96 * se_Revenue),
            lower = mean_Revenue + (1.96 * se_Revenue))

ggplot(plot_data_midwest_revenue, aes(x = Year, y = mean_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Midwest Control Group Revenue DiD Plot",
       x = "Year",
       y = "Mean Revenue") +
  theme_minimal()

library(dplyr)

# Grouping the states into three groups
Three_groups_df <- Clean_Set_up %>%
  group_by(State) %>%
  mutate(group = case_when(
    State == "Nebraska" ~ "Nebraska",
    all(sales == 33.34) ~ "Sales_equal_33.34",
    TRUE ~ "Other_states"
  )) %>%
  ungroup()

# Display the structure of the new dataframe
str(Three_groups_df)

# Assuming "Three_groups_df" is the dataframe for three groups
plot_data_three_groups <- Three_groups_df %>%
  group_by(group, Year) %>%
  summarize(mean_Revenue = mean(Revenue),
            se_Revenue = sd(Revenue) / sqrt(n()),
            upper = mean_Revenue + (-1.96 * se_Revenue),
            lower = mean_Revenue + (1.96 * se_Revenue))

# Create plot for Three-Groups-DiD
ggplot(plot_data_three_groups, aes(x = Year, y = mean_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Three-Groups-DiD",
       x = "Year",
       y = "Mean Revenue") +
  theme_minimal()

# Assuming "Three_groups_df" is the dataframe for three groups
plot_data_three_groups <- Three_groups_df %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

# Create plot for Three-Groups-DiD - Log_Revenue
ggplot(plot_data_three_groups, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Three-Groups-DiD - Log_Revenue",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()

# Assuming "Three_groups_df" is the dataframe for three groups
plot_data_three_groups_filtered <- Three_groups_df %>%
  filter(State != "Texas") %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

# Create plot for Three-Groups-DiD - Log_Revenue (filtered)
ggplot(plot_data_three_groups_filtered, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(1988, 1992), linetype = "dotted", color = "black") +
  labs(title = "Three-Groups-DiD - Log_Revenue (Excluding Texas)",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()


