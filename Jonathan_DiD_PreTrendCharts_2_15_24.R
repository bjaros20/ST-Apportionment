#Second Week Cleaning Attempt, want to eliminate TX: 
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Cleaned Set-up
Initial_df <-read.csv("LogRev_Appt_Merge_1976-2021.csv")

#Remove Region and Number
Clean_Set_up <- subset(Initial_df, select = -c(Region, Number,Revenue))

#All States before 2007
Set_Up_1976_2007 <-subset(Clean_Set_up,Year >=1976 & Year <=2007)

#All States before 2000
Set_Up_1976_1999 <-subset(Clean_Set_up,Year >=1976 & Year <=1999)

#Sort Into TreatMent and Control Groups
Just_NE <- subset(Clean_Set_up, State == "Nebraska")

# For years before 2007
Just_NE_2007 <- subset(Clean_Set_up, State == "Nebraska" & Year < 2008)

# For years before 1999
Just_NE_1999 <- subset(Clean_Set_up, State == "Nebraska" & Year < 2000)


#Create Control Group For 1976 through 2007 (Control Group #1)
Control_2007_Df <-  Set_Up_1976_2007 %>%
  group_by(State) %>%
  filter(all(sales == 33.34))

#Create Control Group for 1976 through 1999 (Control Group #2)
Control_1999_Df <-  Set_Up_1976_1999 %>%
  group_by(State) %>%
  filter(all(sales == 33.34))
#added Mississippi, Utah, Vermont, Virginia, etc.

#Lastly, create control group of Just Midwest States that don't switch at all (Control Group #3), 
Control_Midwest_2007_Df <- Set_Up_1976_2007 %>%
  group_by(State) %>%
  filter(all(sales == 33.34)) %>%
  filter(State %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"))
#only is KS and ND

#Create Combined Dataframes for the 3 control groups:
# Combine dataframes
combined_data_C1 <- bind_rows(Just_NE_2007, Control_2007_Df)

combined_data_C2 <- bind_rows(Just_NE_1999, Control_1999_Df)

combined_data_C3 <- bind_rows(Just_NE_2007, Control_Midwest_2007_Df)


# Create a binary treatment variable for Nebraska and phase-in years
combined_data_C1$Treatment <- ifelse(combined_data_C1$State == "Nebraska" & combined_data_C1$Year %in% 1988:1992, 1, 0)

# Create a binary variable for the post-period
combined_data_C1$Post_Period <- ifelse(combined_data_C1$Year > 1986, 1, 0)

# Create a binary variable for the interaction term
combined_data_C1$Interaction_Term <- combined_data_C1$Treatment * combined_data_C1$Post_Period

# Set up the DiD regression
did_model <- lm(Log_Revenue ~ Treatment + Post_Period + Interaction_Term + factor(Year) - 1, data = combined_data_C1)

# View the regression results
summary(did_model)

# Create a variable to distinguish between Treatment and Control
combined_data_C1$group <- ifelse(combined_data_C1$State == "Nebraska", "Treatment", "Control")

# Create DiD plot
did_plot <- ggplot(combined_data_C1, aes(x = Year, y = Log_Revenue, color = group)) +
  geom_line(aes(group = group, linetype = group), alpha = 0.5) +
  labs(title = "Difference-in-Differences Plot",
       x = "Year",
       y = "Log_Revenue") +
  theme_minimal()

# Display the plot
print(did_plot)


















# Create plot for Log_Revenue with a clear distinction between Nebraska and Control
plot_data <- combined_data_C1 %>%
  mutate(group = factor(ifelse(State == "Nebraska", "Nebraska", "Control"))) %>%
  group_by(group, Year) %>%
  summarize(mean_Log_Revenue = mean(Log_Revenue),
            se_Log_Revenue = sd(Log_Revenue) / sqrt(n()),
            upper = mean_Log_Revenue + (-1.96 * se_Log_Revenue),
            lower = mean_Log_Revenue + (1.96 * se_Log_Revenue))

ggplot(plot_data, aes(x = Year, y = mean_Log_Revenue, color = group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  geom_line(aes(group = group), linetype = "dashed", alpha = 0.5) +
  labs(title = "Log_Revenue DiD Plot- NE and Never Treated States",
       x = "Year",
       y = "Mean Log_Revenue") +
  theme_minimal()



