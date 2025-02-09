#Descriptive-Average plots
# Plot it for the individual state, log and un log it with vertical line for treatment

#load packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(ggthemes)
library(stringr)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#load data
data <- read.csv("real_log_nci.csv")

# Non-corporate tax Revenue
NC_data <- data %>%
  mutate(NCTR = TOTLTAX - CORPINCTX) %>%
  mutate(real_NCTR = (NCTR / CPI_def) * 100) %>%
  mutate(log_RNCTR = log(real_NCTR))

write.csv(NC_data,"real_log_nci.csv",row.names=FALSE)

# Jonathan wants a plot for the four variables.  Avg for Never, Avg 2007/2006 (2007 treatment), Avg 2015-2018 (2016 treatment)

# Function to create a state acronym index for each group
get_state_index <- function(states) {
  if (length(states) > 10) {
    return(paste(str_trunc(paste(unique(states), collapse = ", "), 50), "...")) # Truncate long lists
  } else {
    return(paste(unique(states), collapse = ", "))
  }
}

# Filter and calculate yearly average for each group, keeping state acronyms
avg_2006_07 <- data %>%
  filter(year_effective %in% c(2006, 2007)) %>%
  group_by(year) %>%
  summarise(
    avg_logRealCitRev = mean(logRealCitRev, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2006-2007")

avg_2015_18 <- data %>%
  filter(year_effective >= 2015 & year_effective <= 2018) %>%
  group_by(year) %>%
  summarise(
    avg_logRealCitRev = mean(logRealCitRev, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2015-2018")

avg_NA <- data %>%
  filter(is.na(year_effective)) %>%
  group_by(year) %>%
  summarise(
    avg_logRealCitRev = mean(logRealCitRev, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "No Year Effective (NA)")

# Combine all groups into one dataframe
avg_logRealCitRev_df <- bind_rows(avg_2006_07, avg_2015_18, avg_NA)

# Print to verify the structure
print(avg_logRealCitRev_df)



# Generate labels with state acronyms
group_labels <- avg_logRealCitRev_df %>%
  group_by(group) %>%
  summarise(state_acronyms = unique(state_acronyms)) %>%
  mutate(label = paste(group, "\nStates: ", state_acronyms)) %>%
  pull(label)

# Create a named vector for scale_color_manual
names(group_labels) <- unique(avg_logRealCitRev_df$group)

logRealCitRev_plot <- ggplot(avg_logRealCitRev_df, aes(x = year, y = avg_logRealCitRev, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Year",
    y = "Ln(Corporate Income Tax Revenue)",
    title = "Trends in Log Real Corporate Income Tax Revenue"
  ) +
  scale_color_manual(values = c("blue", "red", "green"), labels = group_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


logRealCitRev_plot <- logRealCitRev_plot +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Arranges legend horizontally
    plot.margin = margin(10, 10, 50, 10)  # Increases bottom margin
  )

# Display the modified plot
print(logRealCitRev_plot)

ggsave(filename = "logRealCitRev_trends.png", plot = logRealCitRev_plot, width = 7.8, height = 3.75)



#PART II: Now plot log_RNCTR

# Filter and calculate yearly average for each group, keeping state acronyms
avg_2006_07 <- data %>%
  filter(year_effective %in% c(2006, 2007)) %>%
  group_by(year) %>%
  summarise(
    avg_log_RNCTR = mean(log_RNCTR, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2006-2007")

avg_2015_18 <- data %>%
  filter(year_effective >= 2015 & year_effective <= 2018) %>%
  group_by(year) %>%
  summarise(
    avg_log_RNCTR = mean(log_RNCTR, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2015-2018")

avg_NA <- data %>%
  filter(is.na(year_effective)) %>%
  group_by(year) %>%
  summarise(
    avg_log_RNCTR = mean(log_RNCTR, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "No Year Effective (NA)")

# Combine all groups into one dataframe
avg_log_RNCTR_df <- bind_rows(avg_2006_07, avg_2015_18, avg_NA)

# Print to verify the structure
print(avg_log_RNCTR_df)



# Generate labels with state acronyms
group_labels <- avg_log_RNCTR_df %>%
  group_by(group) %>%
  summarise(state_acronyms = unique(state_acronyms)) %>%
  mutate(label = paste(group, "\nStates: ", state_acronyms)) %>%
  pull(label)

# Create a named vector for scale_color_manual
names(group_labels) <- unique(avg_log_RNCTR_df$group)

log_RNCTR_plot <- ggplot(avg_log_RNCTR_df, aes(x = year, y = avg_log_RNCTR, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Year",
    y = "Ln(Non-Corporate Income Tax Revenue)",
    title = "Trends in Log Non-Corporate Income Tax Revenue"
  ) +
  scale_color_manual(values = c("blue", "red", "green"), labels = group_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


log_RNCTR_plot <- log_RNCTR_plot +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Arranges legend horizontally
    plot.margin = margin(10, 10, 50, 10)  # Increases bottom margin
  )

# Display the modified plot
print(log_RNCTR_plot)
ggsave(filename = "log_RNCTR_trends.png", plot = log_RNCTR_plot, width = 7.8, height = 3.75)


#PART III: Now plot real_log_nci

# Filter and calculate yearly average for each group, keeping state acronyms
avg_2006_07 <- data %>%
  filter(year_effective %in% c(2006, 2007)) %>%
  group_by(year) %>%
  summarise(
    avg_real_log_nci = mean(real_log_nci, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2006-2007")

avg_2015_18 <- data %>%
  filter(year_effective >= 2015 & year_effective <= 2018) %>%
  group_by(year) %>%
  summarise(
    avg_real_log_nci = mean(real_log_nci, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2015-2018")

avg_NA <- data %>%
  filter(is.na(year_effective)) %>%  # Keep only rows where year_effective is NA
  filter(State_Acronym != "OH") %>%  # Remove Ohio
  group_by(year) %>%
  filter(!is.na(real_log_nci) & real_log_nci != 0) %>%  # Drop NA and zero values
  summarise(
    avg_real_log_nci = mean(real_log_nci, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "No Year Effective (NA)")



# Combine all groups into one dataframe
avg_real_log_nci_df <- bind_rows(avg_2006_07, avg_2015_18, avg_NA)

# Print to verify the structure
print(avg_real_log_nci_df)



# Generate labels with state acronyms
group_labels <- avg_real_log_nci_df %>%
  group_by(group) %>%
  summarise(state_acronyms = unique(state_acronyms)) %>%
  mutate(label = paste(group, "\nStates: ", state_acronyms)) %>%
  pull(label)

# Create a named vector for scale_color_manual
names(group_labels) <- unique(avg_real_log_nci_df$group)

real_log_nci_plot <- ggplot(avg_real_log_nci_df, aes(x = year, y = avg_real_log_nci, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Year",
    y = "Ln(Corporate Income)",
    title = "Trends in Ln(Proxy Corporate Income)"
  ) +
  scale_color_manual(values = c("blue", "red", "green"), labels = group_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


real_log_nci_plot <- real_log_nci_plot +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Arranges legend horizontally
    plot.margin = margin(10, 10, 50, 10)  # Increases bottom margin
  )

# Display the modified plot
print(real_log_nci_plot)
ggsave(filename = "real_log_nci_trends.png", plot = real_log_nci_plot, width = 7.8, height = 3.75)




###Part IV RATES
# Filter and calculate yearly average for each group, keeping state acronyms
avg_2006_07 <- data %>%
  filter(year_effective %in% c(2006, 2007)) %>%
  group_by(year) %>%
  summarise(
    avg_rates = mean(rates, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2006-2007")

avg_2015_18 <- data %>%
  filter(year_effective >= 2015 & year_effective <= 2018) %>%
  group_by(year) %>%
  summarise(
    avg_rates = mean(rates, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "2015-2018")

avg_NA <- data %>%
  filter(is.na(year_effective)) %>%  # Keep only rows where year_effective is NA
  filter(State_Acronym != "OH") %>%  # Remove Ohio
  group_by(year) %>%
  filter(!is.na(rates) & rates != 0) %>%  # Drop NA and zero values
  summarise(
    avg_rates = mean(rates, na.rm = TRUE),
    state_acronyms = get_state_index(State_Acronym)
  ) %>%
  mutate(group = "No Year Effective (NA)")



# Combine all groups into one dataframe
avg_rates_df <- bind_rows(avg_2006_07, avg_2015_18, avg_NA)

# Print to verify the structure
print(avg_rates_df)



# Generate labels with state acronyms
group_labels <- avg_rates_df %>%
  group_by(group) %>%
  summarise(state_acronyms = unique(state_acronyms)) %>%
  mutate(label = paste(group, "\nStates: ", state_acronyms)) %>%
  pull(label)

# Create a named vector for scale_color_manual
names(group_labels) <- unique(avg_rates_df$group)

rates_plot <- ggplot(avg_rates_df, aes(x = year, y = avg_rates, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = 2016, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Year",
    y = "Avg. Corporate Income Tax Rate",
    title = "Trends in Corporate Income Tax Rates"
  ) +
  scale_color_manual(values = c("blue", "red", "green"), labels = group_labels) +
  theme_fivethirtyeight() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )


rates_plot <- rates_plot +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Arranges legend horizontally
    plot.margin = margin(10, 10, 50, 10)  # Increases bottom margin
  )

# Display the modified plot
print(rates_plot)
ggsave(filename = "rates_trends.png", plot = rates_plot, width = 7.8, height = 3.75)
