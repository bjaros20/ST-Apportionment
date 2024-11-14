# Information for presentation at NTA, all packages loaded from stacked DiD

data <- data.frame(
  State = c("Iowa", "Nebraska", "Michigan", "Illinois", "Oregon", "Georgia", "Wisconsin", "Arizona", 
            "Indiana", "Maine", "Minnesota", "Pennsylvania", "South Carolina", "Colorado", "California", 
            "Utah", "New Jersey", "New York", "Rhode Island", "Connecticut", "Louisiana", "North Carolina", 
            "North Dakota", "Delaware", "Kentucky", "Maryland", "Missouri", "Alabama", "Arkansas"),
  `Year(-1)` = c(13.7310, 13.8293, 18.1627, 17.5258, 15.0442, 16.2897, 16.1089, 16.3625, 16.3236, 
                 14.5600, 16.2077, 16.8691, 15.5964, 16.2108, 18.4513, 15.4087, 17.0194, 18.0420, 
                 14.2395, 15.7927, 14.9694, 17.0966, 15.2282, 15.1121, 15.8495, 16.3124, 15.5912, 
                 16.2669, 15.7918),
  `Year(0)` = c(13.9013, 13.9194, 18.0320, 17.5959, 15.3944, 16.5132, 16.1409, 16.4651, 16.2676, 
                14.5376, 16.3070, 16.9461, 15.6462, 15.7781, 18.5046, 15.4155, 16.8805, 18.0867, 
                14.7394, 15.9562, 14.5785, 17.0988, 14.6874, 14.8556, 16.3406, 16.3431, 16.0250, 
                16.6749, 16.1156),
  `Year(1)` = c(14.0784, 14.0081, 18.1141, 17.6680, 15.5267, 16.6460, 16.2741, 16.2364, 16.1857, 
                14.5412, 16.1780, 16.9036, 15.6484, 15.8665, 18.3144, 15.4587, 17.0485, 17.9796, 
                14.5387, 16.1150, 15.1079, 17.0437, 14.1607, 14.8901, 16.5404, 16.5736, 16.6588, 
                16.9298, 16.4554),
  `Y(0) - Y(-1)` = c(0.1704, 0.0901, -0.1307, 0.0701, 0.3502, 0.2235, 0.0320, 0.1026, -0.0559, 
                     -0.0224, 0.0993, 0.0771, 0.0498, -0.4327, 0.0533, 0.0068, -0.1388, 0.0447, 
                     0.4999, 0.1635, -0.3909, 0.0022, -0.5408, -0.2565, 0.4911, 0.0307, 0.4339, 
                     0.4080, 0.3238),
  `Y(1) - Y(-1)` = c(0.3475, 0.1788, -0.0486, 0.1422, 0.4825, 0.3563, 0.1652, -0.1262, -0.1378, -0.0188, 
                     -0.0297, 0.0346, 0.0520, -0.3443, -0.1368, 0.0499, 0.0292, -0.0623, 0.2992, 0.3223, 
                     0.1385, -0.0529, -1.0674, -0.2220, 0.6909, 0.2612, 1.0676, 0.6629, 0.6636),
  `Year Effective` = c(1978, 1988, 1991, 1999, 2004, 2006, 2006, 2007, 2007, 2007, 2007, 2007, 2007, 2009, 
                       2011, 2011, 2012, 2015, 2015, 2016, 2016, 2016, 2016, 2017, 2018, 2018, 2020, 2021, 2021)
)

# View the data
print(data)

library(ggplot2)
library(reshape2)

# Reshape data to long format for plotting
data_long <- data %>%
  pivot_longer(cols = starts_with("Year"), 
               names_to = "Year", 
               values_to = "Log_CI") %>%
  mutate(Year = recode(Year, 
                       "Year..1." = "Year -1", 
                       "Year.0." = "Year 0", 
                       "Year.1." = "Year 1"))

# Filter for Year -1, Year 0, and Year 1 only
data_long_filtered <- data_long %>%
  filter(Year %in% c("Year -1", "Year 0", "Year 1"))

# Plot the line chart
ggplot(data_long_filtered, aes(x = Year, y = Log_CI, group = State, color = State)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Log(Corporate Income)", title = "Change in Log(Corporate Income) by State- Short Run") +
  theme_stata()



data_diff <- data %>%
  pivot_longer(cols = c("Y.0....Y..1.", "Y.1....Y..1."), 
               names_to = "Difference", 
               values_to = "Log_CI_Difference") %>%
  mutate(Difference = recode(Difference, 
                             "Y.0....Y..1." = "Y(0) - Y(-1)", 
                             "Y.1....Y..1." = "Y(1) - Y(-1)"))


# Plot the change-in-change bar chart
ggplot(data_long_filtered, aes(x = State, y = Log_CI_Difference, fill = Difference)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State", y = "Difference in Log(Corporate Income)", title = "Yearly Differences in Log(Corporate Income) by State") +
  theme_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Plot a grouped bar chart
ggplot(data_long_filtered, aes(x = State, y = Log_CI, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "State", y = "Log(Corporate Income)", title = "Log(Corporate Income) by State and Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Short run and Long Run Group Plots

# Creating the data_estimates dataframe in R
data_estimates <- data.frame(
  State = c("Arizona", "Colorado", 
            "Michigan", "Illinois", "Oregon",
            "Nebraska", "Minnesota", "Utah",
            "Georgia", "Wisconsin", "Indiana", "Maine", "Pennsylvania", "South Carolina",
            "California", "New Jersey", "New York",
            "Rhode Island", "Connecticut", "Louisiana", "North Carolina", "Kentucky", "Missouri",
            "North Dakota", "Delaware", "Maryland"),
  SR_Estimate = c(0.01873, 0.01618, 
                  0.00224, 0.11045, 0.13223,
                  -0.01958, -0.11023, -0.02099,
                  -0.19896, -0.23165, -0.64051, -0.12710, -0.13765, -0.16904,
                  -0.27194, -0.07802, -0.00058,
                  0.15475, 0.25046, 0.01182, 0.04647, 0.23948, 0.52780,
                  -0.34955, -0.32371, -0.01044),
  SR_Delta_Percent = c(1.89065, 1.63116, 
                       0.22425, 11.67805, 14.13708,
                       -1.93896, -10.43719, -2.07712,
                       -18.04173, -20.67763, -47.29764, -11.93544, -12.85964, -15.55249,
                       -23.81000, -7.50541, -0.05798,
                       16.73661, 28.46162, 1.18901, 4.75667, 27.05883, 69.51988,
                       -29.49947, -27.65400, -1.03857),
  LR_Estimate = c(0.22250, 0.33630, 
                  -1.23610, -0.42870, -0.03540,
                  0.20450, 0.00610, 0.51340,
                  -0.11590, -0.25650, -0.89030, -0.15510, -0.35020, -0.22710,
                  -0.22390, -0.47160, -0.17000,
                  NA, NA, NA, NA, NA, NA,
                  NA, NA, NA),
  LR_Delta_Percent = c(24.91958, 39.97589, 
                       -70.94850, -34.86447, -3.47807,
                       22.69115, 0.61186, 67.09628,
                       -10.94357, -22.62450, -58.94674, -14.36705, -29.54528, -20.31589,
                       -20.06049, -37.59969, -15.63352,
                       NA, NA, NA, NA, NA, NA,
                       NA, NA, NA),
  Group = c("Positive SR & LR", "Positive SR & LR", 
            "Positive SR & Negative LR", "Positive SR & Negative LR", "Positive SR & Negative LR",
            "Negative SR & Positive LR", "Negative SR & Positive LR", "Negative SR & Positive LR",
            "Negative SR & LR", "Negative SR & LR", "Negative SR & LR", "Negative SR & LR", "Negative SR & LR", "Negative SR & LR",
            "Negative SR & LR", "Negative SR & LR", "Negative SR & LR",
            "Positive SR, < 6 yr LR", "Positive SR, < 6 yr LR", "Positive SR, < 6 yr LR", 
            "Positive SR, < 6 yr LR", "Positive SR, < 6 yr LR", "Positive SR, < 6 yr LR",
            "Negative SR, < 6 yr LR", "Negative SR, < 6 yr LR", "Negative SR, < 6 yr LR")
)

# View the data
print(data_estimates)


# Example data preparation
data_long_estimates <- data_estimates %>%
  pivot_longer(cols = c(SR_Estimate, SR_Delta_Percent, LR_Estimate, LR_Delta_Percent),
               names_to = "Measure", values_to = "Value") %>%
  mutate(Category = case_when(
    Measure == "SR_Estimate" ~ "Short-Run Estimate",
    Measure == "SR_Delta_Percent" ~ "Short-Run Δ %",
    Measure == "LR_Estimate" ~ "Long-Run Estimate",
    Measure == "LR_Delta_Percent" ~ "Long-Run Δ %"
  ))


# plot Estimates, I actually like this plot, if I can get all the bar plots on the same scale.

ggplot(data_long_estimates, aes(x = State, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Group, scales = "free", ncol = 2) +
  labs(x = "State", y = "Estimate / Δ %", title = "Corporate Income Point Estimates by Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_long_estimates, aes(x = State, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Group, scales = "free_x", ncol = 2) +  # "free_x" keeps separate x-axes but uniform y-axis
  labs(x = "State", y = "Estimate / Δ %", title = "Corporate Income Point Estimates by Group") +
  scale_y_continuous(limits = c(-80, 80)) +  # Set a uniform y-axis range across all facets
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



