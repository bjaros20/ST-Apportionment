#Code from Iowa practice for Percentage Change

#Create base year 1976 plot
Iowa_base76 <- syn_Iowa %>%
  group_by(State_Acronym) %>%
  mutate(
    real_ci_cap_76 = (real_ci_cap / real_ci_cap[year == 1976]) * 100,
    syn_Iowa_76 = (syn_Iowa / syn_Iowa[year == 1976]) * 100
  ) %>%
  ungroup()


#Create the shift
diff_base76 <- Iowa_base76 %>%
  group_by(State_Acronym)%>%
  mutate(diff = abs(real_ci_cap_76 - syn_Iowa_76)) %>%
  ungroup()

# use optimize
objective_function <- function(shift) {
  # Filter the data where Post == 0
  filtered_data <- Iowa_base76[Iowa_base76$Post == 0, ]
  # Calculate the absolute difference with the shift applied
  difference <- abs(filtered_data$real_ci_cap_76 - (filtered_data$syn_Iowa_76 + shift))
  # Calculate the average difference
  average_difference <- mean(difference)
  return(average_difference)
}

# Use optim to find the shift that minimizes the average absolute difference
initial_guess <- 0  # Start with an initial guess for shift
# Optimizing the shift using optim
result <- optim(par = initial_guess, fn = objective_function, method = "Brent", lower = -100, upper = 100)


# Print the optimal shift value
optimal_shift <- result$par
optimal_shift


#mutate the Iowa_base76 df to generate the syn_Iowa_shift
shift_Iowa_base_76 <- Iowa_base76 %>%
  mutate(syn_Iowa_76_shift = syn_Iowa_76 + optimal_shift)


Iowa_perc <- shift_Iowa_base_76 %>%
  mutate(perc_chan = (real_ci_cap_76-syn_Iowa_76_shift)/syn_Iowa_76_shift)

#Mid point formula
Iowa_perc <- shift_Iowa_base_76 %>%
  mutate(perc_chan = (real_ci_cap_76 - syn_Iowa_76_shift) / ((real_ci_cap_76 + syn_Iowa_76_shift) / 2))


#Short Run Percentage Change
Iowa_SR_perc <- Iowa_perc %>%
  filter(year %in% c(1978, 1979, 1980)) %>% 
  summarize(Avg_SR_perc = mean(perc_chan, na.rm = TRUE)) 
print(Iowa_SR_perc*100)


#Long Run percentage Change
Iowa_LR_perc <- Iowa_perc %>%
  summarize(Avg_SR_perc = mean(perc_chan))
print(Iowa_LR_perc*100)


#Iowa plots

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

