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


