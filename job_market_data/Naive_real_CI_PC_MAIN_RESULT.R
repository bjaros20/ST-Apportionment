# Main result: Naive, Real, Corporate Income per capita
#Empirical Approach, sDiD
# Care about the t-statistic, need to BOOTSTRAP with replace for normal distribution
#Run a one-sided Significance test

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

#Load Detrended, per capita, real revenue data
Rev <- read.csv("detrend_per_capita.csv")
Corp <-read.csv("CIT_sDID_loop.csv")
naive_ci<-read.csv("naive_ci.csv")

naive_ci <- naive_ci %>%
  select(-X)

write.csv(naive_ci,"naive_ci.csv", row.names = FALSE)




#BELOW IS THE LOOP RUN FOR RESULT 3.8
#estimate again, and run with one- sided (tailed test)

# Next thing to estimate, Real Corporate Income, base year 1983-1984
real_CI <- naive_ci%>%
  mutate(real_ci = (naive_ci/CPI_def)*100)

real_CI_cap <- real_CI %>%
  mutate(real_ci_cap = real_ci/population)

#Create base dataframe that has nat_share as dependent variable.
Filter_frac <-real_CI_cap %>%
  select(State_Acronym,year,year_effective,State_Name,real_ci_cap,Post)

filt_Corp <-Filter_frac


#Create State Dataframes
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
  if(df$year_effective[counter] >= 2022) {break}
  
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state$State_Name
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, 
  df <- df %>% 
    filter(is.na(year_effective) | is.na(State_Acronym) | year_effective >= treatment_year)
  
  # removes states treated in same year
  #the year_effective=treatment_year are filtered out if they are not 'treatment_state'
  df <- df %>%
    filter(is.na(year_effective) | (!(State_Name != treatment_state_name & year_effective == treatment_year)))
  
  
  # Filter out rows with year <= 2 years after treatment_year
  df <- df %>%
    filter(year <= treatment_year + 2)
  
  #filter out any states that get treated within 2 years of treatment
  df <- df %>%
    filter(is.na(year_effective) | (!(year_effective > treatment_year & year_effective<= treatment_year + 2)))
  
  #filter out Ohio after treatment_year >=2012, because OH eliminates CI in 2014
  df <- df %>%
    filter(!(treatment_year >= 2012 & State_Name == "Ohio"))
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2022) {break}
  
  #increment counter
  counter <-counter + 1
  
  #empty dataframe break
  if (nrow(df) == 0) {break}
}



#STEP 2, create just a point estimate loop with t-stat and confidence intervals

# Initialize an empty list to store point estimates and statistics
point_estimate_list <- list()

# Loop over each dataframe in result_list
for (state_name in names(result_list)) {
  # Access the dataframe
  current_df <- result_list[[state_name]]
  
  #Make the tibble from the result list a dataframe, easier for panel.matrics function
  current_df <- as.data.frame(current_df)
  
  #Drop states that have NA for ratio (like Alaska because no Income Tax)
  current_df <- na.omit(current_df[, c("State_Acronym", "year", "real_ci_cap", "Post")])
  
  #eliminate Inf lines and the state that has them for estimation, like Ohio 2009-2013
  states_with_inf <- current_df %>%
    group_by(State_Acronym) %>%
    filter(any(is.infinite(real_ci_cap))) %>%
    pull(State_Acronym) %>%
    unique()
  
  # Filter out those states from the dataframe
  current_df <- current_df %>%
    filter(!State_Acronym %in% states_with_inf)
  
  # Create the panel matrices for sDiD using synthdid
  current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")
  
  # Calculate the synthetic difference-in-differences estimate
  current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)
  se <- sqrt(vcov(current_tau_hat, method = 'placebo'))
  print(se)
  
  
  
#  vcov.synthdid_estimate(current_tau_hat,method = 'bootstrap',replications = 500)
  
  # Calculate the t-statistic
  t_statistic <- as.numeric(current_tau_hat) / se
  
  # Calculate the p-value
  p_value <- 2 * pt(abs(t_statistic), df = nrow(current_df) - 1, lower.tail = FALSE)
  
  # Print the point estimate, confidence interval, t-statistic, and p-value
  cat(sprintf('State: %s\n', state_name))
  cat(sprintf('Point estimate: %1.2f\n', current_tau_hat))
  cat(sprintf('95%% CI (%1.2f, %1.2f)\n', current_tau_hat - 1.96 * se, current_tau_hat + 1.96 * se))
  cat(sprintf('t-statistic: %1.3f\n', t_statistic))
  cat(sprintf('p-value: %1.4f\n', p_value))
  
  # Summary statistics
  #  print(summary(current_tau_hat))
}


#TRY BOOTSTRAP OUTSIDE OF the loop

#' @references Dmitry Arkhangelsky, Susan Athey, David A. Hirshberg, Guido W. Imbens, and Stefan Wager.
#'  "Synthetic Difference in Differences". arXiv preprint arXiv:1812.09970, 2019.
#'
#' @method vcov synthdid_estimate
#' @export
# Define the custom vcov function for synthdid_estimate
vcov.synthdid_estimate <- function(object, method = c("bootstrap", "jackknife", "placebo"), replications = 500, ...) {
  method = match.arg(method)
  if(method == 'bootstrap') {
    se = bootstrap_se(object, replications)
  } else if(method == 'jackknife') {
    se = jackknife_se(object)
  } else if(method == 'placebo') {
    se = placebo_se(object, replications)
  }
  matrix(se^2)
}

#' Calculate the standard error of a synthetic diff in diff estimate. Deprecated. Use vcov.synthdid_estimate.
#' @param ... Any valid arguments for vcov.synthdid_estimate
# @export synthdid_se

synthdid_se = function(current_tau_hat,) { sqrt(vcov(...)) }

 replications <-500
 estimate <-current_tau_hat
 setup<- current_sDiD
 
# The bootstrap se: Algorithm 2 of Arkhangelsky et al.
bootstrap_se = function(estimate, replications) { sqrt((replications-1)/replications) * sd(bootstrap_sample(estimate, replications)) }



bootstrap_sample = function(estimate, replications) {
  setup = attr(estimate, 'setup')
  opts = attr(estimate, 'opts')
  weights = attr(estimate, 'weights')
  if (setup$N0 == nrow(setup$Y) - 1) { return(NA) }
  theta = function(ind) {
    if(all(ind <= setup$N0) || all(ind > setup$N0)) { NA }
    else {
      weights.boot = weights
      weights.boot$omega = sum_normalize(weights$omega[sort(ind[ind <= setup$N0])])
      do.call(synthdid_estimate, c(list(Y=setup$Y[sort(ind),], N0=sum(ind <= setup$N0), T0=setup$T0, X=setup$X[sort(ind), ,], weights=weights.boot), opts))
    }
  }
  bootstrap.estimates = rep(NA, replications)
  count = 0
  while(count < replications) {
    bootstrap.estimates[count+1] = theta(sample(1:nrow(setup$Y), replace=TRUE))
    if(!is.na(bootstrap.estimates[count+1])) { count = count+1 }
  }
  bootstrap.estimates
}





#Summary Statistics for real_ci_cap
summary_stats <- Filter_frac %>%
  filter(!is.na(real_ci_cap) & is.finite(real_ci_cap)) %>%  # Remove rows with NA in nat_share
  group_by(State_Name) %>%
  summarize(
    mean = mean(real_ci_cap, na.rm = TRUE),
    median = median(real_ci_cap, na.rm = TRUE),
    IQR = IQR(real_ci_cap, na.rm = TRUE),
    min = min(real_ci_cap, na.rm = TRUE),
    max = max(real_ci_cap, na.rm = TRUE)
  )

print(summary_stats,n = nrow(summary_stats))



current_sDiD <- panel.matrices(current_df, unit = "State_Acronym", time = "year", outcome = "real_ci_cap", treatment = "Post")
current_tau_hat <- synthdid_estimate(current_sDiD$Y, current_sDiD$N0, current_sDiD$T0)

# Calculate standard error using bootstrap method
result <- tryCatch({
  se <- sqrt(vcov(current_tau_hat, method = "bootstrap", replications = 500))
  print(se)  # If successful, print the SE
  se  # Return the SE for further calculations
}, error = function(e) {
  cat(sprintf('Bootstrap method failed. Error: %s\n', e$message))
  NA  # Return NA if bootstrap fails
})

# Print the result
print(result)


test_vcov <- vcov(current_tau_hat, method = "bootstrap", replications = 500)
print(test_vcov)


# Create example data
example_Y <- matrix(c(
  1, 2, 3, 4, 5, 6,
  2, 3, 4, 5, 6, 7,
  3, 4, 5, 6, 7, 8,
  4, 5, 6, 7, 8, 9,
  5, 6, 7, 8, 9, 10,
  6, 7, 8, 9, 10, 11  # Adding more rows
), nrow=6)  # Increase rows

example_N0 <- 5  # Number of control units
example_T0 <- 3  # Number of treatment periods

# Estimate using synthdid
library(synthdid)  # Make sure the synthdid package is loaded

example_estimate <- tryCatch({
  synthdid_estimate(example_Y, example_N0, example_T0)
}, error = function(e) {
  cat(sprintf('Error in synthdid_estimate: %s\n', e$message))
  NA
})

# Check vcov with the example estimate
if (!is.na(example_estimate)) {
  example_vcov <- tryCatch({
    vcov(example_estimate, method = "bootstrap", replications = 10)
  }, error = function(e) {
    cat(sprintf('Error: %s\n', e$message))
    NA
  })
  
  print(example_vcov)
}
