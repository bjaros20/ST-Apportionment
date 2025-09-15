# TWFE for 2006/2007 with 2015-2018 as the control group.

# Load required libraries
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(lmtest)
library(sandwich)

# Set working directory (modify as needed)
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

# Load data
data <- read.csv("real_log_nci.csv")

# ---- Step 1: Filter Data for the Required Sample ----
# Treated states: Policy in 2006 or 2007
# Control states: Policy in 2015-2018
filtered_data <- data %>%
  filter(year_effective %in% c(2006, 2007, 2015, 2016, 2017, 2018))

# ---- Step 2: Create Treatment Indicator ----
filtered_data <- filtered_data %>%
  mutate(Post = ifelse(year >= year_effective, 1, 0))

# ---- Step 3: Create Non-Corporate Tax Revenue Variables ----
filtered_data <- filtered_data %>%
  mutate(NCTR = TOTLTAX - CORPINCTX,                        # Non-Corporate Tax Revenue
         real_NCTR = (NCTR / CPI_def) * 100,                # Inflation-adjusted NCTR
         log_RNCTR = log(real_NCTR))                        # Log transformation

# ---- Step 4: Estimate TWFE Regression Models ----
# 1. Corporate Tax Revenue (logRealCitRev)
model_corporate <- feols(logRealCitRev ~ Post | State_Acronym + year, data = filtered_data)

# 2. Non-Corporate Tax Revenue (log_RNCTR)
model_non_corporate <- feols(log_RNCTR ~ Post | State_Acronym + year, data = filtered_data)

# 3. Proxy Corporate Income (real_log_nci)
model_proxy_corporate <- feols(real_log_nci ~ Post | State_Acronym + year, data = filtered_data)

# ---- Step 5: Summarize Regression Results ----
summary(model_corporate, cluster = "State_Acronym")
summary(model_non_corporate, cluster = "State_Acronym")
summary(model_proxy_corporate, cluster = "State_Acronym")



# Extract coefficient tables for each model
coef_corporate <- coeftable(model_corporate)
coef_non_corporate <- coeftable(model_non_corporate)
coef_proxy_corporate <- coeftable(model_proxy_corporate)

# Extract coefficient tables for each model
coef_corporate <- as.data.frame(coeftable(model_corporate))
coef_non_corporate <- as.data.frame(coeftable(model_non_corporate))
coef_proxy_corporate <- as.data.frame(coeftable(model_proxy_corporate))

# Create summary table
results_summary <- data.frame(
  "Dependent Variable" = c("Corporate Tax Revenue", "Non-Corporate Tax Revenue", "Proxy Corporate Income"),
  "Observations" = c(nobs(model_corporate), nobs(model_non_corporate), nobs(model_proxy_corporate)),
  "Post Estimate" = c(coef_corporate["Post", "Estimate"], coef_non_corporate["Post", "Estimate"], coef_proxy_corporate["Post", "Estimate"]),
  "Std. Error" = c(coef_corporate["Post", "Std. Error"], coef_non_corporate["Post", "Std. Error"], coef_proxy_corporate["Post", "Std. Error"]),
  "t-value" = c(coef_corporate["Post", "t value"], coef_non_corporate["Post", "t value"], coef_proxy_corporate["Post", "t value"]),
  "p-value" = c(coef_corporate["Post", "Pr(>|t|)"], coef_non_corporate["Post", "Pr(>|t|)"], coef_proxy_corporate["Post", "Pr(>|t|)"]),
)



#simple DiD
# Load required libraries
library(dplyr)
library(fixest)

# ---- Step 1: Filter Data for the Required Sample ----
# Include treated states (2006/2007) and control states (2015-2018)
filtered_data <- data %>%
  filter(year_effective %in% c(2006, 2007, 2015, 2016, 2017, 2018))

# ---- Step 2: Create Treatment and Post Indicators ----
filtered_data <- filtered_data %>%
  mutate(
    Treated = ifelse(year_effective %in% c(2006, 2007), 1, 0),  # 1 if treated in 2006/2007, 0 otherwise
    Post = ifelse(year >= year_effective, 1, 0),               # 1 if after treatment, 0 otherwise
    DiD = Treated * Post                                       # Interaction term
  )

# ---- Step 3: Estimate Simple DiD Models ----
# Corporate Tax Revenue (logRealCitRev)
model_did_corporate <- feols(logRealCitRev ~ Treated + Post + DiD, data = filtered_data)

# Non-Corporate Tax Revenue (log_RNCTR)
model_did_non_corporate <- feols(log_RNCTR ~ Treated + Post + DiD, data = filtered_data)

# Proxy Corporate Income (real_log_nci)
model_did_proxy_corporate <- feols(real_log_nci ~ Treated + Post + DiD, data = filtered_data)

# ---- Step 4: Summarize Regression Results ----
summary(model_did_corporate)
summary(model_did_non_corporate)
summary(model_did_proxy_corporate)

# ---- Step 5: Organize Results into a Table ----
extract_did_coef <- function(model) {
  coef_table <- as.data.frame(coeftable(model))
  if ("DiD" %in% rownames(coef_table)) {
    return(coef_table["DiD", ])
  } else {
    return(data.frame(Estimate = NA, `Std. Error` = NA, `t value` = NA, `Pr(>|t|)` = NA))
  }
}

# Extract results for 'DiD' term
coef_did_corporate <- extract_did_coef(model_did_corporate)
coef_did_non_corporate <- extract_did_coef(model_did_non_corporate)
coef_did_proxy_corporate <- extract_did_coef(model_did_proxy_corporate)

# Create results summary table
did_results_summary <- data.frame(
  "Dependent Variable" = c("Corporate Tax Revenue", "Non-Corporate Tax Revenue", "Proxy Corporate Income"),
  "DiD Estimate" = c(coef_did_corporate$Estimate, coef_did_non_corporate$Estimate, coef_did_proxy_corporate$Estimate),
  "Std. Error" = c(coef_did_corporate$`Std. Error`, coef_did_non_corporate$`Std. Error`, coef_did_proxy_corporate$`Std. Error`),
  "t-value" = c(coef_did_corporate$`t value`, coef_did_non_corporate$`t value`, coef_did_proxy_corporate$`t value`),
  "p-value" = c(coef_did_corporate$`Pr(>|t|)`, coef_did_non_corporate$`Pr(>|t|)`, coef_did_proxy_corporate$`Pr(>|t|)`)
)

# Print results
print(did_results_summary)

# ---- Step 6: Save Results ----
write.csv(did_results_summary, "DiD_Regression_Results.csv", row.names = FALSE)


# Simple DiD plots

# Load necessary libraries
library(ggplot2)
library(dplyr)

# ---- Step 1: Prepare Data for Plotting ----
plot_data <- filtered_data %>%
  mutate(Treatment = ifelse(Treated == 1, "Treated", "Control")) %>%  # Label groups
  group_by(year, Treatment) %>%
  summarise(
    Mean_Log_Corp_Tax = mean(logRealCitRev, na.rm = TRUE),
    Mean_Log_NCTR = mean(log_RNCTR, na.rm = TRUE),
    Mean_Log_Proxy_Corp = mean(real_log_nci, na.rm = TRUE),
    .groups = "drop"
  )

# Define Treatment Year (2007)
treatment_year <- 2007

# ---- Step 2: Generate Plots ----

# 1️⃣ Corporate Tax Revenue Plot
ggplot(plot_data, aes(x = year, y = Mean_Log_Corp_Tax, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Corporate Tax Revenue",
       subtitle = "Difference-in-Differences Analysis",
       x = "Year",
       y = "Average log(Corporate Tax Revenue)",
       color = "Group") +
  theme_minimal()

# 2️⃣ Non-Corporate Tax Revenue Plot
ggplot(plot_data, aes(x = year, y = Mean_Log_NCTR, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Non-Corporate Tax Revenue",
       subtitle = "Difference-in-Differences Analysis",
       x = "Year",
       y = "Average log(Non-Corporate Tax Revenue)",
       color = "Group") +
  theme_minimal()

# 3️⃣ Proxy Corporate Income Plot
ggplot(plot_data, aes(x = year, y = Mean_Log_Proxy_Corp, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Proxy Corporate Income",
       subtitle = "Difference-in-Differences Analysis",
       x = "Year",
       y = "Average log(Proxy Corporate Income)",
       color = "Group") +
  theme_minimal()




#Event Study Style plots
# Load necessary libraries
library(fixest)
library(ggplot2)

# ---- Step 1: Estimate Event-Study Models with 2007 as Reference Year ----
# Corporate Tax Revenue
event_study_corporate <- feols(logRealCitRev ~ i(year, Treated, ref = 2007) | State_Acronym + year, 
                               data = filtered_data, 
                               cluster = ~State_Acronym)

# Non-Corporate Tax Revenue
event_study_non_corporate <- feols(log_RNCTR ~ i(year, Treated, ref = 2007) | State_Acronym + year, 
                                   data = filtered_data, 
                                   cluster = ~State_Acronym)

# Proxy Corporate Income
event_study_proxy_corporate <- feols(real_log_nci ~ i(year, Treated, ref = 2007) | State_Acronym + year, 
                                     data = filtered_data, 
                                     cluster = ~State_Acronym)

# ---- Step 2: Generate Event-Study Plots ----
# Corporate Tax Revenue Plot
iplot(event_study_corporate, 
      xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate",
      main = "Event Study: Impact on Corporate Tax Revenue",
      ref.line = TRUE)

# Non-Corporate Tax Revenue Plot
iplot(event_study_non_corporate, 
      xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate",
      main = "Event Study: Impact on Non-Corporate Tax Revenue",
      ref.line = TRUE)

# Proxy Corporate Income Plot
iplot(event_study_proxy_corporate, 
      xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate",
      main = "Event Study: Impact on Proxy Corporate Income",
      ref.line = TRUE)



#1990 as earliest Year, everything done and saved.

# Load necessary libraries
library(dplyr)
library(fixest)
library(ggplot2)
library(readr)

# ---- Step 1: Filter Data for the Required Sample (1990 onward) ----
filtered_data <- data %>%
  filter(year >= 1990, year_effective %in% c(2006, 2007, 2015, 2016, 2017, 2018))

# ---- Step 2: Create Treatment Indicators ----
filtered_data <- filtered_data %>%
  mutate(
    Treated = ifelse(year_effective %in% c(2006, 2007), 1, 0),  # 1 if treated in 2006/2007, 0 otherwise
    Post = ifelse(year >= year_effective, 1, 0),               # 1 if after treatment, 0 otherwise
    DiD = Treated * Post                                       # Interaction term
  )

# ---- Step 3: Estimate TWFE Regression Models ----
model_corporate <- feols(logRealCitRev ~ Post | State_Acronym + year, data = filtered_data)
model_non_corporate <- feols(log_RNCTR ~ Post | State_Acronym + year, data = filtered_data)
model_proxy_corporate <- feols(real_log_nci ~ Post | State_Acronym + year, data = filtered_data)

# ---- Step 4: Extract TWFE Results ----
extract_model_results <- function(model) {
  coef_table <- as.data.frame(coeftable(model))
  data.frame(
    "Estimate" = coef_table["Post", "Estimate"],
    "Std. Error" = coef_table["Post", "Std. Error"],
    "t-value" = coef_table["Post", "t value"],
    "p-value" = coef_table["Post", "Pr(>|t|)"]
  )
}

twfe_results <- data.frame(
  "Approach" = "TWFE",
  "Dependent Variable" = c("Corporate Tax Revenue", "Non-Corporate Tax Revenue", "Proxy Corporate Income"),
  extract_model_results(model_corporate),
  extract_model_results(model_non_corporate),
  extract_model_results(model_proxy_corporate)
)

# ---- Step 5: Estimate Simple DiD Models ----
model_did_corporate <- feols(logRealCitRev ~ Treated + Post + DiD, data = filtered_data)
model_did_non_corporate <- feols(log_RNCTR ~ Treated + Post + DiD, data = filtered_data)
model_did_proxy_corporate <- feols(real_log_nci ~ Treated + Post + DiD, data = filtered_data)

# Extract results for 'DiD' term
did_results <- data.frame(
  "Approach" = "Simple DiD",
  "Dependent Variable" = c("Corporate Tax Revenue", "Non-Corporate Tax Revenue", "Proxy Corporate Income"),
  extract_model_results(model_did_corporate),
  extract_model_results(model_did_non_corporate),
  extract_model_results(model_did_proxy_corporate)
)

# ---- Step 6: Combine and Save Results ----
combined_results <- bind_rows(twfe_results, did_results)
write_csv(combined_results, "Regression_Results.csv")

# ---- Step 7: Generate Treated vs. Control Group Plots ----
plot_data <- filtered_data %>%
  mutate(Treatment = ifelse(Treated == 1, "Treated", "Control")) %>%
  group_by(year, Treatment) %>%
  summarise(
    Mean_Log_Corp_Tax = mean(logRealCitRev, na.rm = TRUE),
    Mean_Log_NCTR = mean(log_RNCTR, na.rm = TRUE),
    Mean_Log_Proxy_Corp = mean(real_log_nci, na.rm = TRUE),
    .groups = "drop"
  )

treatment_year <- 2007

# Create and Save Plots as Objects
corp_tax_plot <- ggplot(plot_data, aes(x = year, y = Mean_Log_Corp_Tax, color = Treatment)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Corporate Tax Revenue", x = "Year", y = "Avg. log(Corp Tax Revenue)") +
  theme_minimal()
ggsave("corp_tax_plot.png", corp_tax_plot)

non_corp_tax_plot <- ggplot(plot_data, aes(x = year, y = Mean_Log_NCTR, color = Treatment)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Non-Corporate Tax Revenue", x = "Year", y = "Avg. log(Non-Corp Tax Revenue)") +
  theme_minimal()
ggsave("non_corp_tax_plot.png", non_corp_tax_plot)

proxy_income_plot <- ggplot(plot_data, aes(x = year, y = Mean_Log_Proxy_Corp, color = Treatment)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Treated vs. Control: Log Proxy Corporate Income", x = "Year", y = "Avg. log(Proxy Corporate Income)") +
  theme_minimal()
ggsave("proxy_income_plot.png", proxy_income_plot)

# ---- Step 8: Generate Event-Study Plots ----
event_study_corporate <- feols(logRealCitRev ~ i(year, Treated, ref = 2007) | State_Acronym + year, data = filtered_data, cluster = ~State_Acronym)
event_study_non_corporate <- feols(log_RNCTR ~ i(year, Treated, ref = 2007) | State_Acronym + year, data = filtered_data, cluster = ~State_Acronym)
event_study_proxy_corporate <- feols(real_log_nci ~ i(year, Treated, ref = 2007) | State_Acronym + year, data = filtered_data, cluster = ~State_Acronym)

# Save Event-Study Plots as Objects and PNGs
# Save Event-Study Plots using base R plotting functions
png("corp_event_plot.png", width = 1140, height = 685)  # Adjust size as needed
iplot(event_study_corporate, xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate", 
      main = "Event Study: Corporate Tax Revenue", 
      ref.line = TRUE)
dev.off()

png("non_corp_event_plot.png", width = 1140, height = 685)
iplot(event_study_non_corporate, xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate", 
      main = "Event Study: Non-Corporate Tax Revenue", 
      ref.line = TRUE)
dev.off()

png("proxy_event_plot.png", width = 1140, height = 685)
iplot(event_study_proxy_corporate, xlab = "Year Relative to 2007",
      ylab = "Coefficient Estimate", 
      main = "Event Study: Proxy Corporate Income", 
      ref.line = TRUE)
dev.off()



