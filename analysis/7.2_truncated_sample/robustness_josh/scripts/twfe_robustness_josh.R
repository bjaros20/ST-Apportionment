################################################################################
# REFEREE RESPONSE: TWFE Robustness Checks 
# Purpose: Test if negative coefficient comes from state FE vs. pooling cohorts
#          and add state-specific trends specification
# Date: November 2025
################################################################################

# Load required libraries
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(lmtest)
library(sandwich)
install.packages("modelsummary")
library(modelsummary)  # NEW: for creating comparison tables
install.packages("kableExtra")
library(kableExtra)    # For nice table formatting

# Create the robustness folder and subfolders
dir.create("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/robustness_josh", 
           showWarnings = FALSE)
dir.create("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/robustness_josh/scripts", 
           showWarnings = FALSE)
dir.create("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/robustness_josh/output", 
           showWarnings = FALSE)

# Verify it was created
list.files("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample")
list.files("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/robustness_josh")


#Set working directory
setwd("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/data")

# Load data
data <- read.csv("real_log_nci.csv")

# Quick data check
cat("\n=== DATA LOADED ===\n")
cat("Total observations:", nrow(data), "\n")
cat("Years range:", min(data$year), "to", max(data$year), "\n")
cat("Number of states:", length(unique(data$State_Acronym)), "\n\n")


################################################################################
# FILTER DATA: 1990-2014, 2007 Switchers vs Late Switchers
################################################################################

data_2007 <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    # Treatment indicator
    PostTreatment = ifelse(State_Acronym %in% c("AZ", "IN", "ME", "MN", "PA", "SC") & 
                             year >= 2007, 1, 0),
    # State-specific time trend (for Josh's Request #2)
    state_trend = year - 1990
  )

# Verify filtered data
cat("\n=== FILTERED DATA (1990-2014) ===\n")
cat("Total observations:", nrow(data_2007), "\n")
cat("Treatment observations (post-2007):", sum(data_2007$PostTreatment), "\n")
cat("2007 Switchers (treatment):", 
    paste(unique(data_2007$State_Acronym[data_2007$year_effective == 2007]), collapse = ", "), "\n")
cat("Late Switchers (control):", 
    paste(unique(data_2007$State_Acronym[data_2007$year_effective >= 2015]), collapse = ", "), "\n\n")


################################################################################
# MODEL 1: ORIGINAL TABLE 8 - State FE + Year FE (for reference)
################################################################################

cat("\n=== MODEL 1: ORIGINAL TABLE 8 (State FE + Year FE) ===\n")

model_1_cluster <- feols(
  real_log_nci ~ PostTreatment | State_Acronym + year,
  data = data_2007,
  cluster = ~State_Acronym
)

model_1_homo <- feols(
  real_log_nci ~ PostTreatment | State_Acronym + year,
  data = data_2007,
  se = "standard"
)

cat("Clustered SE coefficient:", coef(model_1_cluster)["PostTreatment"], "\n")
cat("Expected: -0.34292 (should match Table 8)\n\n")

################################################################################
# MODEL 2: JOSH REQUEST #1 - Year FE ONLY (no State FE)
################################################################################

cat("=== MODEL 2: YEAR FE ONLY (Josh Request #1) ===\n")
cat("Purpose: Test if negative coefficient comes from state-level differences\n")
cat("         vs. pooling treatment cohorts\n\n")

model_2_cluster <- feols(
  real_log_nci ~ PostTreatment | year,
  data = data_2007,
  cluster = ~State_Acronym
)

model_2_homo <- feols(
  real_log_nci ~ PostTreatment | year,
  data = data_2007,
  se = "standard"
)

cat("Year FE Only coefficient:", coef(model_2_cluster)["PostTreatment"], "\n")
cat("Interpretation: If similar to Model 1 → effect from pooling cohorts\n")
cat("                If different → state-level differences matter\n\n")

################################################################################
# MODEL 3: JOSH REQUEST #2 - State FE + Year FE + State Trends
################################################################################

cat("=== MODEL 3: STATE-SPECIFIC LINEAR TRENDS (Josh Request #2) ===\n")
cat("Purpose: Control for idiosyncratic state booms/slowdowns\n")
cat("         (similar to what SDID does with reweighting)\n\n")

model_3_cluster <- feols(
  real_log_nci ~ PostTreatment + state_trend:factor(State_Acronym) | State_Acronym + year,
  data = data_2007,
  cluster = ~State_Acronym
)

cat("State Trends coefficient:", coef(model_3_cluster)["PostTreatment"], "\n")
cat("Interpretation: If coefficient robust → not driven by pre-trends\n\n")

################################################################################
# CREATE COMPARISON TABLE
################################################################################

# Create list of all models for comparison
models_list <- list(
  "TWFE\n(Clustered)" = model_1_cluster,
  "TWFE\n(Homo)" = model_1_homo,
  "Year FE Only\n(Clustered)" = model_2_cluster,
  "Year FE Only\n(Homo)" = model_2_homo,
  "State Trends\n(Clustered)" = model_3_cluster
)

# Create comparison table
comparison_table <- modelsummary(
  models_list,
  stars = c('*' = .10, '**' = .05, '***' = .01),
  gof_map = c("nobs", "r.squared", "adj.r.squared", "FE: State_Acronym", "FE: year"),
  coef_rename = c("PostTreatment" = "SSFA Treatment"),
  title = "Table: TWFE Robustness Checks - Referee Response to Josh",
  notes = c(
    "Dependent variable: log(Real Corporate Income)",
    "Sample: 1990-2014, 2007 switchers vs. late switchers (2015-2018)",
    "Columns 1-2: Original Table 8 specification (State FE + Year FE)",
    "Columns 3-4: Year FE only - tests if effect from state differences vs. pooling",
    "Column 5: Adds state-specific linear trends to control for idiosyncratic trends"
  )
)

print(comparison_table)

# Save table outputs
setwd("~/Documents/GitHub/ST-Apportionment/analysis/7.2_truncated_sample/robustness_josh/output")

# Save as text file
sink("table_robustness_comparison.txt")
print(comparison_table)
sink()

# Save as LaTeX (for later)
modelsummary(models_list,
             output = "table_robustness_comparison.tex",
             stars = c('*' = .10, '**' = .05, '***' = .01))

cat("\n✓ Tables saved to output folder\n")

#Event study plot
event_study_2007 <- feols(
  real_log_nci ~ i(year, Treated, ref = 2006) | State_Acronym + year,
  data = data_2007,
  cluster = ~State_Acronym
)
