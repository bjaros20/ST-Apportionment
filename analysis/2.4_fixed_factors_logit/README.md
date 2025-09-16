# Section 2.4 – Fixed Factors and Severance Tax

This folder contains the data and code used to produce the severance tax logit regression (Table 1 in the paper).

## Data
- `data/` – raw inputs (FRED revenue, CIT rates, SSFA adoption data)
- `output/Sev_early_switch.csv` – **final cleaned dataset** used in the regression

## Script
- `scripts/Severance2WFE.R` – processes raw data → cleaned dataset → regression

## Replication Notes
Several intermediate datasets are generated along the way:
- `FRED_rev_all_states.csv`: merged corporate tax revenues
- `Severance_2WFE.csv`: adds treatment timing variables for event study
- `Severance_Population.csv`: population and per-capita values
- `Severance_Cap_mutate.csv`: cleaned per-capita severance tax values
- `No_Sev_revenue.csv`: diagnostic (states/years with zero severance)

The key dataset for replication is:

analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv


It contains severance tax revenues, treatment timing, and the `early_switch` indicator.

## Core Regression
To replicate Table 1:

sev <- read.csv("analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv")

logit_model <- glm(early_switch ~ Sev_log, data = sev, family = binomial)
summary(logit_model)
exp(coef(logit_model))   # odds ratios

The original script is saved in the scripts folder.
