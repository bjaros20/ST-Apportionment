# Section 2.5 – Employment, Manufacturing, and SSFA Adoption

This folder contains the data and code used to estimate the hazard models in Section 2.5 of the paper.

## Data
- `data/Severance_Cap_mutate.csv` – base dataset from Section 2.4, containing severance tax revenues and treatment timing.
- `data/BEA_GDPGrowth.csv` – BEA GDP growth series.
- `data/CurrentEmploymentStatistics_National.csv` – BLS CES national employment.
- `data/CurrentEmploymentStatistics_States.csv` – BLS CES state-level employment.
- `data/LocalAreaUnemploymentStatistics_States.csv` – BLS LAUS state unemployment rates.
- `data/StatewideManufacturingEmployment_States.csv` – BLS CES state-level manufacturing employment.

## Script
- `scripts/SSFA_Why_Switch_Script.R` – complete pipeline for Section 2.5:
  1. Loads severance and state-level predictors.
  2. Creates hazard panel (at risk until adoption year).
  3. Constructs lagged predictors.
  4. Estimates hazard models with both logit (main) and probit (robustness).
  5. Produces LaTeX-ready regression tables.

## Results in Paper
- **Table 2** – Logit hazard model (odds ratios for lagged employment, unemployment, manufacturing).
- **Table 3** – Probit hazard model (coefficients).

## Replication Notes
All results can be reproduced directly by running the script:
```r
source("analysis/2.5_employment_probit/scripts/SSFA_Why_Switch_Script.R")
