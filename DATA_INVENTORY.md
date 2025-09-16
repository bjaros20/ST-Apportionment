# Data Inventory and Replication Guide

This document links each section of the paper to the underlying data files and scripts.

---

## Section 2.4 – Fixed Factors and Severance Tax
- **Data files:**
  - `analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv` – cleaned dataset for regression
  - Generated from raw inputs:
    - `data/raw/filled_data_jmp.csv`
    - `data/raw/Non_CIT_states_FRED_OH.csv`
    - `data/raw/ssfa_data_jmp.xlsx`
    - `data/raw/clean_rates_1976-2022.csv`
    - `data/raw/rates_jmp.csv`
    - `data/raw/elasticity_rates_jmp.csv` (alternate version of rates)
- **Scripts:**
  - `analysis/2.4_fixed_factors_logit/scripts/Severance2WFE.R`
- **Results in paper:**
  - Table 1 – Logistic regression (odds ratios) showing higher severance tax revenues reduce probability of SSFA adoption

---

## Section 2.5 – Employment and Manufacturing
- **Data files:**
  - `data/raw/CES_employment.csv`
  - `data/raw/manufacturing_employment.csv`
  - `data/raw/LAUS_unemployment.csv`
- **Scripts:**
  - `analysis/2.5_employment_probit/scripts/employment_hazard_logit.R`
  - `analysis/2.5_employment_probit/scripts/employment_hazard_probit.R`
- **Results in paper:**
  - Tables 2 and 3 (hazard models, logit and probit)

---

## Section 7.1 – Year-to-Year Changes
- **Data files:**
  - `data/raw/corp_income_tax_collections.csv`  
  - `data/raw/corp_tax_rates.csv`
- **Scripts:**
  - `analysis/7.1_yearly_changes/scripts/yearly_diff.R`
- **Results in paper:**
  - Table 7 (Yearly Differences)
  - Figures 1–3 (group trends)

---

## Section 7.2 – Truncated Sample
- **Data files:**
  - Same as 7.1 (corporate income and tax rates), restricted to 2007/2015–18 cohorts
- **Scripts:**
  - `analysis/7.2_truncated_sample/scripts/TwoWayFEReg.R`
  - `analysis/7.2_truncated_sample/scripts/Direct_Switch_First7.R`
  - `analysis/7.2_truncated_sample/scripts/SimplePlots_DirectSwitch.R`
- **Results in paper:**
  - Tables 8–10 (TWFE, DID, Event-study)
  - Figure 4 (event study plot)
  - Figure 8 (treated vs. control)

---

## Section 7.3 – Synthetic DID
- **Data files:**
  - `data/raw/corp_income_tax_collections.csv`  
  - `data/raw/corp_tax_rates.csv`
- **Scripts:**
  - `analysis/7.3_synthetic_did/scripts/loop_sDiD.R`
  - `analysis/7.3_synthetic_did/scripts/pipeline_bootstrap_sDiD.R`
  - `analysis/7.3_synthetic_did/scripts/retroactive_sDiD.R`
  - `analysis/7.3_synthetic_did/scripts/SynDiD.R`
- **Results in paper:**
  - Tables 11–13 (Synthetic DID estimates)
  - Figures in appendix (online)

---

## Section 7.4 – Non-Corporate Income
- **Data files:**
  - `data/raw/non_corp_tax_collections.csv` (FRED total minus corporate tax)
  - `data/raw/state_population.csv`
- **Scripts:**
  - `analysis/7.4_noncorp_income/scripts/noncorp_did.R`
- **Results in paper:**
  - Tables 22–24 (Non-corporate tax estimates)
