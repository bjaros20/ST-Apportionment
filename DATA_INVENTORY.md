# Data Inventory and Replication Guide

This document links each section of the paper to the underlying data files and scripts.

---

## Section 2.4 – Fixed Factors and Severance Tax
- **Data files:**
  - `analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv` – cleaned dataset for regression
  - Generated from raw inputs (stored locally in this section):
    - `analysis/2.4_fixed_factors_logit/data/filled_data_jmp.csv`
    - `analysis/2.4_fixed_factors_logit/data/Non_CIT_states_FRED_OH.csv`
    - `analysis/2.4_fixed_factors_logit/data/ssfa_data_jmp.xlsx`
    - `analysis/2.4_fixed_factors_logit/data/clean_rates_1976-2022.csv`
    - `analysis/2.4_fixed_factors_logit/data/rates_jmp.csv`
    - `analysis/2.4_fixed_factors_logit/data/elasticity_rates_jmp.csv`
- **Scripts:**
  - `analysis/2.4_fixed_factors_logit/scripts/Severance2WFE.R`
- **Results in paper:**
  - Table 1 – Logistic regression (odds ratios) showing higher severance tax revenues reduce probability of SSFA adoption

---

## Section 2.5 – Employment and Manufacturing

- **Data files:**
  - `analysis/2.5_employment_probit/data/Severance_Cap_mutate.csv` – base panel of severance tax and adoption timing  
  - `analysis/2.5_employment_probit/data/BEA_GDPGrowth.csv` – BEA GDP growth controls  
  - `analysis/2.5_employment_probit/data/CurrentEmploymentStatistics_National.csv` – CES national employment  
  - `analysis/2.5_employment_probit/data/CurrentEmploymentStatistics_States.csv` – CES state-level employment  
  - `analysis/2.5_employment_probit/data/LocalAreaUnemploymentStatistics_States.csv` – LAUS unemployment rates  
  - `analysis/2.5_employment_probit/data/StatewideManufacturingEmployment_States.csv` – BLS manufacturing employment  

- **Scripts:**
  - `analysis/2.5_employment_probit/scripts/SSFA_Why_Switch_Script.R` – builds hazard panel, cleans employment/manufacturing/unemployment datasets, runs logit and probit models

- **Results in paper:**
  - Table 2 – Logit hazard model of SSFA adoption  
  - Table 3 – Probit hazard model of SSFA adoption  



---

## Section 7.1 – Year-to-Year Changes
- **Data files:**
  - `analysis/7.1_yearly_changes/data/real_log_nci.csv` – base dataset (real, log taxable corporate income by state/year)
- **Scripts:**
  - `analysis/7.1_yearly_changes/scripts/desc_log_nci.R`
- **Outputs:**
  - `analysis/7.1_yearly_changes/output/sr_descriptive_log_nci.csv` – computed year(-1), year(0), year(+1) logs and differences
- **Results in paper:**
  - Table 7 – Yearly differences in taxable corporate income (real, log scale)
  - Figures 1–3 – descriptive plots by adoption timing

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
