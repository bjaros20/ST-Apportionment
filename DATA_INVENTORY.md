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
  - `analysis/7.2_truncated_sample/data/real_log_nci.csv`  
    State-level log non-corporate income dataset, used for TWFE and DiD analyses.  

- **Scripts:**
  - `analysis/7.2_truncated_sample/scripts/TWFE_DiD_EventStudy_2007.R`  
    Estimates TWFE, simple DiD, and event-study style DiD for the 2007 adoption cohort.  

- **Results in paper:**
  - Tables 8–10 (TWFE, DiD, and Event-study estimates).  
  - Figures 4 and 8 (event-study plots for truncated sample).  
---

## Section 7.3 – Synthetic DID
- **Data files:**
  - `analysis/7.3_synthetic_did/data/real_log_nci.csv` – state-level log of real taxable corporate income (base dataset for all sDiD analyses)
- **Scripts:**
`analysis/7.3_synthetic_did/scripts/log_nci.R` – runs sequential cohort filtering, synthetic DID (short- and long-run), and generates per-state plots
- **Results in paper:**
Tables 11–13 (Synthetic DID estimates)
Appendix: per-state short-run and long-run plots, Tables 17–21

---

## Section 7.4 – Non-Corporate Income
- **Data files:**
- `analysis/7.4_non_corporate_income/data/real_NCI_cap.csv` – non-corporate income tax revenue per capita, derived from naive_ci.csv
- **Scripts:**
  - `analysis/7.4_non_corporate_income/scripts/pe_nonCI_SR_LR.R` – constructs NCI per capita dataset, runs sDiD (short- and long-run), and saves estimates/plots
- **Results in paper:**
  - Tables 22–24 (Synthetic DID estimates for non-corporate tax revenue)
