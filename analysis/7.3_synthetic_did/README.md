# Section 7.3 – Synthetic DID

This folder replicates the synthetic difference-in-differences (sDiD) analysis of SSFA adoption using state-level corporate income data.

## Data
- `data/real_log_nci.csv`  
  State-level log of real taxable corporate income (base dataset for all sDiD analyses).

## Scripts
- `scripts/log_nci.R`  
  Main script that:
  - Filters state adoption cohorts sequentially.
  - Runs synthetic DID (short-run and long-run) using the `synthdid` package.
  - Produces point estimates, confidence intervals, t-statistics, p-values.
  - Generates plots for each treated state.

## Output
- `output/` (to be populated when the script is run)  
  - `*_sDiD_real_log_nci_short_run.png` – per-state short-run estimates.  
  - `*_sDiD_real_log_nci_long_run.png` – per-state long-run estimates.  

## Results in Paper
- Tables 11–13 (Synthetic DID estimates).  
- Appendix figures: per-state short-run and long-run plots, Tables 17-21.

