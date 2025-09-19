# Section 7.4 – Non-Corporate Income

This folder replicates the synthetic difference-in-differences (sDiD) analysis using **non-corporate income (NCI) tax revenue per capita** as the outcome.

## Data
- `data/real_NCI_cap.csv`  
  State-level non-corporate income tax revenue per capita.  
  - Derived from `naive_ci.csv` by subtracting corporate income revenue from total tax revenue, adjusting by CPI, and scaling per capita.

## Scripts
- `scripts/pe_nonCI_SR_LR.R`  
  Main script that:
  - Constructs the non-corporate income per capita dataset.
  - Runs synthetic DID (short-run and long-run) using the `synthdid` package.
  - Produces point estimates, confidence intervals, t-statistics, and p-values.
  - Saves per-state plots of treatment effects.


## Results in Paper
- Tables 22-24 (Synthetic DID estimates for non-corporate tax revenue).  
- Appendix figures: per-state short-run and long-run plots.
