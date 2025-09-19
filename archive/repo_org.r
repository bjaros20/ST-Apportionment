# inventory.R

# set seed
set.seed(26)

#Set Directory
setwd("~/Documents/GitHub/ST-Apportionment")


# inventory.R
dir.create("repo_audit", showWarnings = FALSE)
files <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = FALSE, no.. = TRUE)
# Drop .git internals from inventory
files <- files[!grepl("^\\.git(/|$)", files)]

get_git_last_commit <- function(path) {
  cmd <- sprintf('git log -1 --format="%%h|%%ad|%%an|%%s" --date=short -- "%s"', path)
  out <- tryCatch(system(cmd, intern = TRUE), error = function(e) NA_character_)
  if (length(out) == 0) return(NA_character_)
  parts <- strsplit(out, "\\|")[[1]]
  if (length(parts) < 4) return(NA_character_)
  data.frame(last_commit = parts[1], last_date = parts[2], last_author = parts[3], last_subject = parts[4])
}

rows <- lapply(files, function(f) {
  info <- file.info(f)
  ext  <- tools::file_ext(f)
  meta <- get_git_last_commit(f)
  data.frame(
    path = f,
    ext = ifelse(nzchar(ext), ext, NA),
    size_bytes = info$size,
    mtime = as.character(info$mtime),
    last_commit = meta$last_commit,
    last_date = meta$last_date,
    last_author = meta$last_author,
    last_subject = meta$last_subject,
    stringsAsFactors = FALSE
  )
})
inv <- do.call(rbind, rows)
write.csv(inv, "repo_audit/inventory.csv", row.names = FALSE)
message("Wrote repo_audit/inventory.csv")


# Now organizing the repo
install.packages("gert")  # if you don’t already have it
library(gert)

# ensure target dir exists
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)


# move the files
file.rename("CAEMP25N/CAEMP25N__ALL_AREAS_2001_2022.csv",
            "data/raw/CAEMP25N__ALL_AREAS_2001_2022.csv")

file.rename("CAEMP25N/bls_wide.csv",
            "data/raw/bls_wide.csv")

file.rename("CAEMP25N/bls_transpose.csv",
            "data/raw/bls_transpose.csv")

file.rename("CAEMP25N/CAEMP25N_MIC_2001_2022.csv",
            "data/raw/CAEMP25N_MIC_2001_2022.csv")

file.rename("CAEMP25N/CAEMP25N_MSA_2001_2022.csv",
            "data/raw/CAEMP25N_MSA_2001_2022.csv")

file.rename("CAEMP25N/CAEMP25N_TX_2001_2022.csv",
            "data/raw/CAEMP25N_TX_2001_2022.csv")

file.rename("CAEMP25N/CAEMP25N_CSA_2001_2022.csv",
            "data/raw/CAEMP25N_CSA_2001_2022.csv")

file.rename("CAEMP25N/CAEMP25N_PORT_2001_2022.csv",
            "data/raw/CAEMP25N_PORT_2001_2022.csv")

# job market data
dir.create("data/raw/job_market", recursive = TRUE, showWarnings = FALSE)

file.rename("job_market_data/real_log_nci.csv",
            "data/raw/job_market/real_log_nci.csv")

file.rename("job_market_data/real_NCI_cap.csv",
            "data/raw/job_market/real_NCI_cap.csv")

file.rename("job_market_data/naive_ci.csv",
            "data/raw/job_market/naive_ci.csv")

file.rename("2021.annual 01000 Alabama -- Statewide.numbers",
            "data/raw/2021_annual_Alabama_Statewide.numbers")


#Organize Notebooks
dir.create("notebooks", showWarnings = FALSE)

file.rename("Rates_Of_Change/Notebook_Rates_Of_Change.Rmd",
            "notebooks/Notebook_Rates_Of_Change.Rmd")

file.rename("SSFA_RevenueEffects.Rmd",
            "notebooks/SSFA_RevenueEffects.Rmd")

#Pngs, Maps
dir.create("figures", showWarnings = FALSE)

file.rename("ssf_adoption_map2.png", "figures/ssf_adoption_map2.png")
file.rename("ssf_adoption_map4.png", "figures/ssf_adoption_map4.png")
file.rename("ssf_adoption_map5.png", "figures/ssf_adoption_map5.png")


#General use R scripts
dir.create("scripts", showWarnings = FALSE)

file.rename("AttemptEffectEventsRevenue.R",
            "scripts/AttemptEffectEventsRevenue.R")

file.rename("AttemptReplicate_KyleButts_DiD.R",
            "scripts/AttemptReplicate_KyleButts_DiD.R")

file.rename("CAEMP25N/bls_filter.R",
            "scripts/bls_filter.R")

file.rename("CrossPartial/Code_Cross_Partial.R",
            "scripts/Code_Cross_Partial.R")

file.rename("CrossPartial/ElasticityTaxBase.R",
            "scripts/ElasticityTaxBase.R")

file.rename("Direct_Switch_DiD/Direct_Switch_First7.R",
            "scripts/Direct_Switch_First7.R")

file.rename("event_study.R",
            "scripts/event_study.R")

file.rename("Gardner_Butts_DiD.R",
            "scripts/Gardner_Butts_DiD.R")

# Processed Data
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

file.rename("CAEMP25N/.RData", "data/processed/CAEMP25N.RData")
file.rename("Spring24_Workshop/.RData", "data/processed/Spring24_Workshop.RData")
file.rename("CrossPartial/.RData", "data/processed/CrossPartial.RData")
file.rename("Direct_Switch_DiD/.RData", "data/processed/Direct_Switch_DiD.RData")
file.rename(".RData", "data/processed/root_session.RData")


#Take stock
list.files(".", recursive = TRUE)

# Just R scripts
list.files(".", recursive = TRUE, pattern = "\\.R$")


dir.create("scripts/job_market", recursive = TRUE, showWarnings = FALSE)

files_jobmarket <- list.files("job_market_data", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

for (f in files_jobmarket) {
  file.rename(f, file.path("scripts/job_market", basename(f)))
}

# workshop scripts
dir.create("scripts/workshop", recursive = TRUE, showWarnings = FALSE)

files_workshop <- list.files("Spring24_Workshop", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

for (f in files_workshop) {
  file.rename(f, file.path("scripts/workshop", basename(f)))
}

file.rename("Rates_Of_Change/RateOfChange_ElasticityTaxBase.R",
            "scripts/RateOfChange_ElasticityTaxBase.R")



# miscellaneous scripts
dir.create("scripts", showWarnings = FALSE)

misc_scripts <- c(
  "Clean_MySalesFactorWeights_10_3.R",
  "Clean_UpJonathan_BaseRevFile2_15_24.R",
  "CrossPartial/Cross_Calculator.R",
  "DiD_PreTrendCharts_2_16_Clean.R",
  "Direct_Switch_DiD/Direct_Switch_Through2007.R",
  "DTA_Giroud_Data.R",
  "Fraction_Nationwide_CIT_Rev.R",
  "GPT_PRE_WORKSHOP_SCIT.R",
  "InflationAdj.R",
  "Jonathan_DiD_PreTrendCharts_2_15_24.R",
  "Merge_CIT_Rate_Gir_TF_SSF.R",
  "MergeGirDifference.R",
  "MyAppt_Wide2Long_CompGir.R",
  "NebraskaCleanForDiD.R",
  "NebraskaDiD.R",
  "Plot_byRegion.R",
  "Plots_Sales_Jonathan_MTG_Mar1.R",
  "Plots_Sales_Weight_OverTime.R",
  "PopulationPerCapita_SCIT_Clean_Up.R",
  "QuantileRegression.R",
  "QuantRegPart2.R",
  "RealRevDiD_Success.R",
  "SCIT_RevenueEffect.R",
  "SCIT_RevenueEffectSUCCESS.R",
  "Set_UpDiD.R",
  "SimplePlots_DirectSwitch.R",
  "TF.R",
  "TwoWayFEReg.R"
)

for (f in misc_scripts) {
  if (file.exists(f)) {
    file.rename(f, file.path("scripts", basename(f)))
  }
}

# organize figures
dir.create("figures", showWarnings = FALSE)

files_figs <- list.files(".", recursive = TRUE,
                         pattern = "\\.(png|jpg|jpeg|pdf)$",
                         full.names = TRUE)

# Exclude known report-like PDFs later if needed
for (f in files_figs) {
  newpath <- file.path("figures", basename(f))
  file.rename(f, newpath)
}


# loose files to Raw data
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

files_data <- list.files(".", recursive = TRUE,
                         pattern = "\\.(csv|xlsx)$",
                         full.names = TRUE)

# Exclude anything already in data/raw or data/processed
files_data <- files_data[!grepl("^data/", files_data)]

for (f in files_data) {
  newpath <- file.path("data/raw", basename(f))
  file.rename(f, newpath)
}


#reports
dir.create("reports", showWarnings = FALSE)

files_reports <- list.files(".", recursive = TRUE,
                            pattern = "\\.(html|docx)$",
                            full.names = TRUE)

for (f in files_reports) {
  newpath <- file.path("reports", basename(f))
  file.rename(f, newpath)
}

#archive for history etc
dir.create("archive", showWarnings = FALSE)

hist_files <- c(".Rhistory", ".Rapp.history", ".RData", ".DS_Store")

for (f in hist_files) {
  if (file.exists(f)) {
    file.rename(f, file.path("archive", basename(f)))
  }
}



# Create replication folders for Section 2.4 and 2.5
dir.create("analysis/2.4_fixed_factors_logit/scripts", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/2.4_fixed_factors_logit/data", showWarnings = FALSE)
dir.create("analysis/2.4_fixed_factors_logit/output", showWarnings = FALSE)

dir.create("analysis/2.5_employment_probit/scripts", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/2.5_employment_probit/data", showWarnings = FALSE)
dir.create("analysis/2.5_employment_probit/output", showWarnings = FALSE)

# List everything in scripts and tables so we can identify matches
list.files("scripts", recursive = TRUE)

dirs <- c(
  "analysis/2.4_fixed_factors_logit/scripts",
  "analysis/2.4_fixed_factors_logit/output",
  "analysis/2.5_employment_hazard/scripts",
  "analysis/2.5_employment_hazard/output",
  "analysis/7.2_truncated_sample/scripts",
  "analysis/7.2_truncated_sample/output",
  "analysis/7.3_synthetic_did/scripts",
  "analysis/7.3_synthetic_did/output",
  "analysis/7.4_noncorp_spillovers/scripts",
  "analysis/7.4_noncorp_spillovers/output"
)

for (d in dirs) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# move Severance logit script
file.copy("scripts/job_market/Severance2WFE.R",
          "analysis/2.4_fixed_factors_logit/scripts/Severance2WFE.R")

file.copy("tables/logit_severance.csv",
          "analysis/2.4_fixed_factors_logit/output/logit_severance.csv")

file.copy("scripts/Direct_Switch_First7.R",
          "analysis/7.2_truncated_sample/scripts/Direct_Switch_First7.R")

file.copy("scripts/Set_UpDiD.R",
          "analysis/7.2_truncated_sample/scripts/Set_UpDiD.R")

file.copy("scripts/SimplePlots_DirectSwitch.R",
          "analysis/7.2_truncated_sample/scripts/SimplePlots_DirectSwitch.R")

file.copy("scripts/TwoWayFEReg.R",
          "analysis/7.2_truncated_sample/scripts/TwoWayFEReg.R")

list.files("analysis/2.4_fixed_factors_logit/scripts")
list.files("analysis/7.2_truncated_sample/scripts")


# TWFE and DiD scripts
dir.create("analysis/7.2_truncated_sample/scripts", recursive = TRUE, showWarnings = FALSE)

file.copy("scripts/job_market/disseration_TWFE_4_4_25.R",
          "analysis/7.2_truncated_sample/scripts/disseration_TWFE_4_4_25.R")

file.copy("scripts/job_market/DiD3_24_25_Plots.R",
          "analysis/7.2_truncated_sample/scripts/DiD3_24_25_Plots.R")

file.copy("scripts/job_market/DiD_3_3_25_Jonathan.R",
          "analysis/7.2_truncated_sample/scripts/DiD_3_3_25_Jonathan.R")


list.files(".", pattern = "disseration_TWFE_4_4_25.R", recursive = TRUE)
list.files(".", pattern = "DiD3_24_25_Plots.R", recursive = TRUE)
list.files(".", pattern = "DiD_3_3_25_Jonathan.R", recursive = TRUE)


# 7.3
dir.create("analysis/7.3_synthetic_did/scripts", recursive = TRUE, showWarnings = FALSE)

file.copy("scripts/job_market/SynDiD.R",
          "analysis/7.3_synthetic_did/scripts/SynDiD.R")

file.copy("scripts/job_market/synthetic_Control_attempt.R",
          "analysis/7.3_synthetic_did/scripts/synthetic_Control_attempt.R")

file.copy("scripts/job_market/loop_sDiD.R",
          "analysis/7.3_synthetic_did/scripts/loop_sDiD.R")

file.copy("scripts/job_market/pipeline_bootstrap_sDiD.R",
          "analysis/7.3_synthetic_did/scripts/pipeline_bootstrap_sDiD.R")

file.copy("scripts/job_market/retroactive_sDiD.R",
          "analysis/7.3_synthetic_did/scripts/retroactive_sDiD.R")

# 7.4
dir.create("analysis/7.4_non_corporate_income/scripts", recursive = TRUE, showWarnings = FALSE)

file.copy("scripts/job_market/pe_nonCI_SR_LR.R",
          "analysis/7.4_non_corporate_income/scripts/pe_nonCI_SR_LR.R")

file.copy("scripts/job_market/pe_result_IndInc_SR_LR.R",
          "analysis/7.4_non_corporate_income/scripts/pe_result_IndInc_SR_LR.R")

# Optionally include percent change plots
file.copy("scripts/job_market/per_chan_NCI.R",
          "analysis/7.4_non_corporate_income/scripts/per_chan_NCI.R")


dir.create("hist_files", showWarnings = FALSE)

file.rename("CrossPartial/.Rhistory", 
            "hist_files/CrossPartial_Rhistory.Rhistory")

file.rename("Direct_Switch_DiD/.Rhistory", 
            "hist_files/DirectSwitchDiD_Rhistory.Rhistory")


# checking script and data for section 2.4

file.edit("analysis/2.4_fixed_factors_logit/scripts/Severance2WFE.R")

candidates <- c(
  "filled_data_jmp.csv",
  "Non_CIT_states_FRED_OH.csv",
  "ssfa_data_jmp.xlsx",
  "clean_rates_1976-2022.csv",
  "rates_jmp.csv"
)

for (f in candidates) {
  cat("Searching for:", f, "\n")
  print(list.files("data/raw", pattern = f, recursive = TRUE, full.names = TRUE))
}

list.files("analysis/2.4_fixed_factors_logit", recursive = TRUE, full.names = TRUE)

#copy raw output
# Make sure subfolders exist
dir.create("analysis/2.4_fixed_factors_logit/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/2.4_fixed_factors_logit/output", recursive = TRUE, showWarnings = FALSE)

# List of raw input files with confirmed locations
raw_inputs <- c(
  "data/raw/filled_data_jmp.csv",
  "data/raw/Non_CIT_states_FRED_OH.csv",
  "data/raw/ssfa_data_jmp.xlsx",
  "data/raw/clean_rates_1976-2022.csv",
  "data/raw/rates_jmp.csv",
  "data/raw/elasticity_rates_jmp.csv"
)

# Copy each raw input into analysis/2.4/data
for (f in raw_inputs) {
  target <- file.path("analysis/2.4_fixed_factors_logit/data", basename(f))
  if (file.exists(f) && !file.exists(target)) {
    file.copy(f, target)
    message("✅ Copied: ", f, " → ", target)
  } else if (!file.exists(f)) {
    message("⚠️ Source missing: ", f)
  } else {
    message("ℹ️ Already exists: ", target)
  }
}

# Copy cleaned dataset if available
if (file.exists("Sev_early_switch.csv")) {
  target <- "analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv"
  if (!file.exists(target)) {
    file.copy("Sev_early_switch.csv", target)
    message("✅ Copied cleaned dataset: Sev_early_switch.csv → ", target)
  } else {
    message("ℹ️ Cleaned dataset already in output folder")
  }
} else {
  message("⚠️ Cleaned dataset Sev_early_switch.csv not found in root")
}

file.copy("data/raw/Sev_early_switch.csv",
          "analysis/2.4_fixed_factors_logit/output/Sev_early_switch.csv")



#section 2.5 repo organization
# Make sure Section 2.5 folder structure exists
dir.create("analysis/2.5_employment_probit/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/2.5_employment_probit/output", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/2.5_employment_probit/scripts", recursive = TRUE, showWarnings = FALSE)

# Copy Severance_Cap_mutate.csv from your main repo
sev_file <- "data/raw/job_market/Severance_Cap_mutate.csv"
if (file.exists(sev_file)) {
  file.copy(sev_file,
            "analysis/2.5_employment_probit/data/Severance_Cap_mutate.csv",
            overwrite = FALSE)
} else {
  message("⚠️ Could not find: ", sev_file)
}

list.files(".", pattern = "Severance_Cap_mutate.csv", recursive = TRUE, full.names = TRUE)

file.copy("data/raw/Severance_Cap_mutate.csv",
          "analysis/2.5_employment_probit/data/Severance_Cap_mutate.csv",
          overwrite = FALSE)



# Path to your external 'projects' folder
projects_path <- "/Users/ben/Documents/GitHub/projects"

# Files to copy from projects
project_files <- c(
  "BEA_GDPGrowth.csv",
  "CurrentEmploymentStatistics_National.csv",
  "CurrentEmploymentStatistics_States.csv",
  "LocalAreaUnemploymentStatistics_States.csv",
  "NBER_businesscyclechronology.csv",
  "Statewide_ManufacturingEmployment_States.csv"
)

for (f in project_files) {
  src <- file.path(projects_path, f)
  dest <- file.path("analysis/2.5_employment_probit/data", f)
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = FALSE)
    message("✅ Copied: ", f)
  } else {
    message("⚠️ Missing: ", src)
  }
}

list.files("/Users/ben/Documents/GitHub/projects", 
           pattern = "\\.csv$", 
           recursive = TRUE, 
           full.names = TRUE)


# Path to your Why States Switch repo
why_switch_path <- "/Users/benjaros/Documents/GitHub/projects/SSFA_Why_States_Switch/SSFA_switch_data"

# Files to copy
why_switch_files <- c(
  "BEA_GDPGrowth.csv",
  "CurrentEmploymentStatistics_National.csv",
  "CurrentEmploymentStatistics_States.csv",
  "LocalAreaUnemploymentStatistics_States.csv",
  "NBER_businesscyclechronology.csv",
  "StatewideManufacturingEmployment_States.csv"
)

for (f in why_switch_files) {
  src <- file.path(why_switch_path, f)
  dest <- file.path("analysis/2.5_employment_probit/data", f)
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = FALSE)
    message("✅ Copied: ", f)
  } else {
    message("⚠️ Missing: ", src)
  }
}

# move script
# Define source and destination
src_script <- "/Users/benjaros/Documents/GitHub/projects/SSFA_Why_States_Switch/SSFA_Why_Switch_Script.R"
dest_script <- "analysis/2.5_employment_probit/scripts/SSFA_Why_Switch_Script.R"

# Copy if it exists
if (file.exists(src_script)) {
  file.copy(src_script, dest_script, overwrite = FALSE)
  message("✅ Copied: ", src_script, " → ", dest_script)
} else {
  message("⚠️ Could not find script at: ", src_script)
}


# Expected files for Section 2.5
expected_files <- c(
  "Severance_Cap_mutate.csv",
  "BEA_GDPGrowth.csv",
  "CurrentEmploymentStatistics_National.csv",
  "CurrentEmploymentStatistics_States.csv",
  "LocalAreaUnemploymentStatistics_States.csv",
  "StatewideManufacturingEmployment_States.csv"
)

# Check what’s in the 2.5 data folder
actual_files <- list.files("analysis/2.5_employment_probit/data", full.names = FALSE)

# Report missing and extra files
missing <- setdiff(expected_files, actual_files)
extra   <- setdiff(actual_files, expected_files)

cat("✅ Present files:\n", paste(intersect(expected_files, actual_files), collapse = "\n"), "\n\n")
if (length(missing) > 0) cat("⚠️ Missing files:\n", paste(missing, collapse = "\n"), "\n\n")
if (length(extra) > 0) cat("ℹ️ Extra files in folder:\n", paste(extra, collapse = "\n"), "\n")


#organization for Section 7.1
dir.create("analysis/7.1_yearly_changes/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.1_yearly_changes/output", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.1_yearly_changes/scripts", recursive = TRUE, showWarnings = FALSE)


# Copy raw data
file.copy("data/raw/job_market/real_log_nci.csv",
          "analysis/7.1_yearly_changes/data/real_log_nci.csv")
list.files(".", pattern = "real_log_nci.csv", recursive = TRUE, full.names = TRUE)


# Copy raw data (real taxable corporate income, log)
file.copy("data/raw/real_log_nci.csv",
          "analysis/7.1_yearly_changes/data/real_log_nci.csv")


# Copy output (if already created by running script)
if (file.exists("data/raw/job_market/sr_descriptive_log_nci.csv")) {
  file.copy("data/raw/job_market/sr_descriptive_log_nci.csv",
            "analysis/7.1_yearly_changes/output/sr_descriptive_log_nci.csv")
}


# Search for the real_log_nci data file
list.files(".", pattern = "real_log_nci", recursive = TRUE, full.names = TRUE)

# Search for the desc_log_nci script
list.files(".", pattern = "desc_log_nci.R", recursive = TRUE, full.names = TRUE)

}


# onto 7.2
# Move SimplePlots_DirectSwitch.R back to general scripts folder
file.rename("analysis/7.2_truncated_sample/scripts/SimplePlots_DirectSwitch.R",
            "scripts/SimplePlots_DirectSwitch.R")

# Move TwoWayFEReg.R back to general scripts folder
file.rename("analysis/7.2_truncated_sample/scripts/TwoWayFEReg.R",
            "scripts/TwoWayFEReg.R")


list.files("analysis/7.2_truncated_sample/scripts")
list.files("scripts", pattern = "DirectSwitch|TwoWayFEReg")


# Ensure folder structure for Section 7.2
dir.create("analysis/7.2_truncated_sample/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.2_truncated_sample/output", recursive = TRUE, showWarnings = FALSE)

# Copy the raw dataset (real taxable corporate income, log-transformed)
file.copy("data/raw/real_log_nci.csv",
          "analysis/7.2_truncated_sample/data/real_log_nci.csv",
          overwrite = FALSE)

# Move/copy the main script into Section 7.2 scripts
file.copy("analysis/7.2_truncated_sample/scripts/disseration_TWFE_4_4_25.R",
          "analysis/7.2_truncated_sample/scripts/TWFE_DiD_EventStudy_2007.R",
          overwrite = FALSE)

message("✅ Section 7.2 setup complete: data and script organized")


# set up section 7.3
list.files(".", pattern = "real_log_nci.csv", recursive = TRUE, full.names = TRUE)

#find scripts
# Search for any R script mentioning 'real_log_nci.csv'
matches <- system("grep -rn 'real_log_nci.csv' --include=*.R .", intern = TRUE)
cat(matches, sep = "\n")



# Ensure folder structure exists
dir.create("analysis/7.3_synthetic_did/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.3_synthetic_did/scripts", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.3_synthetic_did/output", recursive = TRUE, showWarnings = FALSE)

# Copy dataset into Section 7.3 data folder
file.copy("data/raw/real_log_nci.csv",
          "analysis/7.3_synthetic_did/data/real_log_nci.csv",
          overwrite = FALSE)

# Copy the main script (rename for clarity)
file.copy("scripts/job_market/log_nci.R",
          "analysis/7.3_synthetic_did/scripts/log_nci.R",
          overwrite = FALSE)

# Create README.md for Section 7.3
readme_73 <- "# Section 7.3 – Synthetic DID

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
"

writeLines(readme_73, "analysis/7.3_synthetic_did/README.md")


# --- Section 7.4: Non-Corporate Income ---
# Create folder structure
dir.create("analysis/7.4_non_corporate_income/data", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.4_non_corporate_income/output", recursive = TRUE, showWarnings = FALSE)
dir.create("analysis/7.4_non_corporate_income/scripts", recursive = TRUE, showWarnings = FALSE)

# Copy raw input (naive_ci.csv)
if (file.exists("data/raw/naive_ci.csv")) {
  file.copy("data/raw/naive_ci.csv",
            "analysis/7.4_non_corporate_income/data/naive_ci.csv",
            overwrite = FALSE)
}

list.files(".", pattern = "pe_nonCI_SR_LR.R", recursive = TRUE, full.names = TRUE)




# Copy outputs (if script has been run)
output_files <- c("real_NCI_cap.csv",
                  "sDiD_point_estimate_list_NCI_short_run.csv",
                  "sDiD_point_estimate_list_NCI_long_run.csv")

for (f in output_files) {
  src <- file.path("data/raw", f)
  if (!file.exists(src)) src <- f   # fallback if saved in working dir
  if (file.exists(src)) {
    file.copy(src,
              file.path("analysis/7.4_non_corporate_income/output", basename(f)),
              overwrite = FALSE)
  }
}


#Create READ.ME for 7.4
# Create README.md for Section 7.4
readme_74 <- "# Section 7.4 – Non-Corporate Income

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
- Appendix figures: per-state short-run and long-run plots."

writeLines(readme_74, "analysis/7.4_non_corporate_income/README.md")


#final clean up
# Repo structure search script
# ----------------------------

# Load needed library
if (!requireNamespace("fs", quietly = TRUE)) {
  install.packages("fs")
}
library(fs)

# Define the root of your repository (change if needed)
repo_root <- "."

# List directories and files up to 3 levels deep
repo_tree <- fs::dir_tree(
  path = repo_root,
  recurse = TRUE,
  max_depth = 3
)

# Print structure
cat(repo_tree, sep = "\n")


# Repo organization script
# ------------------------

# Ensure folders exist
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/filtered", recursive = TRUE, showWarnings = FALSE)
dir.create("scripts", recursive = TRUE, showWarnings = FALSE)
dir.create("archive/misc", recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# --- Moves based on your plan ---

# Move CAEMP25N into data/raw
if (dir.exists("CAEMP25N")) {
  file.rename("CAEMP25N", "data/raw/CAEMP25N")
}

# Move Rates_Of_Change into scripts
if (dir.exists("Rates_Of_Change")) {
  file.rename("Rates_Of_Change", "scripts/Rates_Of_Change")
}

# Move Spring24_Workshop into scripts
if (dir.exists("Spring24_Workshop")) {
  file.rename("Spring24_Workshop", "scripts/Spring24_Workshop")
}

# Move job_market_data into data/filtered
if (dir.exists("job_market_data")) {
  file.rename("job_market_data", "data/filtered/job_market_data")
}

# Optional: log a message if a folder wasn’t found
check_and_message <- function(path) {
  if (!file.exists(path)) {
    message("Warning: ", path, " not found.")
  }
}

lapply(c("CAEMP25N", "Rates_Of_Change", "Spring24_Workshop", "job_market_data"), check_and_message)



# Repo organization script (round 2)
# ----------------------------------

# Ensure archive/misc exists
dir.create("archive/misc", recursive = TRUE, showWarnings = FALSE)

# 1. Move hist_files and notebooks into archive/misc
if (dir.exists("hist_files")) {
  file.rename("hist_files", "archive/misc/hist_files")
}
if (dir.exists("notebooks")) {
  file.rename("notebooks", "archive/misc/notebooks")
}

# 2. Move states_shapefile into figures/
if (dir.exists("states_shapefile")) {
  dir.create("figures/states_shapefile", recursive = TRUE, showWarnings = FALSE)
  file.rename("states_shapefile", "figures/states_shapefile")
}

# 3. Move reports into output/reports
if (dir.exists("reports")) {
  dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
  file.rename("reports", "output/reports")
}

# 4. Handle plot_pics
if (dir.exists("plot_pics")) {
  # Move .svg into figures/
  svgs <- list.files("plot_pics", pattern = "\\.svg$", full.names = TRUE)
  if (length(svgs) > 0) {
    file.copy(svgs, "figures", overwrite = TRUE)
  }
  
  # Move .py into scripts/
  pys <- list.files("plot_pics", pattern = "\\.py$", full.names = TRUE)
  if (length(pys) > 0) {
    file.copy(pys, "scripts", overwrite = TRUE)
  }
  
  # Remove original folder if empty
  unlink("plot_pics", recursive = TRUE, force = TRUE)
}

# Repo organization script (round 3)
# ----------------------------------

# 1. Move reports into archive/reports (instead of output)
if (dir.exists("reports")) {
  dir.create("archive/reports", recursive = TRUE, showWarnings = FALSE)
  file.rename("reports", "archive/reports")
}

# 2. Move repo_audit safely into archive/misc
if (dir.exists("repo_audit")) {
  dir.create("archive/misc", recursive = TRUE, showWarnings = FALSE)
  
  # Copy contents first for safety
  files_to_copy <- list.files("repo_audit", full.names = TRUE, recursive = TRUE)
  for (f in files_to_copy) {
    dest <- file.path("archive/misc/repo_audit", basename(f))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    file.copy(f, dest, overwrite = TRUE)
  }
  
  # Optionally: remove original folder (uncomment when ready)
  # unlink("repo_audit", recursive = TRUE, force = TRUE)
}

# Repo organization script (round 3 - with delete)
# ----------------------------------

# Repo organization script (round 3 - true move)
# ----------------------------------

# 1. Move reports into archive/reports
if (dir.exists("reports")) {
  dir.create("archive", recursive = TRUE, showWarnings = FALSE)
  file.rename("reports", "archive/reports")
}

# 2. Move repo_audit into archive/misc/repo_audit (true move)
if (dir.exists("repo_audit")) {
  dir.create("archive/misc", recursive = TRUE, showWarnings = FALSE)
  file.rename("repo_audit", "archive/misc/repo_audit")
}
