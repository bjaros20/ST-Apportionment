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


