# Using 3-3-25 as a guide, I am going to estimate the log CI tax revenue, log NonCI
#tax revenue, and proxy log CI results using the TWFE, DiD, event-study DiD
# I will estimate these for 2006/2007, 2006 alone, and 2007 alone
# with 2015-2018 as the control group.

# Load required libraries
library(tidyr)
library(dplyr)
library(fixest)
library(plm)
library(lmtest)
library(sandwich)

# Set working directory (modify as needed)
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

# Load data
data <- read.csv("real_log_nci.csv")



#Estimate Simple TWFE with GA and WI switchers.
data_2006_sim <- data %>%
  filter(year_effective %in% c(2006, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    PostTreatment = ifelse(State_Acronym %in% c("GA", "WI") & year >= 2006, 1, 0)
  )

twfe_lm <- lm(logRealCitRev ~ PostTreatment + factor(State_Acronym) + factor(year), data = data_2006_sim)
summary(twfe_lm)



# --- Filter for 2006 and late switchers (GA, WI), from 1990 to 2014
data_2006 <- data %>%
  filter(year_effective %in% c(2006, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    PostTreatment = ifelse(State_Acronym %in% c("GA", "WI") & year >= 2006, 1, 0)
  )

model_twfe_2006 <- feols(logRealCitRev ~ PostTreatment | State_Acronym + year, data = data_2006)
summary(model_twfe_2006)

#PostTreatment -0.197228   0.093319 -2.11347 0.060686 .  
#same result for both regressions.





# Simple DiD
data_did_2006 <- data %>%
  filter(year_effective %in% c(2006, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    Treated = ifelse(year_effective == 2006, 1, 0),
    Post = ifelse(year >= 2006, 1, 0),
    DiD = Treated * Post
  )

model_did_2006 <- lm(logRealCitRev ~ Treated + Post + DiD, data = data_did_2006)
summary(model_did_2006)


# Think you have accurately estimated TWFE-no cluser, TWFE-state cluser, Simple DiD for logRealCitRev
# will need to return to this later. Estimates saved in chat. Going to Chesapeake




# --- Filter for 2007 and late switchers (SC, PA, MN, ME, IN, AZ), from 1990 to 2014
data_2007 <- data %>%
  filter(year_effective %in% c(2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    Treated = ifelse(year_effective == 2007, 1, 0),
    Post = ifelse(year >= year_effective, 1, 0),
    DiD = Treated * Post
  )

# --- TWFE and Simple DiD
model_twfe_2007 <- feols(logRealCitRev ~ Post | State_Acronym + year, data = data_2007)
model_did_2007  <- feols(logRealCitRev ~ Treated + Post + DiD, data = data_2007)


# --- Filter for 2006 + 2007 and late switchers, from 1990 to 2014
data_0607 <- data %>%
  filter(year_effective %in% c(2006, 2007, 2015, 2016, 2017, 2018),
         year >= 1990, year <= 2014) %>%
  mutate(
    Treated = ifelse(year_effective %in% c(2006, 2007), 1, 0),
    Post = ifelse(year >= year_effective, 1, 0),
    DiD = Treated * Post
  )

# --- TWFE and Simple DiD
model_twfe_0607 <- feols(logRealCitRev ~ Post | State_Acronym + year, data = data_0607)
model_did_0607  <- feols(logRealCitRev ~ Treated + Post + DiD, data = data_0607)


# --- Event-Study Models (ref = year prior to treatment)
#create indicators for treated

event_study_2006 <- feols(
  logRealCitRev ~ i(year, Treated, ref = 2005) | State_Acronym + year,
  data = data_2006,
  cluster = ~State_Acronym
)

event_study_2007 <- feols(
  logRealCitRev ~ i(year, Treated, ref = 2006) | State_Acronym + year,
  data = data_2007,
  cluster = ~State_Acronym
)

event_study_0607 <- feols(
  logRealCitRev ~ i(year, Treated, ref = 2006) | State_Acronym + year,
  data = data_0607,
  cluster = ~State_Acronym
)


# TWFE
summary(model_twfe_0607, cluster = "State_Acronym")
# Simple DiD
summary(model_did_0607)
# Event Study
summary(event_study_0607, cluster = "State_Acronym")



# TWFE
summary(model_twfe_2006, cluster = "State_Acronym")
# Simple DiD
summary(model_did_2006)
# Event Study
summary(event_study_2006, cluster = "State_Acronym")


# TWFE
summary(model_twfe_2007, cluster = "State_Acronym")
# Simple DiD
summary(model_did_2007)
# Event Study
summary(event_study_2007, cluster = "State_Acronym")



#now estimate the other NCR and proxy CI for 06.07
# --- TWFE model for Non-Corporate Tax Revenue
model_twfe_0607_log_RNCTR <- feols(log_RNCTR ~ Post | State_Acronym + year,
                                   data = data_0607,
                                   cluster = ~State_Acronym)

# --- Simple DiD model for Non-Corporate Tax Revenue
model_did_0607_log_RNCTR <- feols(log_RNCTR ~ Treated + Post + DiD,
                                  data = data_0607)

# --- TWFE model for Proxy Corporate Income
model_twfe_0607_real_log_nci <- feols(real_log_nci ~ Post | State_Acronym + year,
                                      data = data_0607,
                                      cluster = ~State_Acronym)

# --- Simple DiD model for Proxy Corporate Income
model_did_0607_real_log_nci <- feols(real_log_nci ~ Treated + Post + DiD,
                                     data = data_0607)

# Summary for TWFE model: log_RNCTR (Non-Corporate Tax Revenue)
summary(model_twfe_0607_log_RNCTR, cluster = "State_Acronym")

# Summary for Simple DiD model: log_RNCTR
summary(model_did_0607_log_RNCTR)

# Summary for TWFE model: real_log_nci (Proxy Corporate Income)
summary(model_twfe_0607_real_log_nci, cluster = "State_Acronym")

# Summary for Simple DiD model: real_log_nci
summary(model_did_0607_real_log_nci)


### NOW PLOTS FOR 06.07
library(fixest)
library(ggplot2)
library(dplyr)
library(broom)

# Define outcomes and labels
outcomes <- c("logRealCitRev", "log_RNCTR", "real_log_nci")
titles <- c(
  "Log Corporate Tax Revenue",
  "Log Non-Corporate Tax Revenue",
  "Log Proxy Corporate Income"
)

# Loop through each outcome
for (i in seq_along(outcomes)) {
  
  outcome <- outcomes[i]
  title <- titles[i]
  treatment_year <- 2006
  group <- "Treated vs. Control"
  
  # ---- Step 1: Treated vs. Control Average Plot ----
  plot_data <- data_0607 %>%
    mutate(Treatment = ifelse(year_effective %in% c(2006, 2007), "Treated", "Control")) %>%
    group_by(year, Treatment) %>%
    summarise(Mean = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  p1 <- ggplot(plot_data, aes(x = year, y = Mean, color = Treatment)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "black", size = 1) +
    labs(
      title = paste(group, ":", title),
      x = "Year",
      y = paste("Average", title)
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("0607_trend_", outcome, ".png"),
    plot = p1, width = 6, height = 4, units = "in", dpi = 300
  )
  
  # ---- Step 2: Event Study Plot ----
  model <- feols(
    as.formula(paste0(outcome, " ~ i(year, Treated, ref = 2006) | State_Acronym + year")),
    data = data_0607,
    cluster = ~State_Acronym
  )
  
  coef_df <- tidy(model, conf.int = TRUE) %>%
    filter(grepl("year::", term)) %>%
    mutate(
      year = as.numeric(gsub("year::", "", gsub(":Treated", "", term))),
      sig = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      )
    )
  
  p2 <- ggplot(coef_df, aes(x = year, y = estimate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
    geom_vline(xintercept = 2006, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      title = paste("Event Study:", title),
      x = "Year",
      y = "Coefficient Estimate"
    ) +
    theme_minimal()
  
  ggsave(
    filename = paste0("0607_event_", outcome, ".png"),
    plot = p2, width = 6, height = 4, units = "in", dpi = 300
  )
}

