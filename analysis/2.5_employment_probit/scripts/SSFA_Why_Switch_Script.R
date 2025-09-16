# load Packages
library(dplyr)
library(readr)
library(stringr)
library(broom)
library(broom.helpers)   # nice label helpers (optional)
library(sandwich)        # robust/clustered vcov
library(lmtest) # coeftest
library(texreg)          # LaTeX table
# or: library(modelsummary)




# ------------------------------
# 1) Load the "all states" base
# ------------------------------
sev <- read_csv("/Users/ben/Documents/GitHub/ST-Apportionment/job_market_data/Severance_Cap_mutate.csv")

#save in this folder
write.csv(sev,"severance_reg")


# Expect columns: state, year, year_effective, post_eff, etc.

# create 'adopt_year' = first year_effective per state
sev1 <- sev %>%
  group_by(state) %>%
  mutate(adopt_year = ifelse(year_effective > 0, year_effective, NA_integer_),
         adopt_year = min(adopt_year, na.rm = TRUE)) %>%
  mutate(adopt_year = ifelse(is.infinite(adopt_year), NA_integer_, adopt_year)) %>%
  ungroup()

# ------------------------------
# 2) Build discrete-time hazard panel
# ------------------------------
# Outcome adopt_it = 1 only in the first adoption year; 0 otherwise (while at risk)
hazard <- sev1 %>%
  mutate(adopt_it = as.integer(!is.na(adopt_year) & year == adopt_year)) %>%
  # keep only years up to adoption year; if never adopts, keep all years
  filter(is.na(adopt_year) | year <= adopt_year)

# baseline hazard via year FE
hazard <- hazard %>%
  mutate(year_f = factor(year))

# ------------------------------
# 3) Load Datasets for regressions

#BEA_GDPGrowth.csv 

BEA_gdp <- read.csv("/Users/ben/Documents/GitHub/projects/BEA_GDPGrowth.csv", skip =14)

#CurrentEmploymentStatistics_National.csv

CES_N <- read.csv("/Users/ben/Documents/GitHub/projects/CurrentEmploymentStatistics_National.csv", skip =13)

#CurrentEmploymentStatistics_States.csv 

CES_S <- read.csv("/Users/ben/Documents/GitHub/projects/CurrentEmploymentStatistics_States.csv", skip =12)
# Need to create an annual average, then log it.

#LocalAreaUnemploymentStatistics_States.csv 
LAUS <- read.csv("/Users/ben/Documents/GitHub/projects/LocalAreaUnemploymentStatistics_States.csv", skip =11)
# Need to create an annual average, then log it.

#NBER_businesscyclechronology.csv 
NBER <- read.csv("/Users/ben/Documents/GitHub/projects/NBER_businesscyclechronology.csv")
# will need to transform this into an annual time series, might hold off on integrating this.

#Statewide_ManufacturingEmployment_States.csv
Man_Emp <- read.csv("/Users/ben/Documents/GitHub/projects/StatewideManufacturingEmployment_States.csv", skip = 14)
# Will need to create an annual number and log it.


#Now need script that does this annual averaging, then adds columns to log that value.
# Clean CES_N data
CES_N_clean <- CES_N %>%
  mutate(
    annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    log_annual_avg = log(annual_avg)
  ) %>%
  select(Year, annual_avg, log_annual_avg)


# CES_S (State-level Current Employment Statistics)
CES_S_clean <- CES_S %>%
  mutate(
    annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    log_annual_avg = log(annual_avg)
  ) %>%
  select(State, Year, annual_avg, log_annual_avg)

# LAUS (State-level Unemployment Statistics)
LAUS_clean <- LAUS %>%
  mutate(
    annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    log_annual_avg = log(annual_avg)
  ) %>%
  select(State, Year, annual_avg, log_annual_avg)

# Man_Emp (Statewide Manufacturing Employment)
Man_Emp_clean <- Man_Emp %>%
  mutate(
    annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    log_annual_avg = log(annual_avg)
  ) %>%
  select(State, Year, annual_avg, log_annual_avg)


#Rename before merge
# CES National (merge on Year only)
CES_N_clean <- CES_N %>%
  mutate(
    cesN_annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    cesN_log_avg    = log(cesN_annual_avg)
  ) %>%
  select(Year, cesN_annual_avg, cesN_log_avg)

# CES State-level
CES_S_clean <- CES_S %>%
  mutate(
    cesS_annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    cesS_log_avg    = log(cesS_annual_avg)
  ) %>%
  select(State, Year, cesS_annual_avg, cesS_log_avg)

# LAUS State-level
LAUS_clean <- LAUS %>%
  mutate(
    laus_annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    laus_log_avg    = log(laus_annual_avg)
  ) %>%
  select(State, Year, laus_annual_avg, laus_log_avg)

# Manufacturing Employment State-level
Man_Emp_clean <- Man_Emp %>%
  mutate(
    manuf_annual_avg = rowMeans(select(., Jan:Dec), na.rm = TRUE),
    manuf_log_avg    = log(manuf_annual_avg)
  ) %>%
  select(State, Year, manuf_annual_avg, manuf_log_avg)




# Ensure join keys match (state naming!). If manuf uses abbreviations, build a crosswalk.
panel <- hazard %>%
  # National CES: join only on Year
  left_join(CES_N_clean, by = c("year" = "Year")) %>%
  # State-level merges
  left_join(CES_S_clean, by = c("state" = "State", "year" = "Year")) %>%
  left_join(LAUS_clean,  by = c("state" = "State", "year" = "Year")) %>%
  left_join(Man_Emp_clean, by = c("state" = "State", "year" = "Year"))



# lags 
panel1 <- panel %>%
  group_by(state) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # Lags for each predictor (one-year lag)
    cesN_annual_avg_l1   = lag(cesN_annual_avg, 1),
    cesN_log_avg_l1      = lag(cesN_log_avg, 1),
    cesS_annual_avg_l1   = lag(cesS_annual_avg, 1),
    cesS_log_avg_l1      = lag(cesS_log_avg, 1),
    laus_annual_avg_l1   = lag(laus_annual_avg, 1),
    laus_log_avg_l1      = lag(laus_log_avg, 1),
    manuf_annual_avg_l1  = lag(manuf_annual_avg, 1),
    manuf_log_avg_l1     = lag(manuf_log_avg, 1)
  ) %>%
  ungroup()

write.csv(panel1,"/Users/ben/Documents/GitHub/projects/merged_state_panel.csv", row.names = FALSE)



# More cleaning of the panel
panel1 <- panel1 %>%
  mutate(
    # ensure annual avgs non-negative -> NA if nonpositive
    cesN_annual_avg = ifelse(cesN_annual_avg > 0, cesN_annual_avg, NA_real_),
    cesS_annual_avg = ifelse(cesS_annual_avg > 0, cesS_annual_avg, NA_real_),
    laus_annual_avg = ifelse(laus_annual_avg > 0, laus_annual_avg, NA_real_),
    manuf_annual_avg = ifelse(manuf_annual_avg > 0, manuf_annual_avg, NA_real_),
    
    # recompute logs (no epsilon, per your preference—zeros become NA)
    cesN_log_avg   = ifelse(!is.na(cesN_annual_avg), log(cesN_annual_avg), NA_real_),
    cesS_log_avg   = ifelse(!is.na(cesS_annual_avg), log(cesS_annual_avg), NA_real_),
    laus_log_avg   = ifelse(!is.na(laus_annual_avg), log(laus_annual_avg), NA_real_),
    manuf_log_avg  = ifelse(!is.na(manuf_annual_avg), log(manuf_annual_avg), NA_real_)
  )

# Function to get summary stats for a pair of vars (annual & log)
summarize_predictor <- function(df, annual_col, log_col, name){
  df %>%
    summarise(
      N              = n(),
      Mean_annual    = mean(.data[[annual_col]], na.rm = TRUE),
      SD_annual      = sd(.data[[annual_col]], na.rm = TRUE),
      Min_annual     = min(.data[[annual_col]], na.rm = TRUE),
      Max_annual     = max(.data[[annual_col]], na.rm = TRUE),
      Mean_log       = mean(.data[[log_col]], na.rm = TRUE),
      SD_log         = sd(.data[[log_col]], na.rm = TRUE),
      Min_log        = min(.data[[log_col]], na.rm = TRUE),
      Max_log        = max(.data[[log_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Variable = name) %>%
    select(Variable, everything())
}

# Run for each predictor
cesN_stats <- summarize_predictor(panel1, "cesN_annual_avg", "cesN_log_avg", "CES National")
cesS_stats <- summarize_predictor(panel1, "cesS_annual_avg", "cesS_log_avg", "CES State")
laus_stats <- summarize_predictor(panel1, "laus_annual_avg", "laus_log_avg", "LAUS Unemployment")
manuf_stats <- summarize_predictor(panel1, "manuf_annual_avg", "manuf_log_avg", "Manufacturing")

# Combine into one summary table
summary_table <- bind_rows(cesN_stats, cesS_stats, laus_stats, manuf_stats)

summary_table

panel1 <- panel1 %>% dplyr::select(-1)


panel1 <- panel1 %>%
  mutate(
    adopt_it = ifelse(year == year_effective & year_effective > 0, 1, 0),
    adopt_it = ifelse(is.na(adopt_it), 0, adopt_it)
  )

# Restrict hazard set to pre-adoption years and the adoption year
hazard <- panel1 %>%
  filter(is.na(year_effective) | year <= year_effective)



# Rebuild hazard panel.

library(dplyr); library(broom); library(sandwich); library(lmtest)

# 1) Make sure year_f exists
hazard <- hazard %>% mutate(year_f = factor(year))

# 2) Rebuild lagged LOG predictors fresh (by state), so no stale NA/Inf
hazard <- hazard %>%
  group_by(state) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    cesN_log_avg_l1  = dplyr::lag(cesN_log_avg, 1),
    cesS_log_avg_l1  = dplyr::lag(cesS_log_avg, 1),
    laus_log_avg_l1  = dplyr::lag(laus_log_avg, 1),
    manuf_log_avg_l1 = dplyr::lag(manuf_log_avg, 1)
  ) %>%
  ungroup()

# 3) Safe runner: drop incomplete rows for the current model, cluster by state
vcov_state <- function(m, data) sandwich::vcovCL(m, cluster = data$state)

run_hazard <- function(data, xvar) {
  vars <- c("adopt_it","year_f", xvar)
  dat <- data %>%
    mutate(across(all_of(vars), ~ ifelse(is.infinite(.), NA, .))) %>%
    tidyr::drop_na(all_of(vars))
  stopifnot(length(unique(dat$adopt_it)) == 2)
  fit <- glm(reformulate(c(xvar,"year_f"), "adopt_it"), data = dat, family = binomial())
  broom::tidy(fit, conf.int = TRUE, vcov = vcov_state(fit, dat)) %>%
    mutate(odds_ratio = exp(estimate),
           or_lo = exp(conf.low), or_hi = exp(conf.high),
           predictor = xvar, n_obs = nrow(dat))
}

# 4) Estimate
m_cesN_tidy  <- run_hazard(hazard, "cesN_log_avg_l1")
m_cesS_tidy  <- run_hazard(hazard, "cesS_log_avg_l1")
m_laus_tidy  <- run_hazard(hazard, "laus_log_avg_l1")
m_manuf_tidy <- run_hazard(hazard, "manuf_log_avg_l1")

# Quick peek at the key terms
m_cesN_tidy %>% dplyr::filter(term == "cesN_log_avg_l1")
m_cesS_tidy %>% dplyr::filter(term == "cesS_log_avg_l1")
m_laus_tidy  %>% dplyr::filter(term == "laus_log_avg_l1")
m_manuf_tidy %>% dplyr::filter(term == "manuf_log_avg_l1")


# --- 4) PROBIT robustness ---
# --- PROBIT robustness checks (skip CES-N) ---

m_cesS_pb  <- glm(adopt_it ~ cesS_log_avg_l1 + year_f,
                  data = hazard, family = binomial(link = "probit"))

m_laus_pb  <- glm(adopt_it ~ laus_log_avg_l1 + year_f,
                  data = hazard, family = binomial(link = "probit"))

m_manuf_pb <- glm(adopt_it ~ manuf_log_avg_l1 + year_f,
                  data = hazard, family = binomial(link = "probit"))

# Tidy output with clustered SEs
m_cesS_pb_tidy  <- broom::tidy(m_cesS_pb,  conf.int = TRUE, vcov = vcov_state(m_cesS_pb,  hazard))
m_laus_pb_tidy  <- broom::tidy(m_laus_pb,  conf.int = TRUE, vcov = vcov_state(m_laus_pb,  hazard))
m_manuf_pb_tidy <- broom::tidy(m_manuf_pb, conf.int = TRUE, vcov = vcov_state(m_manuf_pb, hazard))

# Quick peek at the key terms
m_cesS_pb_tidy  %>% dplyr::filter(term == "cesS_log_avg_l1")
m_laus_pb_tidy  %>% dplyr::filter(term == "laus_log_avg_l1")
m_manuf_pb_tidy %>% dplyr::filter(term == "manuf_log_avg_l1")



# --- 5) (Optional) Combined table with odds ratios for logit, hide year FE ---
# modelsummary::msummary(
#   list("CES Nat (logit)"  = m_cesN,  "CES State (logit)" = m_cesS,
#        "Unemp (logit)"    = m_laus,  "Manufacturing (logit)" = m_manuf),
#   vcov = list(vcov_state(m_cesN, hazard), vcov_state(m_cesS, hazard),
#               vcov_state(m_laus, hazard), vcov_state(m_manuf, hazard)),
#   exponentiate = TRUE,  # odds ratios
#   coef_omit = "^year_f",
#   gof_omit = "AIC|BIC"
# )

# ------------------------------
# 5) LaTeX-ready table
# ------------------------------
library(modelsummary)
library(sandwich)
library(broom)

# LOGIT models (main spec; odds ratios)
m_cesS_logit  <- glm(adopt_it ~ cesS_log_avg_l1  + year_f, data = hazard, family = binomial())
m_laus_logit  <- glm(adopt_it ~ laus_log_avg_l1  + year_f, data = hazard, family = binomial())
m_manuf_logit <- glm(adopt_it ~ manuf_log_avg_l1 + year_f, data = hazard, family = binomial())

vcovs_logit <- list(
  sandwich::vcovCL(m_cesS_logit,  cluster = hazard$state),
  sandwich::vcovCL(m_laus_logit,  cluster = hazard$state),
  sandwich::vcovCL(m_manuf_logit, cluster = hazard$state)
)

modelsummary(
  list("CES-S (logit)" = m_cesS_logit,
       "LAUS (logit)" = m_laus_logit,
       "Manufacturing (logit)" = m_manuf_logit),
  vcov = vcovs_logit,
  exponentiate = TRUE,                # shows Odds Ratios
  coef_map = c(
    "cesS_log_avg_l1"   = "Lag log State Employment",
    "laus_log_avg_l1"   = "Lag log Unemployment",
    "manuf_log_avg_l1"  = "Lag log Manufacturing"
  ),
  coef_omit = "^year_f",              # hide year FEs
  gof_omit  = "AIC|BIC",
  stars = TRUE,
  output = "latex"
)
