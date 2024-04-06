#Summary statistics for April Workshop

#loaded Rev from Gardner Final csv


#Rev is loaded

library(dplyr)
# Replace NA values with 0 in specific columns
Rev2 <- Rev %>%
  mutate(
    INCTAX = replace(INCTAX, is.na(INCTAX), 0),
    Log_CORPINCTX = replace(Log_CORPINCTX, is.na(Log_CORPINCTX), 0),
    Log_INCTAX = replace(Log_INCTAX, is.na(Log_INCTAX), 0)
  )

Rev$INCTAX <- gsub("NA", "0", Rev$INCTAX)
Rev$Log_CORPINCTX <- gsub("NA", "0", Rev$Log_CORPINCTX)
Rev$Log_INCTAX <- gsub("NA", "0", Rev$Log_INCTAX)
# Assuming 'Rev' is your dataframe containing the groups 'c' and 't'

Rev_filtered <- Rev[Rev$INCTAX != 0, ]


summary_stats_INCTAX <- Rev_filtered %>%
  summarize(
    n = n(),
    mean = mean(INCTAX, na.rm = TRUE),
    median = median(INCTAX, na.rm = TRUE),
    sd = sd(INCTAX, na.rm = TRUE),
    min = min(INCTAX, na.rm = TRUE),
    max = max(INCTAX, na.rm = TRUE)
  )

print(summary_stats_INCTAX)



write.csv(Rev_filtered,"SummaryStats_Sheet_4_6_24.csv")


# Replace NAs with zeros for 'CORPINCTX'
Rev$CORPINCTX <- replace(Rev$CORPINCTX, is.na(Rev$CORPINCTX), 0)

# Calculate summary statistics for group 'c'
summary_c_CORPINCTX <- Rev %>%
  filter(group == "c") %>%
  summarize(
    n = n(),
    mean_CORPINCTX = mean(CORPINCTX),
    median_CORPINCTX = median(CORPINCTX),
    sd_CORPINCTX = sd(CORPINCTX),
    min_CORPINCTX = min(CORPINCTX),
    max_CORPINCTX = max(CORPINCTX)
  )

# Repeat the process for other columns

# Replace NAs with zeros for 'Log_CORPINCTX'
Rev$Log_CORPINCTX <- replace(Rev$Log_CORPINCTX, is.na(Rev$Log_CORPINCTX), 0)

# Calculate summary statistics for group 'c'
summary_c_Log_CORPINCTX <- Rev %>%
  filter(group == "c") %>%
  summarize(
    n = n(),
    mean_Log_CORPINCTX = mean(Log_CORPINCTX),
    median_Log_CORPINCTX = median(Log_CORPINCTX),
    sd_Log_CORPINCTX = sd(Log_CORPINCTX),
    min_Log_CORPINCTX = min(Log_CORPINCTX),
    max_Log_CORPINCTX = max(Log_CORPINCTX)
  )

# Repeat the process for other columns

# Replace NAs with zeros for 'INCTAX'
Rev$INCTAX <- replace(Rev$INCTAX, is.na(Rev$INCTAX), 0)

# Calculate summary statistics for group 'c'
summary_c_INCTAX <- Rev %>%
  filter(group == "c") %>%
  summarize(
    n = n(),
    mean_INCTAX = mean(INCTAX),
    median_INCTAX = median(INCTAX),
    sd_INCTAX = sd(INCTAX),
    min_INCTAX = min(INCTAX),
    max_INCTAX = max(INCTAX)
  )

# Repeat the process for other columns

# Replace NAs with zeros for 'Log_INCTAX'
Rev$Log_INCTAX <- replace(Rev$Log_INCTAX, is.na(Rev$Log_INCTAX), 0)

# Calculate summary statistics for group 'c'
summary_c_Log_INCTAX <- Rev %>%
  filter(group == "c") %>%
  summarize(
    n = n(),
    mean_Log_INCTAX = mean(Log_INCTAX),
    median_Log_INCTAX = median(Log_INCTAX),
    sd_Log_INCTAX = sd(Log_INCTAX),
    min_Log_INCTAX = min(Log_INCTAX),
    max_Log_INCTAX = max(Log_INCTAX)
  )

# Print the summary dataframes
print(summary_c_CORPINCTX)
print(summary_c_Log_CORPINCTX)
print(summary_c_INCTAX)
print(summary_c_Log_INCTAX)


#For just individual income tax
Rev_filtered <- Rev %>%
  filter(INCTAX != 0)


# Calculate summary statistics for group 'c' after replacing NAs with 0
summary_c_INCTAX <- Rev %>%
  filter(group == "c") %>%
  summarize(
    n = n(),
    mean_INCTAX = mean(INCTAX),
    median_INCTAX = median(INCTAX),
    sd_INCTAX = sd(INCTAX),
    min_INCTAX = min(INCTAX),
    max_INCTAX = max(INCTAX)
  )

# Repeat the process for group 't'
summary_t_INCTAX <- Rev %>%
  filter(group == "t") %>%
  summarize(
    n = n(),
    mean_INCTAX = mean(INCTAX),
    median_INCTAX = median(INCTAX),
    sd_INCTAX = sd(INCTAX),
    min_INCTAX = min(INCTAX),
    max_INCTAX = max(INCTAX)
  )

# Calculate summary statistics for all observations
summary_overall_INCTAX <- Rev %>%
  summarize(
    n = n(),
    mean_INCTAX = mean(INCTAX),
    median_INCTAX = median(INCTAX),
    sd_INCTAX = sd(INCTAX),
    min_INCTAX = min(INCTAX),
    max_INCTAX = max(INCTAX)
  )

# Combine the summary statistics into a single dataframe for 'INCTAX'
summary_df_INCTAX <- bind_rows(
  data.frame(Group = "c", summary_c_INCTAX),
  data.frame(Group = "t", summary_t_INCTAX),
  data.frame(Group = "Overall", summary_overall_INCTAX)
)

# Print the summary dataframe for 'INCTAX'
print(summary_df_INCTAX)
