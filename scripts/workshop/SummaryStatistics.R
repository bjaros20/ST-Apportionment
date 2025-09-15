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


# Assuming 'Rev_filtered' is your filtered dataframe containing the groups 'c' and 't'

# Calculate summary statistics for group 'c' for the INCTAX column
summary_c_INCTAX <- Rev_filtered %>%
  filter(group == "c") %>%
  summarize(
    n_INCTAX = n(),
    mean_INCTAX_c = mean(INCTAX),
    median_INCTAX_c = median(INCTAX),
    sd_INCTAX_c = sd(INCTAX),
    min_INCTAX_c = min(INCTAX),
    max_INCTAX_c = max(INCTAX)
  )

# Calculate summary statistics for group 't' for the INCTAX column
summary_t_INCTAX <- Rev_filtered %>%
  filter(group == "t") %>%
  summarize(
    n_INCTAX = n(),
    mean_INCTAX_t = mean(INCTAX),
    median_INCTAX_t = median(INCTAX),
    sd_INCTAX_t = sd(INCTAX),
    min_INCTAX_t = min(INCTAX),
    max_INCTAX_t = max(INCTAX)
  )

# Calculate summary statistics for all observations for the INCTAX column
summary_overall_INCTAX <- Rev_filtered %>%
  summarize(
    n_INCTAX = n(),
    mean_INCTAX = mean(INCTAX),
    median_INCTAX = median(INCTAX),
    sd_INCTAX = sd(INCTAX),
    min_INCTAX = min(INCTAX),
    max_INCTAX = max(INCTAX)
  )

# Combine the summary statistics into a single dataframe for the INCTAX column
summary_df_INCTAX <- bind_rows(
  data.frame(Group = "c", summary_c_INCTAX),
  data.frame(Group = "t", summary_t_INCTAX),
  data.frame(Group = "Overall", summary_overall_INCTAX)
)

# Print the summary dataframe for the INCTAX column
print(summary_df_INCTAX)

# Repeat the process for the Log_INCTAX column and Log_CORPINCTX column similarly
# Calculate summary statistics for group 'c' for the Log_INCTAX column
# Calculate summary statistics for group 't' for the Log_INCTAX column
# Calculate summary statistics for all observations for the Log_INCTAX column
# Combine the summary statistics into a single dataframe for the Log_INCTAX column
# Print the summary dataframe for the Log_INCTAX column

# Calculate summary statistics for group 'c' for the Log_CORPINCTX column
# Calculate summary statistics for group 't' for the Log_CORPINCTX column
# Calculate summary statistics for all observations for the Log_CORPINCTX column
# Combine the summary statistics into a single dataframe for the Log_CORPINCTX column
# Print the summary dataframe for the Log_CORPINCTX column


