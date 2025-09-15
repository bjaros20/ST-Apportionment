#Attempt to Run Gardner DiD
library(tidyr)
library(dplyr)
library(tidyverse)
install.packages("fixest")
library(fixest)
library(did2s)
install.packages("devtools")



#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/Spring24_Workshop")


# Read in DF
Rev <- read.csv("SALT_App_Merge_2024copy.csv")

Dates <-read.csv("Gardner_Year.csv")

Rates <-read.csv("StateCIT_Rates_1976_2014.csv")

#Start Cleaning, get Rev in Log and merge.

#For CIT
Rev <- Rev %>%
  mutate(Log_CORPINCTX=log(CORPINCTX))

Rev <- Rev %>%
  mutate(Log_CORPINCTX=ifelse(is.nan(Log_CORPINCTX),0,Log_CORPINCTX))

#For Ind IT
Rev <-Rev %>%
  

#Filter Revenue Year to Dates
Rev2 <- Rev %>%
  filter(c(year >=1976 & year <=2019))

#Create Rev2 DF that only has treatment and control states:

DataRevMerge <-semi_join(Rev2, Dates, by="state")

#Remove X Column
DataRevMerge <- DataRevMerge %>% 
  select(-X)

#Get rid of duplicate Rows and columns

# Eliminate duplicate rows
DataRevMerge_unique_rows <- unique(DataRevMerge)

# Eliminate duplicate columns
DataRevMerge_unique_columns <- unique(DataRevMerge, MARGIN = 2)

# Create a new dataframe 'DataRevMerge2' with unique rows and columns
DataRevMerge2 <- DataRevMerge_unique_columns[!duplicated(DataRevMerge_unique_columns), ]

# Check if any columns are now duplicates of other columns
duplicated_columns <- duplicated(DataRevMerge2) | duplicated(DataRevMerge2, fromLast = TRUE)
duplicated_column_names <- colnames(DataRevMerge2)[duplicated_columns]

if (length(duplicated_column_names) > 0) {
  cat("Duplicate columns found after eliminating duplicate rows:", paste(duplicated_column_names, collapse = ", "), "\n")
} else {
  cat("No duplicate columns found after eliminating duplicate rows.\n")
}

#now to merge the Date data back with the Data Rev Merge 2 data
merged_df <- full_join(DataRevMerge2, Dates, by = "state")

# Print the merged dataframe
print(merged_df)

#Save this CSV, has control, treatment, etc, and has years for treatment
write_csv(merged_df, "Spring24_Pub_Merged_Rev_Dates.csv")


# Create a Post Column
merged_df2 <- merged_df %>%
  group_by(state) %>%
  mutate(Post = ifelse(year_pass_intro > year, 0, 1)) %>%
  ungroup()

#Create a treatment column from the Post Column and the group column

merged_df3 <- merged_df2 %>%
  mutate(treatment = ifelse(Post == 1 & group == "t", 1, 0))

write_csv(merged_df3, "RevDates_TreatmentColumn.csv")

#Create a Relative Year column for the Event Study Plot
# Create rel_year column
merged_df3$rel_year <- merged_df3$year - merged_df3$year_pass_intro

#Create inf column, like Butts for control group
merged_df3$rel_year[grepl("c", merged_df3$group)] <- "inf"

#Run Gardner
TwoWayFE <- did2s(
  merged_df3,
  yname = "Log_CORPINCTX", first_stage = ~ 0 | state + year,
  second_stage = ~ i(treatment, ref = FALSE), treatment = "treatment",
  cluster_var = "state"
)
#> Running Two-stage Difference-in-Differences
#>  - first stage formula `~ 0 | state + year`
#>  - second stage formula `~ i(treat, ref = FALSE)`
#>  - The indicator variable that denotes when treatment is on is `treat`
#>  - Standard errors will be clustered by `state`
fixest::etable(TwoWayFE)
#>          Gardner Result               TwoWayFE
#Dependent Var.:   Log_CORPINCTX

#treatment = 1   0.0042 (0.0766)
#_______________ _______________
#S.E. type                Custom
#Observations              1,496
#R2                     -0.00064
#Adj. R2                -0.00064
#---
#  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>  
#

# I will also run it with not logged Revenue and Individual income tax:
#Run Gardner, Regular Revenue
TwoWayFE_RegRev <- did2s(
  merged_df3,
  yname = "CORPINCTX", first_stage = ~ 0 | state + year,
  second_stage = ~ i(treatment, ref = FALSE), treatment = "treatment",
  cluster_var = "state"
)
fixest::etable(TwoWayFE_RegRev)

#Gardner Individual Income Tax Revenue
TwoWayFE_IncTax <- did2s(
  merged_df3,
  yname = "INCTAX", first_stage = ~ 0 | state + year,
  second_stage = ~ i(treatment, ref = FALSE), treatment = "treatment",
  cluster_var = "state"
)
fixest::etable(TwoWayFE_IncTax)

#Log Invividual
TwoWayFE_IncTax_Log <- did2s(
  merged_df3,
  yname = "Log_INCTAX", first_stage = ~ 0 | state + year,
  second_stage = ~ i(treatment, ref = FALSE), treatment = "treatment",
  cluster_var = "state"
)
fixest::etable(TwoWayFE_IncTax_Log)


write_csv(merged_df3, "FinalGardner_AprilWorkshop24.csv")




#Create log Individual income, Sales, and Total Tax Revenue
merged_df3 <- merged_df3 %>%
  mutate(Log_INCTAX=log(INCTAX))

merged_df3 <- merged_df3 %>%
  mutate(Log_INCTAX=ifelse(is.nan(Log_INCTAX),0,Log_INCTAX))
#Sales Tax
merged_df3 <- merged_df3 %>%
  mutate(Log_SALESTAX=log(SALESTAX))

merged_df3 <- merged_df3 %>%
  mutate(Log_SALESTAX=ifelse(is.nan(Log_SALESTAX),0,Log_SALESTAX))
#Total Tax
merged_df3 <- merged_df3 %>%
  mutate(Log_TOTLTAX=log(TOTLTAX))

merged_df3 <- merged_df3 %>%
  mutate(Log_TOTLTAX=ifelse(is.nan(Log_TOTLTAX),0,Log_TOTLTAX))




#Got a result, now try Event Study DiD
# Event Study
es <- did2s(merged_df3,
            yname = "Log_CORPINCTX", first_stage = ~ 0 | state + year,
            second_stage = ~ i(rel_year, ref = c(-1, Inf)), treatment = "treatment",
            cluster_var = "state"
)

#Plot Event Study
fixest::iplot(es, main = "Event study: Staggered treatment", xlab = "Relative time to treatment", col = "steelblue", ref.line = -0.5)

iplot(es)



#Will try to create plot on our own
#First calculate the average Corp inc tax by year.
average_by_year <- merged_df3 %>%
  filter(group == "c") %>%
  group_by(year) %>%
  summarise(mean_log_CORPINCTX = mean(Log_CORPINCTX, na.rm = TRUE),
            se_log_CORPINCTX = sd(Log_CORPINCTX, na.rm = TRUE) / sqrt(n()))

#Plots Weren't working great.  Saving the estimate and going to get more plots from Callaway and Sant'Anna Approach

