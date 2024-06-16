#dougan just wants an estimation of the total revenue
#What approach should be used?

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(readxl) #load ssfa switch date
library(did) # for running DiD
library(caret) 
library(plm) # for Two way FE
library(fixest) # multiple FE
library(broom) #extract coefficients for each state


#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load files
Rev <- read.csv("filled_data_jmp.csv")

Log_rev <- read.csv("filled_log_data_jmp.csv")

Date <- read_xlsx("ssfa_data_jmp.xlsx")

#remove the X column, must be something that happens when saving CSV
Rev <-Rev %>%
  select(-X)
Log_rev <- Log_rev %>%
  select(-X)

#Now what approach?
#Going for Did
#reduce size of date dataframe:
Date2 <- Date %>%
  select(state,year_effective,group)

#Merge those dates with Log_rev
Merge <- full_join(Log_rev,Date2, by="state")

#start with post and treatment columns
# Create a Post Column
merged_df2 <- Merge %>%
  group_by(state) %>%
  mutate(Post = ifelse(year_effective > year, 0, 1)) %>%
  ungroup()

#Create a treatment column from the Post Column and the group column
merged_df3 <- merged_df2 %>%
  mutate(treatment = ifelse(Post == 1 & group == "t", 1, 0))

#Create a Relative Year column for the Event Study Plot
# Create rel_year column
merged_df3$rel_year <- merged_df3$year - merged_df3$year_effective


#Create inf column, like Butts for control group. (NEED CONTROL GROUP, NOT CHANGED YET)
#merged_df3$rel_year[grepl("c", merged_df3$group)] <- "inf"

#Need the result with Total Tax
Res <- merged_df3 %>%
  mutate(log_totrev=log(TOTLTAX))

#Replace other NA's in post to be Zero
Res[c("Post")][is.na(Res[c("Post")])] <-0

#estimate a result for Total Taxes
treat_var <- "year_effective"
time_var <- "year"

#Make state a numeric factor
Res$state <- as.numeric(factor(Res$state))

#Run Callaway and Sant Anna, ran the Callaway and Sant Anna package, and dropped 419 rows 
#it also gave a warning on small group sizes (all are small).  So, going to try, 
#just Fixed effects regression
Res_attgt <- att_gt(yname = "log_totrev",
                    tname = "year",
                    idname = "state",
                    gname = "year_effective",
                    data = Res)

# Run Two way fixed Effect regression, OLS; dropped almost all abservations, will try again
log_tot_ols = feols(log_totrev ~ log(Post), Res)
summary(log_tot_ols)

#going to try plm approach -  https://stackoverflow.com/questions/61633179/model-with-three-fixed-effects-in-plm-package-in-r

#plm FE model
zz1 <- plm(log_totrev ~ factor(Post),Res)

#simple reg
Reg <- lm(log_totrev ~ Post + year + state,Res)
summary(Reg)

#Simple reg with rate, year and state NEED to be factors because they are categorical variables
#This regression will not work
Reg_rate <- lm(log_totrev ~ Post + year + state + rates, Res)
summary(Reg_rate)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -80.075922   3.457566 -23.160  < 2e-16 ***
#  Post          0.293586   0.059633   4.923 9.18e-07 ***
# year          0.047920   0.001733  27.658  < 2e-16 ***
#  state        -0.006251   0.001487  -4.204 2.73e-05 ***
#  rates        -0.016319   0.010532  -1.549    0.121    

#simple reg, with log rate
# reg_lrate <-lm(log_totrev ~ Post + year + state + log(rates), Res)
#log rate won't run


#Simple Reg (w/o rate) (a) (i)
Simple_reg <- lm(log_totrev ~ Post + factor(year) + factor(state), Res)
summary(Simple_reg)
#Result Post              0.01614    0.01386   1.165 0.244183 


#Simple reg with rate, ran this and positive result for post went away (a) (ii)
factor_rate <- lm(log_totrev ~ Post + factor(year) + factor(state) + rates, Res)
summary(factor_rate)
#result Post              0.015200   0.013753   1.105 0.269179 

#Lost state name, creating df to get it back
Res3 <- Res %>%
  full_join(df, by = c("State_Acronym"))


#He just wants to see, do these states increase their revenue post
# I can try and break those coefficients up by state
# Interaction Reg (with interaction between Post and state)
Interaction_reg <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Res3)
summary(Interaction_reg)

# It is close, just trying to get Alabama
# Set Alabama as the reference category
Res3$State_Name <- relevel(factor(Res3$State_Name), ref = "Alabama")

# Interaction Reg (with interaction between Post and state)
Interaction_reg2 <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Res3)
summary(Interaction_reg2)



#Extract coefficients for each state using Broom
tidy_model <- tidy(Interaction_reg2)

# Filter the coefficients to get only those related to Post
post_coefficients <- tidy_model %>%
  filter(grepl("Post", term))

print(post_coefficients)

#Extracting the post coefficient for each state
fit_state_model <- function(df) {
  lm(log_totrev ~ Post + factor(year), data = df)
}

# Split data by state and fit model for each state
state_models <- Res3 %>%
  group_by(State_Name) %>%
  group_map(~ fit_state_model(.x))

# Extract coefficients for Post from each model
post_coeffs <- lapply(state_models, function(model) {
  tidy(model) %>% filter(term == "Post")
})

# Combine results into a single data frame
post_coeffs_df <- bind_rows(post_coeffs, .id = "State_Name")

print(post_coeffs_df)

#Had an issue with every states Post:coefficient being relative to Alabama.
#the following solution from ChatGPT disaggregated it from alabama and to a generic state factor


#Attempt to dissaggregate the results from the Alabama coefficient
# Set Alabama as the reference category
Res3$State_Name <- relevel(factor(Res3$State_Name), ref = "Alabama")

# Interaction regression with interaction between Post and state, more explanation needs to be given for this interaction in github note.
interaction_model <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Res3)
summary(interaction_model)

# Extract coefficients for each state using Broom
tidy_interaction_model <- tidy(interaction_model)

# Filter the coefficients to get only those related to Post
post_interaction_coefficients <- tidy_interaction_model %>%
  filter(grepl("Post", term))

print(post_interaction_coefficients) 

# Create a data frame for the Post coefficients for each state
post_coefficients_combined <- post_interaction_coefficients %>%
  mutate(
    State = ifelse(term == "Post", "Alabama", gsub("Post:factor\\(State_Name\\)", "", term)),
    Post_Estimate = ifelse(term == "Post", estimate, estimate + post_interaction_coefficients$estimate[post_interaction_coefficients$term == "Post"]),
    Post_Std_Error = std.error,
    Post_Statistic = statistic,
    Post_P_Value = p.value
  ) %>%
  select(State, Post_Estimate, Post_Std_Error, Post_Statistic, Post_P_Value)

options(digits=9)
print(post_coefficients_combined, n= Inf)


#allowed for printing the dataframe to be converted more easily in Latex
print(
  post_coefficients_combined %>%
    mutate(across(where(is.numeric), ~ format(.x, digits = 6, scientific = FALSE))),
  n = Inf
)

#After this print, I am done with results for Latex purposes, now I need to create 
#plots to interpret these coefficients.


# The graph to accompany that would be the cannonical DiD (iii)
#Plot the coefficients for each state from the posts_coefficients_combined df

#time to save dataframe
#save the coefficients for each state for regression 
#Simple_reg <- lm(log_totrev ~ Post + factor(year) + factor(state), Res)
write.csv(post_coefficients_combined,"state_post_dummy_total_rev._6_15_24.csv",row.names = FALSE)

#save the Res3 dataframe, which was the foundation for the result.
write.csv(Res3,"revenue_panel_total_rev_state_coef_result_6_15_24.csv",row.names = FALSE)

#load these dataframes for next plot session