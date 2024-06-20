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
library(coefplot) #Stata coefficient plot, https://cran.r-project.org/web/packages/coefplot/coefplot.pdf

# set seed
set.seed(26)

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


#Need to run a  joint significance test of the hypothesis that no state's value of tau is different from zero.
#from here in the model, I am using Rev as the name of Res3.  Tried F- test, didn't work.  
#Deleting from Script, will still be in history.


## going to remove the state factor part of the state*post coefficient because it is creating
#multicollinearity.

#Attempt Chow test, did not work.  Deleting from script, will still be in history.



# Fit the unrestricted model (with interactions)
interaction_model <- lm(log_totrev ~ Post * factor(State_Name) + factor(year), data = Rev)

# Fit the restricted model (no Post_Estimates)
restricted_model <- lm(log_totrev ~ factor(State_Name) + factor(year), data = Rev)

# Compute RSS for both models
RSS_unrestricted <- sum(residuals(interaction_model)^2)
RSS_restricted <- sum(residuals(restricted_model)^2)

# Degrees of freedom
df_unrestricted <- df.residual(interaction_model)
df_restricted <- df.residual(restricted_model)
num_restrictions <- df_restricted - df_unrestricted

# Calculate F-statistic
F_statistic <- ((RSS_restricted - RSS_unrestricted) / num_restrictions) / (RSS_unrestricted / df_unrestricted)

# Calculate p-value
p_value <- pf(F_statistic, num_restrictions, df_unrestricted, lower.tail = FALSE)

# Print the results
cat("F-statistic:", F_statistic, "\n")
cat("p-value:", p_value, "\n")

# Create a dataframe to store the results
results_df <- data.frame(
  RSS_unrestricted = RSS_unrestricted,
  RSS_restricted = RSS_restricted,
  df_unrestricted = df_unrestricted,
  df_restricted = df_restricted,
  F_statistic = F_statistic,
  p_value = p_value
)

# Print the results dataframe
print(results_df)


# Determine significance
if (p_value < 0.05) {
  cat("The null hypothesis that no state's Post_Estimate is different from zero is rejected.\n")
} else {
  cat("The null hypothesis that no state's Post_Estimate is different from zero is not rejected.\n")
}

#Rerun for closed R, state coefficient FE plot 6/19/24
Rev3 <- read.csv("revenue_cpi_total.csv")

#Run the Two way FE regression again, except, do it with Real Revenue, loaded in Rev 3
Rev4 <- Rev3 %>%
  mutate(logRealTotRev=log(real_totRev))

#Simple regression, real revenue
Simple_reg <- lm(logRealTotRev ~ Post + factor(year) + factor(state), Rev4)
summary(Simple_reg)
#result Post              0.01614    0.01386   1.165  0.24418 


#going to run earlier results, but with the real revenue

# Interaction Reg (with interaction between Post and state)
Interaction_Realreg <- lm(logRealTotRev ~ Post * factor(State_Name) + factor(year), data = Rev4)
summary(Interaction_Realreg)

# It is close, just trying to get Alabama
# Set Alabama as the reference category
Rev4$State_Name <- relevel(factor(Rev4$State_Name), ref = "Alabama")

# Interaction Reg (with interaction between Post and state)
Interaction_Realreg2 <- lm(logRealTotRev ~ Post * factor(State_Name) + factor(year), data = Rev4)
summary(Interaction_Realreg2)


#STOPPED HERE


#Extract coefficients for each state using Broom
tidy_model <- tidy(Interaction_Realreg2)

# Filter the coefficients to get only those related to Post
post_coefficients <- tidy_model %>%
  filter(grepl("Post", term))

print(post_coefficients)

#Extracting the post coefficient for each state
fit_state_model <- function(df) {
  lm(logRealTotRev ~ Post + factor(year), data = Rev4)
}

# Split data by state and fit model for each state
state_models <- Rev4 %>%
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
Rev4$State_Name <- relevel(factor(Rev4$State_Name), ref = "Alabama")

# Interaction regression with interaction between Post and state, more explanation needs to be given for this interaction in github note.
interaction_model <- lm(logRealTotRev ~ Post * factor(State_Name) + factor(year), data = Rev4)
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


#Coef plot of whole model
coefplot(interaction_model)

#Coef plot excluding coefficient
coefplot(interaction_model, exclude = "(Intercept)")

summary(interaction_model)


#Coefficient plot for just year FE
# Specify the coefficients to include
coefficients_to_include <- c(
  "factor(year)1977", "factor(year)1978", "factor(year)1979", 
  "factor(year)1980", "factor(year)1981", "factor(year)1982", 
  "factor(year)1983", "factor(year)1984", "factor(year)1985", 
  "factor(year)1986", "factor(year)1987", "factor(year)1988", 
  "factor(year)1989", "factor(year)1990", "factor(year)1991", 
  "factor(year)1992", "factor(year)1993", "factor(year)1994", 
  "factor(year)1995", "factor(year)1996", "factor(year)1997", 
  "factor(year)1998", "factor(year)1999", "factor(year)2000", 
  "factor(year)2001", "factor(year)2002", "factor(year)2003", 
  "factor(year)2004", "factor(year)2005", "factor(year)2006", 
  "factor(year)2007", "factor(year)2008", "factor(year)2009", 
  "factor(year)2010", "factor(year)2011", "factor(year)2012", 
  "factor(year)2013", "factor(year)2014", "factor(year)2015", 
  "factor(year)2016", "factor(year)2017", "factor(year)2018", 
  "factor(year)2019", "factor(year)2020", "factor(year)2021", 
  "factor(year)2022"
)

#State FE
coefficients_to_include_2 <- c(
  "Post", "factor(State_Name)Alaska", "factor(State_Name)Arizona", 
  "factor(State_Name)Arkansas", "factor(State_Name)California", 
  "factor(State_Name)Colorado", "factor(State_Name)Connecticut", 
  "factor(State_Name)Delaware", "factor(State_Name)Florida", 
  "factor(State_Name)Georgia", "factor(State_Name)Hawaii", 
  "factor(State_Name)Idaho", "factor(State_Name)Illinois", 
  "factor(State_Name)Indiana", "factor(State_Name)Iowa", 
  "factor(State_Name)Kansas", "factor(State_Name)Kentucky", 
  "factor(State_Name)Louisiana", "factor(State_Name)Maine", 
  "factor(State_Name)Maryland", "factor(State_Name)Massachusetts", 
  "factor(State_Name)Michigan", "factor(State_Name)Minnesota", 
  "factor(State_Name)Mississippi", "factor(State_Name)Missouri", 
  "factor(State_Name)Montana", "factor(State_Name)Nebraska", 
  "factor(State_Name)New Hampshire", "factor(State_Name)New Jersey", 
  "factor(State_Name)New Mexico", "factor(State_Name)New York", 
  "factor(State_Name)North Carolina", "factor(State_Name)North Dakota", 
  "factor(State_Name)Ohio", "factor(State_Name)Oklahoma", 
  "factor(State_Name)Oregon", "factor(State_Name)Pennsylvania", 
  "factor(State_Name)Rhode Island", "factor(State_Name)South Carolina", 
  "factor(State_Name)Tennessee", "factor(State_Name)Utah", 
  "factor(State_Name)Vermont", "factor(State_Name)Virginia", 
  "factor(State_Name)West Virginia", "factor(State_Name)Wisconsin"
)


# Create the coefficient plot including only the specified coefficients, year FE
coefplot(interaction_model, coefficients=c(coefficients_to_include), title= "Coefficient Plot for Year Fixed Effects")
        
#State FE
coefplot(interaction_model, coefficients=c(coefficients_to_include_2), title= "Coefficient Plot for State Fixed Effects") +
  theme_minimal()

#chat gpt didn't work for the state policy dummy coefficient, save previous two plots when I return.
#had to close plot, loading DF for last FE plot, had to run lines 


# Still trying to get the post estimate plot, going to try this: https://interludeone.com/posts/2022-12-15-coef-plots/coef-plots#:~:text=Drawing%20coefficient%20plots%20in%20R%20and%20ggplot%20.&text=Recently%20a%20colleague%20asked%20how,use%20the%20command%20coefplot%20afterwards.

post_coefficients_combined %>%
  ggplot(aes(x = Post_Estimate, y = State)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x= "Value", y= "Coefficient", title= "Coefficient Plot for Policy by State") +
  theme_minimal()  

#Plot ran, will save and close

#Now will try and plot after removing NA States
#Filter out NA states
na_states <- post_coefficients_combined %>%
  filter(is.na(Post_Estimate)) %>%
  pull(State)

#Create filtered DF
filtered_data <- post_coefficients_combined %>%
  filter(!is.na(Post_Estimate))

#Plot without NA states
plot <- ggplot(filtered_data, aes(x = Post_Estimate, y = State)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Value", y = "Coefficient", title = "Coefficient Plot for Policy by State- No NA States") +
  theme_minimal()

install.packages("grid")
library(grid)
# Add footnote
footnote <- paste("States with NA estimates:", paste(na_states, collapse = ", "))
plot <- plot + annotation_custom(
  grob = textGrob(footnote, x = 0, y = 0, hjust = -0.1, vjust = -0.1, gp = gpar(fontsize = 10, col = "blue")),
  xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
)

print(plot)

#save Rev4 sheet
write.csv(Rev4,"logTotRev_cpi.csv",row.names = FALSE)
#save post_interactions_coefficients
write.csv(post_coefficients_combined,"twoWFE_LogTotRev_result.csv",row.names = FALSE)
