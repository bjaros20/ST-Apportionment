# Create Dataframe for Elasticity results for All the Tax Bases
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#New Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load App Weights Dataframe from Earlier Elasticity Plots
AppWeights <-read.csv("AppWeights_4_21_24_Rev copy.csv")

SalesFac_jmp <-read_xlsx("sales_factor_jmp_may_24.xlsx")

#difference between AppWeights and SaleFac_jmp.  Need to create a master JMP elasticity sheet to become public
#2023 data is one difference.

#Going to use antijoin to find out what rows are in AppWeights but aren't in SalesFac_jmp
diff_App_Sal <- anti_join(AppWeights,SalesFac_jmp,by=c("state","year"))

#^ ran it, these are the non corporate income tax states (NV, SD, TX, WA, and WY) + OH, excluded because of the CAT
#save this to be added back when needed for control group

write_csv(diff_App_Sal,"Non_CIT_states_FRED_OH.csv")


#Now going to run anti_join to figure out what is in SaleFac but is not in AppWeights, should be zero, but checking
diff_Sal_App <- anti_join(SalesFac_jmp,AppWeights, by= c("state","year"))
#^it was zero observations

#Create a dataframe
common_rows <-inner_join(AppWeights,SalesFac_jmp, by=c("state","year"))

#find out what sales don't match between the dataframes
differences_in_sales <- common_rows %>% filter(sales.x != sales.y)
 #^No differences in the sales factor weights, only differences in industry, election, and phase in dummies.

#make the common_rows df the job market dataframe for elasticity
common_rows2 <-common_rows %>%
  select(-sales.x) %>% # Remove the sales.x column
  rename(sales = sales.y)

write_csv(common_rows2,"elasticity_data_jmp.csv")


#then Merge the rates to run the Three Way FE:
rates <-read_xlsx("stateCIT_rates_1976_2023 copy.xlsx")

#There are a couple of issues with the rate right now.  1976-2002 are in a different format than 2003 onwards.
#There are also missing values for years.  For now,  I will use the historic rate over the more recent rate

#start with formatting
class(rates)
str(rates)

#The dates between 1976-2002 are characters. They also have a space before the first number

#remove the space first

#install stingr package
install.packages("stringr")
library(stringr)

#Remove the space
str_remove_all(rates," ")
#Need to create a separate dataframe and merge back

rates_1976_2002 <- rates %>% select("1976 ":"2002 ")
colnames(rates)

#Space after the year for 1976 through 2002.  
nospace_rate <- as.data.frame(
  apply(rates,2, function(x) gsub("\ ", "", x)))

#it ran, so will try again.
str(nospace_rate)



#Will need to first remove the leading space from the columns:
colnames(rates) <- str_trim(colnames(rates))

cols_to_modify <- colnames(rates)[grepl("^(1976|1977|1978|1979|1980|1981|1982|1983|1984|1985|1986|1987|1988|1989|1990|1991|1992|1993|1994|1995|1996|1997|1998|1999|2000|2001|2002)$", colnames(rates))]

#Try again but will try to remove the first space from the character columns:
rates_rem_space <- rates %>%
  mutate(across(all_of(cols_to_modify), ~ str_remove(., "^ ")))

#didn't work, try again, by forcing the numeric conversion
rates_rem_space <- rates %>%
  mutate(across(.cols = -State, .fns = ~ as.numeric(trimws(.))))

rates_rem_space <- rates %>%
  mutate(across(.cols = -State, .fns = ~ parse_number(trimws(.))))

# Check the structure of the cleaned data
glimpse(rates_rem_space)

class(rates_rem_space)
str(rates_rem_space)

#One last thing, make sure all columns titles year is numeric


#save this clean rates dataframe
write.csv(rates_rem_space,"clean_rates_1976-2022.csv")

#this worked, all of the columns in rates_rem_space are numeric.
rates_cleaned <- rates_rem_space %>%
  mutate(across(.cols = `1976`:`2002`, .fns = ~ as.numeric(trimws(.))))

# Check the structure of the cleaned data to confirm conversion
glimpse(rates_cleaned)

#save the one with the columns double checked
write.csv(rates_cleaned,"rates_jmp.csv")

#Now to merge the rates to the elasticity df

#transform the rates dataframe from a wide to long dataframe
# Transform rates_cleaned to long format
rates_long <- rates_cleaned %>%
  pivot_longer(cols = `1976`:`2023`, names_to = "year", values_to = "rate") %>%
  mutate(year = as.integer(year))  # Convert year to integer for joining

#use an inner join to merge Elas and rates_long
Elas2 <- Elas %>%
  inner_join(rates_long,by=c("state"="State","year"="year"))%>%
  rename(rates=rate)

#save as Elasticity and Rate Data for JMP
write.csv(Elas2,"elasticity_rates_jmp.csv")