#Create a loop for the bls employment results by state

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
library(synthdid)
library(fixest)
library(boot)
library(ggthemes)

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Detrended, per capita, real revenue data, bls data
Rev <- read.csv("detrend_per_capita.csv")
naive_ci<-read.csv("naive_ci.csv")
bls <-read.csv("bls_NoID_complete.csv")

#remove x and resave bls sheet after adding post from Rev
Post <- Rev%>%
select(State_Name,Post,year,year_effective)


#merge, full join Post to bls on State_Name and year
bls_merge <- bls %>%
  full_join(Post, by = c("GeoName" = "State_Name", "year" = "year"))


#left join eliminates USA and filters down to just 2001-2022
bls_merge2 <- bls %>%
  left_join(Post, by = c("GeoName" = "State_Name", "year" = "year"))

filter_bls <-bls_merge2 %>%
  select(-X)

# Remove leading "X" from column names from the merge
colnames(filter_bls) <- gsub("^X", "", colnames(filter_bls))

#remove leading periods from spaces
colnames(filter_bls) <- gsub("^X|\\.+", "", colnames(filter_bls))

write.csv(filter_bls,"bls_post.csv")


#Create an industry vector for loop
# Create a vector with the specified column names
selected_columns <- c(
  "Totalemploymentnumberofjobs",
  "Wageandsalaryemployment",
  "Proprietorsemployment",
  "Farmproprietorsemployment",
  "Nonfarmproprietorsemployment2",
  "Farmemployment",
  "Nonfarmemployment",
  "Privatenonfarmemployment",
  "Forestryfishingandrelatedactivities",
  "Miningquarryingandoilandgasextraction",
  "Utilities",
  "Construction",
  "Manufacturing",
  "Wholesaletrade",
  "Retailtrade",
  "Transportationandwarehousing",
  "Information",
  "Financeandinsurance",
  "Realestateandrentalandleasing",
  "Professionalscientificandtechnicalservices",
  "Managementofcompaniesandenterprises",
  "Administrativeandsupportandwastemanagementandremediationservices",
  "Educationalservices",
  "Healthcareandsocialassistance",
  "Artsentertainmentandrecreation",
  "Accommodationandfoodservices",
  "Otherservicesexceptgovernmentandgovernmententerprises",
  "Governmentandgovernmententerprises",
  "Federalcivilian",
  "Military",
  "Stateandlocal",
  "Stategovernment",
  "Localgovernment"
)

# View the vector
selected_columns

#rename GeoName column to State_Name
filter_bls <- filter_bls %>%
  rename(State_Name = GeoName)

#Now create a BLS loop, loop through the selected vectors

filt_Corp <-filter_bls


#Create State Dataframes
#create result list to store dataframe
result_list <- list()

# Make a copy of the original dataframe to work with
original_df <- filt_Corp
#counter variable, so as loop progresses, drops first state
counter <- 1

while (TRUE) {
  #Reset to original each start
  df <-filt_Corp
  
  # Arrange by year_effective and select the first state for treatment
  df <- df %>% arrange(year_effective)
  
  #counter variable for running the loop
  if(df$year_effective[counter] >= 2022) {break}
  
  treatment_state <- df %>% slice(counter) 
  treatment_year <- treatment_state$year_effective
  treatment_state_name <- treatment_state$State_Name
  
  #filter out prior treated states, but keep no treatment states
  #first half of filter keeps no treatment states in, 
  df <- df %>% 
    filter(is.na(year_effective) | is.na(State_Name) | year_effective >= treatment_year)
  
  # removes states treated in same year
  #the year_effective=treatment_year are filtered out if they are not 'treatment_state'
  df <- df %>%
    filter(is.na(year_effective) | (!(State_Name != treatment_state_name & year_effective == treatment_year)))
  
  
  # Filter out rows with year <= 2 years after treatment_year
  df <- df %>%
    filter(year <= treatment_year + 2)
  
  #filter out any states that get treated within 2 years of treatment
  df <- df %>%
    filter(is.na(year_effective) | (!(year_effective > treatment_year & year_effective<= treatment_year + 2)))
  
  #filter out Ohio after treatment_year >=2012, because OH eliminates CI in 2014
  df <- df %>%
    filter(!(treatment_year >= 2012 & State_Name == "Ohio"))
  
  # Store the dataframe for this treatment state
  assign(treatment_state_name, df)
  result_list[[treatment_state_name]] <- df
  
  # Check if the treatment year is 2022 or greater, break
  if (treatment_year >= 2022) {break}
  
  #increment counter
  counter <-counter + 1
  
  #empty dataframe break
  if (nrow(df) == 0) {break}
}
