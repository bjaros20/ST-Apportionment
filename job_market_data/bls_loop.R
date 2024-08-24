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

#Now create a BLS loop, loop through the selected vectors


