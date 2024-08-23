# Sort and Clean BLS employment by State and Industry 2001-2022

#load packages
library(tidyr)
library(dplyr)
library(tidyverse)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/CAEMP25N")

#Load data
bls <- read.csv("CAEMP25N__ALL_AREAS_2001_2022.csv")

#create vector of states and USA
state_names <- c("United States",
                 "Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

#filter to only have bls values that are the US or state
filt_bls <- bls%>%
  filter(GeoName %in% state_names)

filt2_bls <- filt_bls %>%
  select(-GeoFIPS,-TableName,-Unit)

#Remove the X from in front of year
colnames(filt2_bls) <- gsub("^X", "", colnames(filt2_bls))

#convert years to year column
filt2_bls_long <- filt2_bls %>%
  pivot_longer(cols = starts_with("2"),  # This targets all columns from 2001 to 2022
               names_to = "year", 
               values_to = "value")

#spread the values for sector out via wide split:
filt2_bls_wide <- filt2_bls_long %>%
  pivot_wider(names_from = Description, 
              values_from = value)

#collapse the NAs
filt2_bls_transposed <- filt2_bls_wide %>%
  arrange(GeoName, year) 


#One more step, and it is going to be there.
write.csv(filt2_bls_transposed,"bls_transpose.csv")
