#Attempt to Get Revenue in R

install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("did")
install.packages("tidyverse")
install.packages("lmtest")
library(readxl)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)


#set working directory
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")


#Read Many DFs

# create a vector of state acronyms
state_acronyms <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", 
                    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                    "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
                    "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
                    "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

# loop through the state acronyms and read in the corresponding Excel file
state_dfs <- list()  # Initialize a list to store the state dataframes

for (state in state_acronyms) {
  file_name <- paste(state, ".xls", sep="")
  sheet_name <- "Annual"
  state_dfs[[state]] <- read_excel(file_name, sheet=sheet_name)
}

# Access the Alaska dataframe by its acronym
AK_df <- state_dfs[["AK"]]

# Initialize AK_fractions with the same number of rows as AK_df
AK_fractions <- data.frame(matrix(NA, nrow = nrow(AK_df), ncol = length(colnames(AK_df))))

# Assign column names to AK_fractions
colnames(AK_fractions) <- colnames(AK_df)

# Copy the date column to AK_fractions
AK_fractions$DATE <- AK_df$DATE

# Loop through the columns of the original dataframe
for (col in colnames(AK_df)) {
  # Check if the column is one of the specified columns
  if (col %in% c("AKBUSLICTAX", "AKCORPINCTX", "AKCORPLICTAX", "AKPROPTAX", "AKSLGRTAX",
                 "AKSVRNCTAX", "AKTLINCTAX", "AKTLSLTAX", "AKTOTLTAX")) {
    # Calculate the fraction of the AKTOTLTAX column
    if (col == "AKTOTLTAX") {
      # If it's the AKTOTLTAX column, just copy it to the new dataframe
      AK_fractions[[col]] <- AK_df[[col]]
    } else {
      # Otherwise, divide the column by AKTOTLTAX and add it to the new dataframe
      AK_fractions[[col]] <- AK_df[[col]] / AK_df[["AKTOTLTAX"]]
    }
  }
}


#Pause, just CIT revenue and share of tax revenue
EventYear <- read_excel("EffectiveYearSSFA.xlsx",sheet=1)




