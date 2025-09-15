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


#Pause 
# Create an empty dataframe to store the state-year panel
state_year_panel <- data.frame(matrix(NA, nrow = length(state_acronyms), ncol = length(unique(AK_df$DATE))))

# Assign column names to the state-year panel using the unique years from AK_df$DATE
colnames(state_year_panel) <- unique(AK_df$DATE)

# Assign row names to the state-year panel using the state acronyms
rownames(state_year_panel) <- state_acronyms


#stop

# Loop through the state acronyms
for (state in state_acronyms) {
  # Check if the state exists in the state_dfs list
  if (state %in% names(state_dfs)) {
    # Access the dataframe of the current state
    state_df <- state_dfs[[state]]
    
    # Check if the state dataframe has the corresponding column
    if (paste(state, "CORPINCTX", sep = "") %in% colnames(state_df)) {
      # Loop through the rows of the state dataframe
      for (i in 1:nrow(state_df)) {
        # Extract the year from the DATE column
        year <- format(state_df$DATE[i], "%Y")
        
        # Find the column index corresponding to the year
        col_index <- which(colnames(state_year_panel) == year)
        
        # Assign the corresponding CORPINCTX value to the state-year panel
        state_year_panel[state, col_index] <- state_df[[paste(state, "CORPINCTX", sep = "")]][i]
      }
    } else {
      # Display a warning message if the corresponding column is missing
      warning(paste("Column", paste(state, "CORPINCTX", sep = ""), "is missing in the", state, "dataframe. Skipping."))
    }
  } else {
    # Display a warning message if the state dataframe is missing
    warning(paste("Dataframe for", state, "not found. Skipping."))
  }
}

print(state_year_panel)
write.csv(state_year_panel, "state_year_panel.csv", row.names = TRUE)
# Identify the columns with NA values in the first set
na_columns <- colSums(is.na(state_year_panel)) == nrow(state_year_panel)

# Remove the NA columns from the first set
state_year_panel <- state_year_panel[, !na_columns]

# Rearrange the columns to have the second set (1942-1956) before the third set (1957-2021)
state_year_panel <- state_year_panel[, c((which(colnames(state_year_panel) >= "1942" & colnames(state_year_panel) <= "1956")),
                                         (which(colnames(state_year_panel) >= "1957")))]

# Rename the state_year_panel
new_state_year_panel <- state_year_panel

# Print the updated state_year_panel
new_state_year_panel

# Replace NAs with 0s in new_state_year_panel
new_state_year_panel[is.na(new_state_year_panel)] <- 0

write.csv(new_state_year_panel, "new_state_year_panel.csv", row.names = TRUE)

view(new_state_year_panel)
