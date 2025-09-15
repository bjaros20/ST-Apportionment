# Get rate of change for tax bases and find elasticity of tax base w.r.t apportionment and rate changes.
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)

#New Directory
setwd("~/Documents/GitHub/ST-Apportionment/CrossPartial")

# Read in DF
Rev <- read.csv("Clean_Rev_Event_4_16_24.csv")
App <- read_xlsx("SalesFactorWeights_ALLStates_Apri_2024.xlsx")
AllYears <- read.csv("SALT_App_Merge_2024.csv")

#Add Regions et al to merge on
RegionNumber <- read_xlsx("RNChart.xlsx")
StateNumber <- read_xlsx("RegionNumber.xlsx")

#Remove Past Region and Number, because they are not filled in
#filter out State, sales.x and Number
filtered_RevPercChange_App<-filtered_RevPercChange_App %>%
  select(-Region,-Number)

#Merge with Region and State information
filtered_RevPercChange_App2 <- merge(filtered_RevPercChange_App,StateNumber,by=c("state"))
filtered_RevPercChange_App2 <- merge(filtered_RevPercChange_App2,RegionNumber,by=c("Number"))

#Merge Group, type, year_pass_intro, year_effective from
# Specify columns to merge on
merge_cols <- c("group", "type", "year_pass_intro", "year_effective")

# Perform left join
merged_data <- left_join(filtered_RevPercChange_App2,
                         Rev %>% select(State_Acronym, all_of(merge_cols)),
                         by = "State_Acronym") %>%
  rowwise() %>%
  mutate(across(all_of(merge_cols), 
                ~ if_else(is.na(.x), filtered_RevPercChange_App2[[cur_column()]], .x),
                .names = "filled_{.col}")) %>%
  select(-starts_with("filled_"))

# Check the result
head(merged_data)
#Save Merged_data, which includes the AVG Elasticity, and Region Information
write.csv(merged_data,"Avg_CIT_Elasticity_With_Regions.csv")

#Clean Up All Years
#Remove Duplicate Columns
AllYears_Clean <- AllYears %>%
  distinct(State_Acronym, year, BUSLICTAX, .keep_all = T)

#Filter to only have years 1976 through 2023
AYC<- AllYears_Clean %>%
  filter(year >=1976)

#Remove X Column
AYC2 <- select(AYC,c(-X,-sales))



# Have different Match.  Need to match on State Acronym and State
#Create Acronym Dataframe
state_data <- data.frame(
  acronym = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
              "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
              "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)
# Print the dataframe
print(state_data)

#Merge Acronym to State Names
Merge <- left_join(Rev,state_data, by = c("State_Acronym" = "acronym"))

#Merge Apportionment on Year and State,  Want to 

AppRev <-left_join(Merge,App,by = c("state_name"="state","year"))

#filter out State, sales.x and Number
filterAppRev<-AppRev %>%
  select(-state,-sales.x,-Number)

#Create Percentage Change Columns
filterAppRev2<- filterAppRev %>%
  group_by(state_name)%>%
  arrange(state_name,year)%>%
  mutate(perc_change_Sales = ((sales.y-lag(sales.y))/((sales.y+lag(sales.y))/2)))


SalesChange <-filterAppRev2 %>%
  filter(perc_change_Sales != 0 | lag(perc_change_Sales) != 0) %>%
  slice(c(which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0) - 1,
          which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0)))


#Try to conduct merge of new App Weights and find Elasticity on all years.  This DF going foreward will not have event years, treatment, etc.  But, could remerge for information.

#Merge on the more accurate Apportionment weights:
AllYears_AppRev <-left_join(AYC2,App, by = c("state","year"))

#Save this CSV:
write_csv(AllYears_AppRev,"AppWeights_4_21_24_Rev.csv")


#Create Percentage Change Columns
ALLYears_AppRev2<- AllYears_AppRev %>%
  group_by(state)%>%
  arrange(state,year)%>%
  mutate(perc_change_Sales = ((sales-lag(sales))/((sales+lag(sales))/2)))

#(NOTE: may modify to remove duplicate years, or will remove the zero years later)
AllYears_SalesChange <- ALLYears_AppRev2 %>%
  filter(perc_change_Sales != 0 | lag(perc_change_Sales) != 0 | lead(perc_change_Sales) != 0 | lead(perc_change_Sales, 2) != 0) %>%
  slice(c(which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0 | lead(perc_change_Sales) != 0 | lead(perc_change_Sales, 2) != 0) - 1,
          which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0 | lead(perc_change_Sales) != 0 | lead(perc_change_Sales, 2) != 0),
          which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0 | lead(perc_change_Sales) != 0 | lead(perc_change_Sales, 2) != 0) + 1,
          which(perc_change_Sales != 0 | lag(perc_change_Sales) != 0 | lead(perc_change_Sales) != 0 | lead(perc_change_Sales, 2) != 0) + 2))



#Clean up the AllYears_SalesChange, there are duplicates. 
AllYears_SalesChange_NoDup <- AllYears_SalesChange %>%
  distinct(state, year, BUSLICTAX, .keep_all = T)

#Find the Percentage in Revenue for CORPORATE INCOME
RevPercChange_App<- AllYears_SalesChange_NoDup %>%
  group_by(state)%>%
  arrange(state,year)%>%
  mutate(perc_change_CIT_Rev = ((CORPINCTX-lag(CORPINCTX))/((CORPINCTX+lag(CORPINCTX))/2)))

#Save to CSV with Surrounding Years
write.csv(RevPercChange_App,"SurroundingYears_PercChangeApp_Rev.csv")

#Now filter to only have years were perc_Change_Sales Does not equal zero.
PerChange <- RevPercChange_App %>%
  group_by(state, year) %>%
  filter(perc_change_Sales != 0) %>%
  ungroup()

OwnPrice_CIT <- PerChange %>%
  arrange(state,year)%>%
  mutate(OwnPartial_Elasticity= perc_change_CIT_Rev/perc_change_Sales)

#Drop All NAs, removes Vermont and WV for recent Reforms
OwnPrice_CIT <- OwnPrice_CIT %>%
  filter(!is.na(OwnPartial_Elasticity))
  

#Create an Average for Each state
Avg_Elasticity_State <- OwnPrice_CIT %>%
  group_by(state) %>%
  mutate(Avg_Elasticity = mean(OwnPartial_Elasticity))

#Just keep Average Own Price for each state:
filtered_RevPercChange_App <- Avg_Elasticity_State %>%
  distinct(state, .keep_all = TRUE)

#Want to merge Characteristics From Treatment and Control, Year, Phase-in, Region, etc to the Elasticity to plot on those characteristics

Char_Merge <-filtered_RevPercChange_App %>%
  

#Plot the 37 Elasticities

options(repr.plot.width = 10, repr.plot.height = 5)  # Adjust width and height as needed

# Create the scatter plot
plot <- ggplot(filtered_RevPercChange_App, aes(x = Avg_Elasticity, y = State_Acronym)) +
  geom_point(aes(color = State_Acronym), size = 3) +
  geom_segment(aes(xend = Avg_Elasticity, yend = State_Acronym, x = -2, y = State_Acronym), linetype = "dashed") +
  geom_segment(aes(xend = Avg_Elasticity, yend = State_Acronym, x = 2, y = State_Acronym), linetype = "dashed") +
  geom_text(aes(label = State_Acronym), vjust = 0, size = 4, position = position_dodge(width = 0.2)) +
  labs(title = "Average Elasticity by State",
       x = "Avg_Elasticity",
       y = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(xlim = c(-2.5, 2.5))  # Adjust x-axis limits

# Display the plot
print(plot)

#Try one more plot, this with a centered title, trying to improve readability:
# Set plot size
options(repr.plot.width = 10, repr.plot.height = 5)  # Adjust width and height as needed

# Create the scatter plot
plot <- ggplot(filtered_RevPercChange_App, aes(x = Avg_Elasticity, y = State_Acronym)) +
  geom_point(aes(color = State_Acronym), size = 3) +
  geom_text(aes(label = State_Acronym), vjust = 0, size = 4, position = position_dodge(width = 0.2)) +
  labs(title = "Average Own CIT Base Elasticity w.r.t Sales Apportionment Factor for each State",
       x = "Avg Own CIT Elasticity w.r.t Apportionment Weight",
       y = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, margin = margin(b = 20))) +  # Center title and increase space
  coord_cartesian(xlim = c(-2.5, 2.5))  # Adjust x-axis limits

# Add dashed vertical lines at specified x-values using annotate()
x_values <- c(0, 0.5, -0.5, 1, -1)

for (i in x_values) {
  plot <- plot + annotate("segment", x = i, xend = i, y = 1, yend = nrow(filtered_RevPercChange_App) + 1, linetype = "dashed")
}

# Display the plot
print(plot)

#Creating the plot with the Merged Data for Region, etc.
# Create the scatter plot with merged_data
plot <- ggplot(merged_data, aes(x = Avg_Elasticity, y = State_Acronym)) +
  geom_point(aes(color = State_Acronym), size = 3) +
  geom_text(aes(label = State_Acronym), vjust = 0, size = 4, position = position_dodge(width = 0.2)) +
  labs(title = "Average Own CIT Base Elasticity w.r.t Sales Apportionment Factor for each State",
       x = "Avg Own CIT Elasticity w.r.t Apportionment Weight",
       y = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, margin = margin(b = 20))) +  # Center title and increase space
  coord_cartesian(xlim = c(-2.5, 2.5))  # Adjust x-axis limits

# Add dashed vertical lines at specified x-values using annotate()
x_values <- c(0, 0.5, -0.5, 1, -1)

for (i in x_values) {
  plot <- plot + annotate("segment", x = i, xend = i, y = 1, yend = nrow(merged_data) + 1, linetype = "dashed")
}

# Display the plot
print(plot)
