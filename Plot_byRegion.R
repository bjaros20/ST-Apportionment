#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#loadpackages, giroud and your apportionment rates
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")
install.packages("ggthemes")
install.packages("ggtext")
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggthemes)
library(ggtext)

#Load Documents
combined_clean_df <-read.csv("Long_Gir_My_App_Comb_clean.csv")

Master_df <- read.csv("MasterMerge_RealRev_EventDates.csv")

RegionNumber <- read_xlsx("RNChart.xlsx")

StateNumber <- read_xlsx("RegionNumber.xlsx")

#Merge To Get States by Region
merged_df <- merge (combined_clean_df,StateNumber,by = c("state"))

merged_df1 <- merge(merged_df,RegionNumber, by= c("Number"))

by_region_filter <-merged_df1 %>%
  filter(state !="Texas")

#Plot by Region
by_region_year2 <- by_region_filter %>%
  group_by(Region, year)%>%
  summarize(meanSales = mean(sales))

#Plot by Region, line plot
ggplot(by_region_year2, aes(x=year,y=meanSales,color=Region))+geom_line()+
  labs(x="Year",y="Sales Factor")+
  theme_minimal() +
  scale_color_discrete(name="Region") + theme_ipsum()


ggplot(by_region_year2, aes(x=year,y=meanSales,color=Region))+geom_point()+
  labs(x="Year",y="Sales Factor")+
  theme_minimal() +
  scale_color_discrete(name="Region")

#Boxplot, not good.
ggplot(by_region_year2, aes(x=year,y=meanSales,color=Region))+geom_boxplot()+
  labs(x="Year",y="Sales Factor")+
  theme_minimal() +
  scale_color_discrete(name="Region")

#Line Plot by Region is good, but want to make it look better
ggplot(by_region_year2, aes(x=year,y=meanSales,color=Region))+geom_line(size=1.2)+
  labs(x="Year",y="Sales Factor")+
  ggtitle("Average Sales Factor Weight by Region")
  theme_minimal() +
  theme_economist() + 
  scale_color_discrete(name="Region") +
  theme  legend.text = element_text(size = 16),         # Adjust the legend font size
axis.title.x = element_text(size = 18),        # Adjust the x-axis label font size
axis.title.y = element_text(size = 18),        # Adjust the y-axis label font size
plot.title = element_text(size = 20)           # Adjust the plot title font size (if applicable)
)


#All States, Economist Theme, not good
ggplot(combined_clean_df, aes(x=year,y=sales,color=state))+geom_line()+
  labs(x="Year",y="Sales")+
  theme_minimal() +
  scale_color_discrete(name="State") +
  theme_economist() 

#Attempt to Keep Economist Theme, kept, not perfect, but okay.
ggplot(by_region_year2, aes(x = year, y = meanSales, color = Region)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Sales Factor") +
  scale_color_discrete(name = "Region") +
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight by Region")


# Plot Just the Annual Average Sales Factor Over time

# Sort Sales Factor By Just Year
Annual_Avg2 <-by_region_filter %>%
  group_by(year)%>%
  summarize(meanSales = mean(sales))



#Simple Plot
ggplot(Annual_Avg, aes(x = year, y = meanSales)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Sales Factor") +
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),  # Adjust the font size of x-axis labels
    axis.text.y = element_text(size = 14),   # Adjust the font size of y-axis labels
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight For All States by Year")



#Integrate Simple Average to overlay over all other regions:
merged_AA <- merge(by_region_year2,Annual_Avg, by= c("year"))

merge_AA_br2 <- rbind(by_region_year2,Annual_Avg2)

merge_AA_br2$Region[is.na(merge_AA_br2$Region)] <- "Annual Avg"

#Attempt to Overlay Annual Average:
ggplot(merge_AA_br2, aes(x = year, y=meanSales, color = Region)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Sales Factor") +
  scale_color_discrete(name = "Region") +
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight by Region")


#Attempt to add Annual Avg.

# Define a custom color palette
custom_palette <- c("Annual Avg" = "blue", "Other Regions" = "gray")

# Modify the plot using the custom color palette
ggplot(merge_AA_br2, aes(x = year, y = meanSales, color = Region)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Sales Factor") +
  scale_color_manual(name = "Region", values = custom_palette) +
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight by Region")


#Trying to keep color
# Define a custom color palette for the background colors of regions
background_colors <- c("New England" = "#F5A623",      # Define light background colors for each region
                       "Mideast" = "#7ED321",
                       "Southeast" = "#50E3C2",
                       "Great Lakes" = "#4A90E2",
                       "Plains" = "#FF33B1",
                       "Southwest" = "#FFD801",
                       "Rocky Mountain" = "#32D9E0",
                       "Far West" = "#29D05C",
                       "AK/HA" = "#D08E8F")

# Create the plot with separate layers for "Annual Avg" and other regions
ggplot(merge_AA_br2, aes(x = year, y = meanSales, color = Region)) +
  geom_line(data = subset(merge_AA_br2, Region == "Annual Avg"), size = 1.2, color = "black") +
  geom_line(data = subset(merge_AA_br2, Region != "Annual Avg"), size = 1.2) +
  labs(x = "Year", y = "Sales Factor") +
  scale_color_manual(name = "Region", values = c("Annual Avg" = "black", background_colors), guide = "none") +
  scale_fill_manual(values = background_colors) +  # Set background colors
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight by Region")

#FinalGraph

ggplot() +
  geom_line(data = subset(merge_AA_br2, Region != "Annual Avg"), aes(x = year, y = meanSales, color = Region), size = 1.2) +
  geom_line(data = subset(merge_AA_br2, Region == "Annual Avg"), aes(x = year, y = meanSales, color = "Annual Avg"), size = 1.5) +
  labs(x = "Year", y = "Sales Factor") +
  scale_color_manual(values = c("Annual Avg" = "black", background_colors)) +
  scale_fill_manual(values = background_colors) +
  theme_minimal() +
  theme_economist() +
  theme(
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),  # Adjust the font size of x-axis labels
    axis.text.y = element_text(size = 14),   # Adjust the font size of y-axis labels
    plot.title = element_markdown(size = 20, hjust = 0.5, vjust = 0.5)  # Center the title
  ) +
  ggtitle("Average Sales Factor Weight by Region")

write.csv(merge_AA_br2,"Average_SF_Weight_By_Region.csv",row.names=FALSE)

write.csv(merged_df1,"SF_Weight_State_Region.csv",row.names=FALSE)



