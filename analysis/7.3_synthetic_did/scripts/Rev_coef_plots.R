#Plot of Log Total Revenue Coefficients & DiD plots

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom) #extract coefficients for each state
library(readxl)

#set directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load Data, Coefficient Plot & Rev Data for estimates
Coef <-read.csv("state_post_dummy_total_rev._6_15_24.csv")

Rev <-read.csv("revenue_panel_total_rev_state_coef_result_6_15_24.csv")

#Eliminate the X.  For future reference, when saving the CSV
#read.csv("dataframe.csv",row.names=FALSE)
Rev <-Rev %>%
  select(-X)
Coef <-Coef %>%
  select(-X)

#Load Region Dataframe for plot. Also, could use year of switch, trying to find a pattern
Region <- read_xlsx("RNChart.xlsx")
StateNum <- read_xlsx("RegionNumber.xlsx")

#Merge Region and State Number

#Merge Coef and StateNum
Merge <- Coef %>%
  full_join(StateNum, by= c("State"="state"))

#Merge Merge and Number for Region
Merge2 <- Merge %>%
  full_join(Region, by = c("Number"))

#Plot Coef with Region by Color

#histogram plot
ggplot(Merge2, aes(Post_Estimate)) +
  geom_histogram(bins=10)

#density plot of the coefficients
ggplot(Merge2, aes(x = Post_Estimate)) +
  geom_density() + ggtitle("Density Plot of SSFA Coefficent")


#Create a scatter plot
ggplot(Merge2, aes(x = Post_Estimate, y = Number, color = Region)) +
  geom_point()

#Need something on the Y or X axis, I will merge year of switch
#want year_effective from Rev dataframe

Switch <- Rev %>%
  select(State_Name,year_effective)

#Merge plot df to year
plotdf <- Merge2 %>%
  full_join(Switch,by=c("State"="State_Name"))

#Try scatter plot again with year on y axis
ggplot(plotdf, aes(x=year_effective,y=Post_Estimate, color = Region)) +
         geom_point() + ggtitle("Estimate of SSFA and Year Enacted")


#notes from Howard 6/17/24
# Run simple regression without one FE and without the other.


#run same regression but on the different tax types.

# coefficient plot, stata will give it by state.

#individual year effects are massive.  

#run it without either fixed effects

#Classic DiD on a bunch of middle adopters.

#going to close this up and create new sheet for revenue.  Reload Rev, adjust for inflation.  Repeat. 

