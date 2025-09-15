#Second Week Cleaning Attempt, want to eliminate TX: 
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("did")
install.packages("did2s")
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(did)
library(did2s)

#set working directory
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Cleaned Set-up
Initial_df <-read.csv("LogRev_Appt_Merge_1976-2021.csv")

#Remove Region and Number
Clean_Set_up <- subset(Initial_df, select = -c(Region, Number,Revenue))

#All States before 2007
Set_Up_1976_2007 <-subset(Clean_Set_up,Year >=1976 & Year <=2007)

#All States before 2000
Set_Up_1976_1999 <-subset(Clean_Set_up,Year >=1976 & Year <=1999)

#Sort Into TreatMent and Control Groups
Just_NE <- subset(Clean_Set_up, State == "Nebraska")

# For years before 2007
Just_NE_2007 <- subset(Clean_Set_up, State == "Nebraska" & Year < 2008)

# For years before 1999
Just_NE_1999 <- subset(Clean_Set_up, State == "Nebraska" & Year < 2000)


#Create Control Group For 1976 through 2007 (Control Group #1)
Control_2007_Df <-  Set_Up_1976_2007 %>%
  group_by(State) %>%
  filter(all(sales == 33.34))

#Create Control Group for 1976 through 1999 (Control Group #2)
Control_1999_Df <-  Set_Up_1976_1999 %>%
  group_by(State) %>%
  filter(all(sales == 33.34))
#added Mississippi, Utah, Vermont, Virginia, etc.

#Lastly, create control group of Just Midwest States that don't switch at all (Control Group #3), 
Control_Midwest_2007_Df <- Set_Up_1976_2007 %>%
  group_by(State) %>%
  filter(all(sales == 33.34)) %>%
  filter(State %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin"))
#only is KS and ND

#Create Combined Dataframes for the 3 control groups:
# Combine dataframes
combined_data_C1 <- bind_rows(Just_NE_2007, Control_2007_Df)

combined_data_C2 <- bind_rows(Just_NE_1999, Control_1999_Df)

combined_data_C3 <- bind_rows(Just_NE_2007, Control_Midwest_2007_Df)



# Create a binary variable for the post-period
combined_data_C1$Post_Period <- ifelse(combined_data_C1$Year > 1986, 1, 0)

# Create a binary variable for the interaction term
combined_data_C1$Interaction_Term <- combined_data_C1$Treatment * combined_data_C1$Post_Period

# Set up the DiD regression
did_model <- lm(Log_Revenue ~ Treatment + Post_Period + Interaction_Term + factor(Year) - 1, data = combined_data_C1)

# View the regression results
summary(did_model)

# Create a variable to distinguish between Treatment and Control
combined_data_C1$group <- ifelse(combined_data_C1$State == "Nebraska", "Treatment", "Control")



#Calloway set seed so everything is reproducible
set.seed(1814)

# Create a new column "StateNum" with numeric values for each state
combined_data_C1$StateNum <- as.numeric(factor(combined_data_C1$State, levels = unique(combined_data_C1$State)))


combined_data_C1$predicted <- predict(reg1)

# Create a binary treatment variable for Nebraska and phase-in years
 combined_data_C1$Treatment <- ifelse(combined_data_C1$State == "Nebraska", 1, 0)
 
 # Create a binary treatment variable for Nebraska and phase-in years
 combined_data_C2$Treatment <- ifelse(combined_data_C2$State == "Nebraska", 1, 0)
 
 # Create a binary treatment variable for Nebraska and phase-in years
 combined_data_C3$Treatment <- ifelse(combined_data_C3$State == "Nebraska", 1, 0) 
                    
                        
reg1=lm(Log_Revenue~Treatment+Post_Period+(Treatment*Post_Period ),data=combined_data_C1)
summary(reg1)


#This was correct and ran, NE to Never Switchers 2007
reg6 <- feols(Log_Revenue~ i(Year,Treatment,1987), data = combined_data_C1)
coefplot(reg6, ref.line=1987)

iplot(reg6, ref.line= c(1987,1992))


#This was correct and ran, NE to Never Switchers 1999
reg7 <- feols(Log_Revenue~ i(Year,Treatment,1987), data = combined_data_C2)
coefplot(reg6, ref.line=1987)

iplot(reg7, ref.line= c(1987,1992))


#This was correct and ran, NE to ND and KS
reg8 <- feols(Log_Revenue~ i(Year,Treatment,1987), data = combined_data_C3)
coefplot(reg8, ref.line=1987)

iplot(reg8, ref.line= c(1987,1992))

# Show the coefficient plot
print(coefplot)


## Call:
## lm(formula = work ~ post93 + anykids + post93 * anykids, data = eitc)
# Take average value of 'work' by year, conditional on anykids
minfo = aggregate(combined_data_C1$fitted, list(combined_data_C1$Year, combined_data_C1$Treatment==1), mean)
minfo = aggregate(combined_data_C1$Log_Revenue, list(combined_data_C1$Year, combined_data_C1$Treatment==1), std)
# rename column headings (variables)
names(minfo) = c("YR","Treatment", "LFPR")

# Attach a new column with labels
minfo$Group = "Never Switch to SSFA"
minfo$Group[33:64] = "Nebraska"
#minfo


qplot(YR, LFPR, data=minfo, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Log Revenue")+geom_vline(xintercept = 1987)+geom_vline(xintercept = 1992)


combined_data_C1$first_t <- ifelse(combined_data_C1$State == "Nebraska" & combined_data_C1$Year == 1986, 1, 0)

example_attgt <- att_gt(yname = "Log_Revenue",
                        tname = "Year",
                        idname = "StateNum",
                        gname = "first_t",
                        xformla = ~1,
                        data = combined_data_C1
)



