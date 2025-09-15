#AdjustSCIT for Inflation and Run Quantile Regression
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

#load documents
SCIT_Rev <- read_csv("SCIT_Rev_Long.csv")

Def <- read_excel("GDPDEF.xls",sheet=1)
Def2 <- read_excel("GDPDEF.xls",sheet=1)

#Clean Up Year for Deflator
install.packages("lubridate")
library(lubridate)

Def <- mutate(Def, Year = year(Year))

#Now just has year, merge on year
 
SCIT_Rev_Def <- inner_join(SCIT_Rev,Def)

#Adjust for Inflation

SCIT_Rev_Def$RealRev <-SCIT_Rev_Def$Revenue/SCIT_Rev_Def$GDPDEF*100

write.csv(SCIT_Rev_Def,"Real_SCIT_Rev.csv",row.names = FALSE)

#Now making it inflation per capita
Pop <-read_excel("State_Resident_Population.xls",sheet=1)

#Need to 

