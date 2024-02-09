#Attempt to Get DiD_ Nebraska
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
setwd("~/Documents/GitHub/ST-Apportionment")

#Load Cleaned Set-up
Clean_Set_up <-read.csv("NE_DiD_Set_up.csv")

#Remove Region and Number
Clean_Set_up <- subset(Clean_Set_up, select = -c(Region, Number))

#Sort Into TreatMent and Control Groups
Just_NE <- subset(Clean_Set_up, State == "Nebraska")

#Create Control Group
Control_Df <- subset(Clean_Set_up, sales == 33.34)
