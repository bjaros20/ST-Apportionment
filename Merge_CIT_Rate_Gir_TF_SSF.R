#Load CIT Rates and Merge with Master Rev
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("did")
install.packages("tidyverse")
install.packages("lmtest")
install.packages('haven')
library(readxl)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)
library(haven)

#set working directory
setwd("~/Documents/SALT Directory/MyContent_stlouisfed")

data <- read_dta("stata_tax_data.dta")

write.csv(data, "Giroud.csv", row.names = TRUE)