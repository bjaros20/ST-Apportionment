# Loop for Dataframes- Short Run CI percentage change
#going to try and run the percentage change using Synthetic Control, not sDiD


#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(boot)
library(ggthemes)

install.packages('tidysynth')
# install.packages("devtools")
devtools::install_github("edunford/tidysynth")

# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

