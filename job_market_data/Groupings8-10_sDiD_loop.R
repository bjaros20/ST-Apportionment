#Micah's August Update- ideas to complete result
# up until this point, no point estimates have been significantly different from 0.
# Will load packages for TWFE, sDiD, Synthetic Control, DiD.
#Have tried TWFE on Log, detrended Total real Revenue, Could try it on CIT
#DiD & Synthetic Control on 2007 adopters, retro-active adopters.
#sDiD on all pre 2021 (29 states) for CIT, real total revenue per capita,
#share CIT/Total Revenue, share CIT/CIT+Ind Income.

#Therefore, going to try a few things:
#(0) Share of nationwide CIT revenue. then try following.
#(1) Group by adoption.  Group early, middle and late adopters together, to see
#how timing differs.
#(2) Level adoption alone, group by treatment level, direct and step-wise,
#staggered by 10 percent, etc.

#Other ideas from Micah Conversation: Industry & Regional groupings of adopters.
#After all these revenue routes, go rout of getting labor share by industry.

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(did) # for running DiD
library(plm)
library(lmtest)
library(synthdid)
library(fixest)
library(boot)
library(ggthemes)

# set seed
set.seed(26)

