#plot Federal Naive Corporate Income to State Naive Corporate Income

#Load Packages
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(boot)
library(ggthemes)
library(lubridate)


# set seed
set.seed(26)
#JMP Directory
setwd("~/Documents/GitHub/ST-Apportionment/job_market_data")

#Load data
NCI <- read.csv("real_NCI_cap.csv")
fed_data <-read.csv("FCTAX.csv")


# Convert the DATE column to a Date class
fed_year <- fed_data %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%y"),
         year = ifelse(year(DATE) >= 2024, year(DATE) - 100, year(DATE)))

fed_year2 <-fed_year %>%
  filter(year> 1975)

filt_year <- fed_year2 %>%
  select(FCTAX,rate,year)

#create naive_fed_ci
naive_fci <-filt_year %>%
  mutate(naive_fed_ci = ((FCTAX/rate)*100))

bill_naive_fci <- naive_fci  %>%
  mutate(bill_nfci = naive_fed_ci * 1000000000)


#Get state corporate Naive income
state_nci <- NCI %>%
  group_by(State_Acronym) %>%
  select(year,Ann_CORPINC,CPI_def) %>%
  filter(State_Acronym == "AL")
  ungroup()
  

state_nci2 <-state_nci %>%
  select(year,Ann_CORPINC,CPI_def) %>%
  mutate(naive_sci = Ann_CORPINC * 1000)

# left join
join <- bill_naive_fci %>%
  full_join(state_nci2, by = c("year")) %>%
  select(-State_Acronym)


#create real Federal and State Naive CI, Base year is 1983-1984
real_ci <- join %>%
  mutate(real_fci = (bill_nfci/CPI_def)*100) %>%
  mutate(real_sci = (naive_sci/CPI_def)*100)

# Raw Federal CI v State CI
plot1 <- ggplot(real_ci, aes(x = year)) +
  # geom_vline(xintercept = year_effective_first, color = "black", linewidth = .75) +
  geom_line(aes(y = real_fci, color = "Federal CI"), size = 1.2) +
  geom_line(aes(y = real_sci, color = "State CI"), linetype = "dotdash", size = 1.2) +
  labs(title = "Federal vs. Total State Naive Corporate Income",
       x = "Year",
       y = "Real Corporate Income ($)") +
  scale_color_manual(values = c("Federal CI" = "red", "State CI" = "blue"),
                     labels = c("Federal CI" = "Federal CI", "State CI" = "State CI")) +
  guides(color = guide_legend(title = "Corporate Income Type")) +
  theme_stata()

print(plot1)

write.csv(real_ci,"fed_state_ci.csv",row.names=FALSE)
