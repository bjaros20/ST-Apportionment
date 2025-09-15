#Reading Giroud DTA files

install.packages('haven')
library(haven)

data <- read_dta("state_tax_data.dta")