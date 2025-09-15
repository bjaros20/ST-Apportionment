#cuts from Dougan

#try sales as numeric variable
sales_reg <- lm(log_totrev ~ sales + factor (year)+ factor(state) + rates, Res)
summary(sales_reg)
#after running the above regression, got the following result for sales
# sales             0.0006130  0.0002465   2.487 0.012958 *  
# calculate the transform. .000613 is the log revenue in thousands, it is the B
# e^B -1 = percentage change from increasing sales factor
#e^.000613 - 1 = .000613 * 100= 0.0613 %, but that increase on billions is significant
# this is on the weight of sales factor, so is multiplied by that scalar


#For comparison, will try with log sales, #log rate will not run
log_sales_reg <- lm(log_totrev ~ log(sales) + factor (year)+ factor(state) + rates, Res)
summary(log_sales_reg)
# result for log(sales)        0.05897    0.01525   3.868 0.000113 ***

#Lost the state names in the Regression, creating a dataframe to merge them back
# Define the state acronyms and their corresponding state names
state_acronyms <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", 
                    "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
                    "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", 
                    "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", 
                    "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

state_names <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Iowa", 
                 "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", 
                 "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", 
                 "Missouri", "Mississippi", "Montana", "North Carolina", "North Dakota", 
                 "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", 
                 "New York", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                 "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                 "Virginia", "Vermont", "Washington", "Wisconsin", "West Virginia", 
                 "Wyoming")

# Create the dataframe
df <- data.frame(State_Acronym = state_acronyms, State_Name = state_names)

# Display the dataframe
print(df)
