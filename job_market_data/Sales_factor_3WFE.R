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