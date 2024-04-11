#Redo Callaway and Sant'Anna From Summer
library(readxl)
library(tidyr)
library(dplyr)
library(did)
library(tidyverse)
library(lmtest)
library(caret)
library(ggplot2)


#set working directory
setwd("~/Documents/GitHub/ST-Apportionment/Spring24_Workshop")

# Read in DF
Rev <- read_xls("FinalGardner_AprilWorkshop24.xls")

# Set treatment variable and time variable
treat_var <- "year_pass_intro"
time_var <- "year"

#Another try
Rev$state <- as.numeric(factor(Rev$state))

## estimate group-time average treatment effects for Log Revenue
LogCITRev_attgt <- att_gt(yname = "Log_CORPINCTX",
                        tname = "year",
                        idname = "state",
                        gname = "year_pass_intro",
                        data = Rev)

# summarize the results
summary(LogCITRev_attgt)
# plot the Group time Treatment for Log Revenue
ggdid(LogCITRev_attgt)
#Simple Aggregation
agg.simple <- aggte(LogCITRev_attgt, type = "simple")
summary(agg.simple)

# Dynamic Event Study Aggregation
agg.es <- aggte(LogCITRev_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

ggdid_plot <- ggdid(agg.es) +
  scale_x_continuous(limits = c(-10, 10))+
ggtitle("Event Study Plot (Dynamic ATT): Log CIT Revenue")+
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5))+
  geom_vline(xintercept = c(0, 1), linetype = "dashed")+
  ylab("Log Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

print(ggdid_plot)


## estimate group-time average treatment effects for Regular Revenue
CITRev_attgt <- att_gt(yname = "CORPINCTX",
                          tname = "year",
                          idname = "state",
                          gname = "year_pass_intro",
                          data = Rev)

# summarize the results
summary(CITRev_attgt)

#Simple Aggregation
agg.simpleRev <- aggte(CITRev_attgt, type = "simple")
summary(agg.simpleRev)

# Dynamic Event Study Aggregation- Regular Revenue
agg.esRev <- aggte(CITRev_attgt, type = "dynamic")
summary(agg.esRev)

ggdid_plotRev <- ggdid(agg.esRev) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-5000000, 5000000)) +  # Set y-axis limits
  ggtitle("Event Study Plot (Dynamic ATT): Corporate Income Tax Revenue") +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5)) +
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  ylab("Millions of Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

print(ggdid_plotRev)


## estimate group-time average treatment effects for Log Individual Income Tax Revenue
Log_IND_attgt <- att_gt(yname = "Log_INCTAX",
                       tname = "year",
                       idname = "state",
                       gname = "year_pass_intro",
                       data = Rev)

# summarize the results
summary(Log_IND_attgt)

#Simple Aggregation
agg.simpleInd <- aggte(Log_IND_attgt, type = "simple")
summary(agg.simpleInd)

# Dynamic Event Study Aggregation- Regular Revenue
agg.esInd <- aggte(Log_IND_attgt, type = "dynamic")
summary(agg.esInd)

ggdid_plotLog_Ind <- ggdid(agg.esRev) +
  scale_x_continuous(limits = c(-10, 10)) +
  ggtitle("Event Study Plot (Dynamic ATT): Log Individual Income Tax Revenue") +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5)) +
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  ylab("Log Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

print(ggdid_plotLog_Ind)

## estimate group-time average treatment effects for Regular Individual Income Tax Revenue
IND_attgt <- att_gt(yname = "INCTAX",
                        tname = "year",
                        idname = "state",
                        gname = "year_pass_intro",
                        data = Rev)

# summarize the results
summary(IND_attgt)

#Simple Aggregation
agg.simpleRInd <- aggte(IND_attgt, type = "simple")
summary(agg.simpleRInd)

# Dynamic Event Study Aggregation- Regular Revenue
agg.esRInd <- aggte(IND_attgt, type = "dynamic")
summary(agg.esRInd)

# Create the plot with ggdid and set title
ggdid_plotRInd <- ggdid(agg.esRInd) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_y_continuous(limits = c(-18000000, 18000000), 
                     labels = scales::comma_format()) +  # Format y-axis labels
  ggtitle("Event Study Plot (Dynamic ATT): Individual Income Tax Revenue") +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5),
        plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt"),  # Adjust margins
        axis.title.y = element_text(margin = margin(r = 15, unit = "pt"))) +  # Adjust distance from the plot
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  ylab("Millions of Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

# Print the plot
print(ggdid_plotRInd)


#run this once more for CIT Revenue w/ Not yet treated option:
## estimate group-time average treatment effects for Log Individual Income Tax Revenue
Log_C_NYT_attgt <- att_gt(yname = "Log_CORPINCTX",
                        tname = "year",
                        idname = "state",
                        gname = "year_pass_intro",
                        data = Rev,
                        control_group = "notyettreated")

# summarize the results
summary(Log_C_NYT_attgt )

#Simple Aggregation
agg.simpleNYTLog <- aggte(Log_C_NYT_attgt , type = "simple")
summary(agg.simpleNYTLog)

# Dynamic Event Study Aggregation- Regular Revenue
agg.esNYTLog <- aggte(Log_C_NYT_attgt, type = "dynamic")
summary(agg.esNYTLog)

ggdid_plotNYTLog <- ggdid(agg.esNYTLog) +
  scale_x_continuous(limits = c(-10, 10)) +
  ggtitle("Event Study Plot (Dynamic ATT): Log Corporate Income Tax Revenue, Treatment = Control via NYT") +
  theme(plot.title = element_text(color = "black", face = "bold", hjust = 0.5)) +
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  ylab("Log Dollars") +  # Label for y-axis
  xlab("Year in Reference to Treatment")  # Label for x-axis

print(ggdid_plotNYTLog)


# Create the plot
ggplot(Rev, aes(x = year, y = Log_CORPINCTX, color = group, group = group)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, aes(fill = group), alpha = 0.3) +
  labs(title = "Average Revenue by Treatment and Control Group",
       x = "Year",
       y = "Average Revenue") +
  theme_minimal()


#Create a New DF
# Calculate average CORPINCTX by year for group 'c' along with standard error
average_c <- Rev %>%
  filter(group == "c") %>%
  group_by(year) %>%
  summarise(average_CORPINCTX_c = mean(CORPINCTX),
            std_err_c = sd(CORPINCTX) / sqrt(n()))

# Calculate average CORPINCTX by year for group 't' along with standard error
average_t <- Rev %>%
  filter(group == "t") %>%
  group_by(year) %>%
  summarise(average_CORPINCTX_t = mean(CORPINCTX),
            std_err_t = sd(CORPINCTX) / sqrt(n()))

# Merge the two dataframes by year
average_df <- merge(average_c, average_t, by = "year", all = TRUE)

# Print the resulting dataframe
print(average_df)



# Create the plot

# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_CORPINCTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_CORPINCTX_c, ymin = average_CORPINCTX_c - std_err_c, ymax = average_CORPINCTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_CORPINCTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_CORPINCTX_t, ymin = average_CORPINCTX_t - std_err_t, ymax = average_CORPINCTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  labs(title = "Average Corporate Income Tax by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average CORPINCTX") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Group c", "Treatment Group" = "Group t")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal()

# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_line(aes(y = average_CORPINCTX_c, color = "Control Group"), size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_c - std_err_c, 
                  ymax = average_CORPINCTX_c + std_err_c, 
                  fill = "Control Group"), alpha = 0.3) +
  geom_line(aes(y = average_CORPINCTX_t, color = "Treatment Group"), size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_t - std_err_t, 
                  ymax = average_CORPINCTX_t + std_err_t, 
                  fill = "Treatment Group"), alpha = 0.3) +
  labs(title = "Average CORPINCTX by Year for Groups c and t",
       x = "Year",
       y = "Average CORPINCTX") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red")) +
  scale_fill_manual(values = c("Control Group" = "lightblue", "Treatment Group" = "pink"), guide = FALSE) +
  theme_minimal()

# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_line(aes(y = average_CORPINCTX_c, color = "Control Group"), size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_c - std_err_c, 
                  ymax = average_CORPINCTX_c + std_err_c, 
                  fill = "Control Group"), alpha = 0.3) +
  geom_line(aes(y = average_CORPINCTX_t, color = "Treatment Group"), size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_t - std_err_t, 
                  ymax = average_CORPINCTX_t + std_err_t, 
                  fill = "Treatment Group"), alpha = 0.3) +
  labs(title = "Average CORPINCTX by Year for Groups c and t",
       x = "Year",
       y = "Average CORPINCTX") +
  scale_color_manual(values = c("Control Group" = "black", "Treatment Group" = "gray30")) +
  scale_fill_manual(values = c("Control Group" = "lightgray", "Treatment Group" = "gray80"), guide = FALSE) +
  theme_minimal()



# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_point(aes(y = average_CORPINCTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(aes(y = average_CORPINCTX_c, ymin = average_CORPINCTX_c - std_err_c, ymax = average_CORPINCTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_point(aes(y = average_CORPINCTX_t, color = "Treatment Group"), size = 3) +
  geom_errorbar(aes(y = average_CORPINCTX_t, ymin = average_CORPINCTX_t - std_err_t, ymax = average_CORPINCTX_t + std_err_t, color = "Treatment Group"), width = 0.2) +
  geom_line(aes(y = average_CORPINCTX_c, color = "Control Group"), linetype = "dashed") +
  geom_line(aes(y = average_CORPINCTX_t, color = "Treatment Group"), linetype = "dashed") +
  labs(title = "Average Corporate Income Tax by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Corporate Income Tax in Millions") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Control", "Treatment Group" = "Treatment")) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_line(aes(y = average_CORPINCTX_c, color = "Control Group"), linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_c - std_err_c, 
                  ymax = average_CORPINCTX_c + std_err_c, 
                  fill = "Control Group"), alpha = 0.3) +
  geom_line(aes(y = average_CORPINCTX_t, color = "Treatment Group"), size = 1) +
  geom_ribbon(aes(ymin = average_CORPINCTX_t - std_err_t, 
                  ymax = average_CORPINCTX_t + std_err_t, 
                  fill = "Treatment Group"), alpha = 0.3) +
  labs(title = "Average Corporate Income Tax by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Corporate Income Tax in Millions") +
  scale_color_manual(values = c("Control Group" = "blue", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Control- is checkered line", "Treatment Group" = "Treatment")) +
  scale_fill_manual(values = c("Control Group" = "lightblue", "Treatment Group" = "lightpink"), guide = FALSE) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Plots Jonathan Wants- Wants the Purest Control Group