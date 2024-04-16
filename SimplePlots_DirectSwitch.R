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
setwd("~/Documents/GitHub/ST-Apportionment")

# Read in DF
Rev <- read.csv("Clean_Rev_Event_4_16_24.csv")
Diff <-read.csv("Diff_Log_df_4_16_24.csv")
Diff_rev <-read.csv("Diff_Log_4_16_24.csv")


#Create Plot, want the average_Log_CORPINCTX_c and std_err_c from "Diff_rev"

# Create the plot with Standard Error Bars
ggplot() +
  # Control Group plot
  geom_point(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), size = 3) +
  geom_errorbar(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, ymin = average_Log_CORPINCTX_c - std_err_c, ymax = average_Log_CORPINCTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_line(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), linetype = "dashed") +
  
  # Additional lines from Rev2 dataframe
  geom_line(data = Rev2 %>% filter(State_Acronym %in% c("NE", "CO", "RI", "DE", "ND")), aes(x = year, y = Log_CORPINCTX, color = State_Acronym)) +
  
  labs(title = "Average Log Corporate Income Tax by Year for Control Group",
       x = "Year",
       y = "Average Log Corporate Income Tax") +
  
  scale_color_manual(values = c("Control Group" = "blue", "NE" = "red", "CO" = "green", "RI" = "purple", "DE" = "orange", "ND" = "brown"),
                     labels = c("Control Group" = "Control", "NE" = "NE", "CO" = "CO", "RI" = "RI", "DE" = "DE", "ND" = "ND")) +
  
  guides(color = guide_legend(title = "Groups")) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Attempt with Vertical Lines
ggplot() +
  # Control Group plot
  geom_point(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), size = 4) +
  geom_errorbar(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, ymin = average_Log_CORPINCTX_c - std_err_c, ymax = average_Log_CORPINCTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_line(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), linetype = "dashed") +
  
  # Additional lines from Rev2 dataframe
  geom_line(data = Rev2 %>% filter(State_Acronym %in% c("NE", "CO", "RI", "DE", "ND")), aes(x = year, y = Log_CORPINCTX, color = State_Acronym)) +
  
  # Vertical lines
  geom_vline(aes(xintercept = c(1988, 2009, 2015, 2016, 2017)), linetype = "dashed", color = "gray40") +
  
  labs(title = "Average Log Corporate Income Tax by Year for Control Group",
       x = "Year",
       y = "Average Log Corporate Income Tax") +
  
  scale_color_manual(values = c("Control Group" = "blue", "NE" = "red", "CO" = "green", "RI" = "purple", "DE" = "orange", "ND" = "brown"),
                     labels = c("Control Group" = "Control", "NE" = "NE", "CO" = "CO", "RI" = "RI", "DE" = "DE", "ND" = "ND")) +
  
  scale_size_manual(values = c("Control Group" = 4, "NE" = 1, "CO" = 1.5, "RI" = 1, "DE" = 1.5, "ND" = 1.5), 
                    labels = c("4" = "Control", "1" = "NE = 1988, RI = 2015", "1.5" = "CO = 2009, DE = 2017, ND = 2016")) +
  
  guides(color = guide_legend(title = "Groups", 
                              override.aes = list(size = c(4, 1, 1.5, 1, 1.5,1.5), 
                                                  linetype = c("dashed", "solid", "solid", "solid", "solid","solid"),
                                                  color = c("blue", "red", "green", "purple", "orange", "brown")),
                              labels = c("Control", "NE", "CO", "RI", "DE", "ND"))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Try again
# Create the plot with Standard Error Bars
ggplot() +
  # Control Group plot
  geom_point(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), size = 4) +
  geom_errorbar(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, ymin = average_Log_CORPINCTX_c - std_err_c, ymax = average_Log_CORPINCTX_c + std_err_c, color = "Control Group"), width = 0.2) +
  geom_line(data = Diff_rev, aes(x = year, y = average_Log_CORPINCTX_c, color = "Control Group"), linetype = "dashed") +
  
  # Additional lines from Rev2 dataframe
  geom_line(data = Rev2 %>% filter(State_Acronym %in% c("NE", "CO", "RI", "DE", "ND")), aes(x = year, y = Log_CORPINCTX, color = State_Acronym)) +
  
  # Vertical lines
  geom_vline(aes(xintercept = c(1988, 2009, 2015, 2016, 2017)), linetype = "dashed", color = "gray40") +
  
  labs(title = "Average Log Corporate Income Tax by Year for Control Group",
       x = "Year",
       y = "Average Log Corporate Income Tax") +
  
  scale_color_manual(values = c("Control Group" = "blue", "CO" = "red", "NE" = "green", "RI" = "purple", "DE" = "orange", "ND" = "brown"),
                     labels = c("Control Group" = "CO", "CO" = "Control", "NE" = "NE", "RI" = "RI", "DE" = "DE", "ND" = "ND")) +
  
  scale_size_manual(values = c("Control Group" = 4, "NE" = 1, "CO" = 1.5, "RI" = 1, "DE" = 1.5, "ND" = 1.5), 
                    labels = c("4" = "Control", "1" = "NE = 1988, RI = 2015", "1.5" = "CO = 2009, DE = 2017, ND = 2016")) +
  
  guides(color = guide_legend(title = "Groups", 
                              override.aes = list(size = c(4, 1, 1.5, 1, 1.5, 1.5), 
                                                  linetype = c("dashed", "solid", "solid", "solid", "solid", "solid"),
                                                  color = c("blue", "red", "green", "purple", "orange", "brown")),
                              labels = c("CO", "Control", "NE", "RI", "DE", "ND"))) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
