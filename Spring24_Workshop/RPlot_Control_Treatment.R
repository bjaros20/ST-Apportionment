#R Plots for finding Control and Treatment

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

# Create the plot
ggplot(average_df, aes(x = year)) +
  geom_segment(aes(x = year, xend = year, y = average_CORPINCTX_c - std_err_c, yend = average_CORPINCTX_c + std_err_c, color = "Control Group"), size = 1, linetype = "dashed") +
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
  scale_color_manual(values = c("Control Group" = "black", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Control", "Treatment Group" = "Treatment"),
                     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))) +
  scale_fill_manual(values = c("Control Group" = "lightgray", "Treatment Group" = "lightpink"), guide = FALSE) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

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
  labs(title = "Average Corporate Income Tax by Year for Control and Treatment Groups",
       x = "Year",
       y = "Average Corporate Income Tax in Millions") +
  scale_color_manual(values = c("Control Group" = "black", "Treatment Group" = "red"),
                     labels = c("Control Group" = "Control", "Treatment Group" = "Treatment")) +
  scale_fill_manual(values = c("Control Group" = "lightgray", "Treatment Group" = "lightpink"), guide = FALSE) +
  guides(color = guide_legend(title = "Groups")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.key = element_rect(fill = "white"),
    legend.linetype = c("solid", "dashed"),
    legend.key.size = unit(1, "cm")
  )
