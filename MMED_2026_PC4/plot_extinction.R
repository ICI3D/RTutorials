# plot_extinction.R
# Plot combined rabies extinction probability and times from merged RDS data

library(ggplot2)

# Parse inputs
.args <- if (interactive()) {
  c("merged_data.rds", "plot.png")
} else {
  commandArgs(trailingOnly = TRUE)
}

rds_file <- .args[1]
plot_file <- .args[2]

# Load the merged simulation results
df <- readRDS(rds_file)

# Calculate extinction probability and Clopper-Pearson 95% CIs per level
summary_list <- split(df, df$vaccination_level)
summary_df <- do.call(rbind, lapply(summary_list, function(sub_df) {
  n <- nrow(sub_df)
  x <- sum(sub_df$extinct)
  bt <- binom.test(x, n)
  data.frame(
    vaccination_level = sub_df$vaccination_level[1],
    extinction_prob = x / n,
    conf_low = bt$conf.int[1],
    conf_high = bt$conf.int[2],
    mean_extinction_time = mean(sub_df$extinction_time, na.rm = TRUE),
    median_extinction_time = median(sub_df$extinction_time, na.rm = TRUE)
  )
}))

# Prepare extinction time data subset (converting days to years)
df_extinct <- df[df$extinct, ]
df_extinct$metric <- "Time to Extinction (years)"
df_extinct$value <- df_extinct$extinction_time / 365
df_extinct$conf_low <- NA
df_extinct$conf_high <- NA
df_extinct <- df_extinct[, c(
  "vaccination_level", "metric", "value", "conf_low", "conf_high"
)]

# Prepare extinction probability data subset
df_prob <- summary_df
df_prob$metric <- "Extinction Probability"
df_prob$value <- df_prob$extinction_prob
df_prob <- df_prob[, c(
  "vaccination_level", "metric", "value", "conf_low", "conf_high"
)]

# Combine both facets into a single data frame
plot_data <- rbind(df_extinct, df_prob)

# Make a beautiful plot with two facets (free scales)
p <- ggplot(plot_data, aes(x = factor(vaccination_level), y = value)) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  # For Extinction Probability, we want a bar plot
  geom_bar(
    data = subset(plot_data, metric == "Extinction Probability"),
    stat = "identity", fill = "#3182bd", width = 0.6, alpha = 0.8
  ) +
  # Add binomial confidence intervals to Extinction Probability
  geom_errorbar(
    data = subset(plot_data, metric == "Extinction Probability"),
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.15, color = "grey20", linewidth = 0.6
  ) +
  # For Time to Extinction, we want a boxplot
  geom_boxplot(
    data = subset(plot_data, metric == "Time to Extinction (years)"),
    fill = "#de2d26", alpha = 0.6, color = "#a50f15", outlier.alpha = 0.5
  ) +
  labs(
    x = "Vaccination Level (Proportion)",
    y = NULL,
    title = "Rabies Extinction Analysis under Different Vaccination Levels",
    subtitle = "Based on 50 stochastic Gillespie simulation runs per vaccination level",
    caption = "Extinction is defined as E = 0 and I = 0. Simulation limit is 10 years (3650 days)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.spacing = unit(1.5, "lines")
  )

# Save combined plot output
ggsave(
  plot_file,
  plot = p,
  width = 8,
  height = 8,
  bg = "white"
)
