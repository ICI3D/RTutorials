# plot_presentation_time.R
# Plot widescreen presentation slide for rabies time to extinction from merged RDS data

library(ggplot2)

# Parse inputs
.args <- if (interactive()) {
  c("merged_data.rds", "presentation_time.png")
} else {
  commandArgs(trailingOnly = TRUE)
}

rds_file <- .args[1]
plot_file <- .args[2]

# Load the merged simulation results
df <- readRDS(rds_file)

# Extract only runs that went extinct and convert days to years
df_time <- df[df$extinct, ]
df_time$extinction_time_years <- df_time$extinction_time / 365

# Plot time-to-extinction boxplots with presentation-ready large fonts
p_time <- ggplot(
  df_time,
  aes(x = factor(vaccination_level), y = extinction_time_years)
) +
  geom_boxplot(
    fill = "#de2d26", alpha = 0.6, color = "#a50f15",
    outlier.alpha = 0.5, outlier.size = 2.5
  ) +
  labs(
    x = "Vaccination Level (Proportion)",
    y = "Time to Extinction (years)",
    title = "Rabies Time to Extinction by Vaccination Level",
    subtitle = "Excludes runs that did not go extinct within 10 years"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "grey40"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    panel.grid.minor = element_blank()
  )

# Save widescreen output image
ggsave(
  plot_file,
  plot = p_time,
  width = 10,
  height = 5.625,
  bg = "white"
)
