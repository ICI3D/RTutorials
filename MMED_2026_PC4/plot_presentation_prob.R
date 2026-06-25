# plot_presentation_prob.R
# Plot widescreen presentation slide for rabies extinction probability from merged RDS data

library(ggplot2)

# Parse inputs
.args <- if (interactive()) {
  c("merged_data.rds", "presentation_prob.png")
} else {
  commandArgs(trailingOnly = TRUE)
}

rds_file <- .args[1]
plot_file <- .args[2]

# Load the merged simulation results
df <- readRDS(rds_file)

# Calculate extinction probability and confidence intervals per vaccination level
summary_list <- split(df, df$vaccination_level)
summary_df <- do.call(rbind, lapply(summary_list, function(sub_df) {
  n <- nrow(sub_df)
  x <- sum(sub_df$extinct)
  bt <- binom.test(x, n)
  data.frame(
    vaccination_level = sub_df$vaccination_level[1],
    extinction_prob = x / n,
    conf_low = bt$conf.int[1],
    conf_high = bt$conf.int[2]
  )
}))

# Plot probability slide with presentation-ready large fonts
p_prob <- ggplot(
  summary_df,
  aes(x = factor(vaccination_level), y = extinction_prob)
) +
  geom_bar(stat = "identity", fill = "#3182bd", width = 0.6, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.15, color = "grey20", linewidth = 0.8
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x = "Vaccination Level (Proportion)",
    y = "Extinction Probability",
    title = "Rabies Extinction Probability by Vaccination Level",
    subtitle = "95% Clopper-Pearson confidence intervals based on 50 simulation runs per level"
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
  plot = p_prob,
  width = 10,
  height = 5.625,
  bg = "white"
)
