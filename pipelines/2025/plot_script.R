
library(ggplot2)
library(data.table)

.args <- if (interactive()) {
  c(sprintf("target_%02i.csv", 1:4), "plot.png")
} else commandArgs(trailingOnly = TRUE)

head(.args, -1) |>
  lapply(fread) |>
  rbindlist(idcol = "index") -> dt

p <- ggplot(dt) + aes(x = x, y = y, color = factor(index)) +
  geom_line() +
  theme_minimal()

ggsave(
  p,
  filename = tail(.args, 1),
  width = 8, height = 6,
  bg = "white"
)
