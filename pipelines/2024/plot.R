
library(data.table)
library(ggplot2)

.args <- if (interactive()) c(
    list.files("data", pattern = "analysis_.*\\.rds$", full.names = TRUE),
    "plot.png"
) else commandArgs(trailingOnly = TRUE)

plot_dt <- head(.args, -1) |> lapply(
    \(fn) {
        effs <- strsplit(fn, "_")[[1]] |> tail(2) |> gsub("\\.rds$", "", x = _) |> as.numeric()
        readRDS(fn)[, c("oldeff", "neweff") := .(effs[1], effs[2])]
    }
) |> rbindlist()

p <- ggplot(plot_dt) + aes(
    x = oldeff, y = lwr, color = factor(neweff)
) + geom_point(alpha = 0.1) + theme_minimal()


ggsave(tail(.args, 1), p, bg = "white")