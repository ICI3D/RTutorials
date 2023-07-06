
library(data.table)

.args <- commandArgs(trailingOnly = TRUE)

grid <- expand.grid(
    hC = seq(0.1, 1, by = 0.1),
    treatreduction = seq(0.5, 3, by = 0.5)
)

fwrite(grid, tail(.args, 1), sep = " ", col.names = FALSE)