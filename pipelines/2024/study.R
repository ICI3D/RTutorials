
## See notes in study_design.R

library(data.table)

.args <- if (interactive()) c(
  "data/population.rds",
  "0.5", "0.8",
  "data/study.rds"
) else commandArgs(trailingOnly = TRUE)

source_file <- .args[1]
oldeff <- as.numeric(.args[2])
neweff <- as.numeric(.args[3])

dt <- readRDS(source_file)

dt[, treatment_eff := fifelse(treatment == "new", neweff, oldeff) ]
dt[, suppressed := as.logical(deviate < treatment_eff) ]

saveRDS(dt, tail(.args, 1))