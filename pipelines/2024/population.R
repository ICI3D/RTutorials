
# create a population
# use that population to create a sample
# evaluate the population outcome

library(data.table)

.args <- if (interactive()) c(
  "data/population.rds"
) else commandArgs(trailingOnly = TRUE)

popn <- 1000L
numStudies <- 250L

treatments <- factor(c("old", "new"), levels = c("old", "new"))

dt <- seq_len(numStudies) |>
  lapply(
    \(n) data.table(
      treatment = sample(treatments, popn, replace = TRUE),
      study_id = n
    )[, patient_id := seq_len(.N)]
  ) |> rbindlist()

dt[, deviate := runif(.N)]

target_file <- tail(.args, 1)

saveRDS(dt, target_file)