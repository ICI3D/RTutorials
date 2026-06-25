# run_simulation.R
# Run multiple Gillespie simulations using a single initial reference population

source("model_functions.R")

# Parse command line arguments
.args <- if (interactive()) {
  c("0.4", "sim_0.4.csv")
} else {
  commandArgs(trailingOnly = TRUE)
}

# Extracts parameters from trailing arguments
vac_prob <- as.numeric(tail(.args, 2)[1])
out_csv <- tail(.args, 1)

# Set model parameters matching the MMED_2026_PC2.R reference script
days_per_year <- 365
days_per_week <- 7

rabies_pars <- list(
  mu = 1 / (3 * days_per_year), # 1 / life expectancy
  mu_r = 1 / 6, # 1 / rabies duration
  beta = 0.25, # infectious bites per day
  sigma = 1 / (3 * days_per_week)
)

# Set the vaccination rate vt based on the coverage probability argument
rabies_pars$vt <- rabies_pars$mu * vac_prob

# Initial state population matching the original MMED_2026_PC2.R reference pop
states_init <- c(
  S = 680,
  E = 20,
  I = 1,
  V = 300
)

# Set simulation size and maximum timeline limit
n_reps <- 50
time_max <- 10 * days_per_year # 10 years

# Use a unique seed per vaccination level for reproducibility of random draws
set.seed(42 + round(vac_prob * 1000))

# Execute the replicates using the optimized Gillespie runner
results <- replicate(n_reps, {
  res <- gillespie_run_opt(
    states_init = states_init,
    dEventsOpt = dRabies_opt,
    map_matrix = event_matrix,
    pars = rabies_pars,
    time_max = time_max
  )
  c(res$extinction_time, as.numeric(res$extinct))
})

# Construct and write output data frame
df <- data.frame(
  replicate = 1:n_reps,
  vaccination_level = vac_prob,
  extinction_time = results[1, ],
  extinct = as.logical(results[2, ])
)

write.csv(df, out_csv, row.names = FALSE)
