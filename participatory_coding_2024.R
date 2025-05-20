
seiv_params <- list(
    ave_lifespan = 5*365, # days
    ave_rabies_duration = 10, # days
    beta = (2 / 7) * 0.5, # potential infectious interactions per dog per week
    N = 100000,
    ave_incubation_period = 6, # days
    ave_vaccination_period = 365, # days
    ave_vaccination_duration = 3 * 365 # days
)

seiv_pre_vax <- within(seiv_params, {
    ave_vaccination_period <- Inf
})

dogs_zero <- c(S = seiv_params$N - 1, E = 0, I = 1, V = 0)

seiv <- function(t, y, parms){
  with(c(as.list(y), parms), {
    death <- y * (1/ave_lifespan)
    disease_death <- I * (1/ave_rabies_duration)
    birth <- sum(death) + disease_death

    infection <- beta * S * I / N
    incubation <- 1 / (ave_incubation_period) * E
    vaccination <- 1 / (ave_vaccination_period) * S
    immunity_loss <- 1 / (ave_vaccination_duration) * V

    dSdt <- (birth + immunity_loss) - (infection + vaccination)
    dEdt <- (infection) - (incubation)
    dIdt <- (incubation) - (disease_death)
    dVdt <- (vaccination) - (immunity_loss)
    # Note: Population size is constant, so don't need to specify dRdt
    return(list(c(dSdt, dEdt, dIdt, dVdt) - death))
  })
}

library(deSolve)                # Load libary to be used for numerical integration

time.out <- seq(0, 5*365, 0.1)     # INCLUDE THE APPROPRIATE VALUE TO GET A SEQUENCE
                               # FROM 0 to 365 BY STEPS OF 0.1


ts_seiv_pv <- data.frame(lsoda(
  y = dogs_zero,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = seiv,                   # Function to evaluate
  parms = seiv_pre_vax                # Vector of parameters
))

with(
  ts_seiv_pv,
  plot(time, I, type = "l", xlab = "Time in days", ylab = "Number infected", main = "Canine Rabies in Tanzania", xlim = c(0, 5*365), bty = "n")
)

dogs_pv_zero <- (tail(ts_seiv_pv, 1) |> unlist())[-1]

ts_seiv <- tail(data.frame(lsoda(
        y = dogs_pv_zero,               # Initial conditions for population
        times = time.out,             # Timepoints for evaluation
        func = seiv,                   # Function to evaluate
        parms = seiv_params                # Vector of parameters
    )), 100)
ts_seiv$frac_vac <- 1

for (frac_vac in seq(.90, .10, by = -.1)) {
    local_params <- within(seiv_params, ave_vaccination_period <- ave_vaccination_period / frac_vac)
    dt <- tail(data.frame(lsoda(
        y = dogs_pv_zero,               # Initial conditions for population
        times = time.out,             # Timepoints for evaluation
        func = seiv,                   # Function to evaluate
        parms = local_params                # Vector of parameters
    )), 100)
    dt$frac_vac <- frac_vac
    ts_seiv <- rbind(ts_seiv, dt)
}



plot(ts_seiv$time,               # Time on the x axis
     ts_seiv$I,                  # Number infected (I) on the y axis
     xlab = "Time in days",     # Label the x axis
     ylab = "Number infected",  # Label the y axis
     main = "Canine Rabies in Tanzania",    # Plot title
     xlim = c(0, 5*365),           #
     type = "l",                # Use a line plot
     bty = "n")                 # Remove the box around the plot
