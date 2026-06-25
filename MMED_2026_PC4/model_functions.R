# Rabies model functions: extracted and optimized implementations

# ==============================================================================
# 1. Exact Extracted Functions from MMED_2026_PC2.R
# ==============================================================================

#' Event transition map (exact copy from MMED_2026_PC2.R)
#'
#' Defines state changes for each stochastic event in the rabies model.
event_map <- list(
  infection = c(S = -1, E = +1, I = 0, V = 0),
  onset = c(S = 0, E = -1, I = +1, V = 0),
  natural_death_S = c(S = 0, E = 0, I = 0, V = 0),
  natural_death_E = c(S = +1, E = -1, I = 0, V = 0),
  natural_death_I = c(S = +1, E = 0, I = -1, V = 0),
  natural_death_V = c(S = +1, E = 0, I = 0, V = -1),
  rabies_death = c(S = +1, E = 0, I = -1, V = 0),
  vaccination = c(S = -1, E = 0, I = 0, V = +1)
)

#' Sample an event based on rate probabilities
#'
#' @param event_rates A named numeric vector of current event rates.
#' @param map A list containing transition vectors for each event name.
#' @return A vector of state increments corresponding to the chosen event.
revent <- function(event_rates, map) {
  map[[sample(names(event_rates), size = 1, prob = event_rates)]]
}

#' Draw stochastic time increment from rate sum
#'
#' @param rates A numeric vector of event rates.
#' @return A single numeric value drawn from an exponential distribution.
rtime <- function(rates) {
  rexp(1, sum(rates))
}

#' Rate calculation function for standard Gillespie (exact copy from MMED_2026_PC2.R)
#'
#' @param time Current simulation time.
#' @param states A named numeric vector of compartments (S, E, I, V).
#' @param params A list of parameters (mu, mu_r, beta, sigma, vt).
#' @return A named numeric vector of transition rates.
dRabies <- function(time, states, params) {
  with(
    c(as.list(states), params),
    {
      N <- sum(states)

      infections <- beta * (S / N) * I
      vaccinations <- vt * S
      deaths <- mu * states
      onsets <- sigma * E
      rabiesdeaths <- mu_r * I

      return(c(
        infection = infections,
        onset = onsets,
        natural_death_S = deaths[["S"]],
        natural_death_E = deaths[["E"]],
        natural_death_I = deaths[["I"]],
        natural_death_V = deaths[["V"]],
        rabies_death = rabiesdeaths,
        vaccination = vaccinations
      ))
    }
  )
}

#' Original Gillespie simulation runner (exact copy from MMED_2026_PC2.R)
#'
#' @param states_init A named numeric vector of initial compartment sizes.
#' @param dEvents Function to compute event rates.
#' @param map List mapping event names to transition vectors.
#' @param pars List of model parameters.
#' @param time_max Maximum time duration for the simulation.
#' @param n Number of stochastic replicates to run.
#' @return A data frame containing time-series outputs for all replicates.
gillespie <- function(states_init, dEvents, map, pars, time_max, n) {
  outputs <- list()
  for (i in seq_len(n)) {
    time <- 0
    states <- states_init
    outputs[[i]] <- list()
    outputs[[i]][[1]] <- data.frame(
      time = 0,
      S = states_init[["S"]],
      E = states_init[["E"]],
      I = states_init[["I"]],
      V = states_init[["V"]]
    )

    while (time < time_max) {
      rates <- dEvents(time, states, pars)
      event <- revent(rates, map)
      states <- states + event
      time <- time + rtime(rates)

      outputs[[i]][[length(outputs[[i]]) + 1]] <- data.frame(
        time = time,
        S = states["S"],
        E = states["E"],
        I = states["I"],
        V = states["V"]
      )
    }
    outputs[[i]] <- do.call(rbind, outputs[[i]])
    outputs[[i]]$sample_id <- i
  }

  return(do.call(rbind, outputs))
}

# ==============================================================================
# Optimized implementations for performance in high-throughput runs
# ==============================================================================

#' Event transition matrix (optimized)
#'
#' Matrix equivalent of event_map for fast offset-based vector addition without list indexing.
event_matrix <- matrix(c(
  -1,  1,  0,  0, # infection
   0, -1,  1,  0, # onset
   0,  0,  0,  0, # natural_death_S
   1, -1,  0,  0, # natural_death_E
   1,  0, -1,  0, # natural_death_I
   1,  0,  0, -1, # natural_death_V
   1,  0, -1,  0, # rabies_death
  -1,  0,  0,  1  # vaccination
), nrow = 8, byrow = TRUE)

#' Highly optimized rate calculation function
#'
#' Avoids environment dynamic creation (such as with() or as.list()) for maximum speed.
#'
#' @param time Current simulation time.
#' @param states A numeric vector of compartment sizes (S = states[1], E = states[2], etc.).
#' @param params A list of parameters (mu, mu_r, beta, sigma, vt).
#' @return A numeric vector of 8 transition rates.
dRabies_opt <- function(time, states, params) {
  S <- states[1]
  E <- states[2]
  I <- states[3]
  V <- states[4]
  N <- S + E + I + V

  beta <- params$beta
  vt <- params$vt
  mu <- params$mu
  sigma <- params$sigma
  mu_r <- params$mu_r

  return(c(
    beta * (S / N) * I,  # infection
    sigma * E,           # onset
    mu * S,              # natural_death_S
    mu * E,              # natural_death_E
    mu * I,              # natural_death_I
    mu * V,              # natural_death_V
    mu_r * I,            # rabies_death
    vt * S               # vaccination
  ))
}

#' Highly optimized single Gillespie run with early extinction checks
#'
#' Terminate early if the infectious/exposed compartments drop to zero, saving millions of iterations.
#'
#' @param states_init A numeric vector of initial compartment sizes.
#' @param dEventsOpt Function to compute optimized transition rates.
#' @param map_matrix Matrix mapping event indices to transition vectors.
#' @param pars List of parameters.
#' @param time_max Maximum time duration for the simulation.
#' @return A list containing extinction_time (numeric or NA) and extinct (logical).
gillespie_run_opt <- function(states_init, dEventsOpt, map_matrix, pars, time_max) {
  time <- 0
  states <- states_init

  while (time < time_max) {
    # Check if rabies has gone extinct (E compartment states[2] and I compartment states[3])
    if (states[2] == 0 && states[3] == 0) {
      return(list(extinction_time = time, extinct = TRUE))
    }

    rates <- dEventsOpt(time, states, pars)
    sum_rates <- sum(rates)
    if (sum_rates <= 0) {
      break
    }

    # Draw event using fast integer index sampling
    event_idx <- sample.int(8, size = 1, prob = rates)
    states <- states + map_matrix[event_idx, ]
    time <- time + rexp(1, sum_rates)
  }

  # Final check for extinction status at boundary
  if (states[2] == 0 && states[3] == 0) {
    return(list(extinction_time = time, extinct = TRUE))
  } else {
    return(list(extinction_time = NA, extinct = FALSE))
  }
}
