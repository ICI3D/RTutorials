# This Will Be a Rabies ODE Model

## -1. Load Stuff / Setup
library(ggplot2)
library(tidyr)

## 0. Write Out My Model in Plain Language
## Processes, Parameters
# birth (nu), death (natural, mu & rabies, mu_r),
# transmission (beta), progression/onset (sigma)
# vaccination (vt)
## States / Compartments
# S -> E -> I, S -> V

## 1. Define Some Variables*
## Initial Variables
## Define THE function
# death (natural, mu & rabies, mu_r),
# transmission (beta), progression/onset (sigma)
# vaccination (vt)
## States / Compartments
# S -> E -> I, S -> V

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

days_per_year <- 365
days_per_week <- 7
days_per_month <- 30
percent_vaccinated <- 0.4

rabies_pars <- list(
	mu = 1 / (3 * days_per_year), # 1 / life expectancy
	mu_r = 1 / 6, # 1 / rabies duration
	beta = 0.25, # infectious bites per day
	sigma = 1 / (3 * days_per_week)
)

rabies_pars$vt = rabies_pars$mu * percent_vaccinated

states_init <- c(
	S = 680,
	E = 20,
	I = 1,
	V = 300
)

## Todo Gillespie model need ...
##  - need to draw time step
##  - need to identify events
##  - need to draw event
##  - determine total rate
##  - determine specific event relative rate

# my event_rates should be a named vector of the rates
revent <- function(event_rates, map) {
	map[[sample(names(event_rates), size = 1, prob = event_rates)]]
}

rtime <- function(rates) {
	rexp(1, sum(rates))
}

dRabies <- function(time, states, params) {
	with(
		c(as.list(states), params),
		{
			# param extraction
			N <- sum(states)

			# processes
			infections <- beta * (S / N) * I
			vaccinations <- vt * S
			deaths <- mu * states
			onsets <- sigma * E
			rabiesdeaths <- mu_r * I
			# births <- sum(deaths) + rabiesdeaths

			## TODO need to change this to be event rates
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

## 2. Do Something! Run the simulation

## TODO implement Gillespie loop
## TODO: switch to sampling the simulation

dRabies(0, states_init, rabies_pars)

gillespie <- function(
	states_init,
	dEvents,
	map,
	pars,
	time_max,
	n
) {
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

result <- gillespie(
	states_init,
	dRabies,
	event_map,
	rabies_pars,
	5 * days_per_year,
	n = 10
) |>
	as.data.frame() |>
	pivot_longer(
		-c(time, sample_id),
		names_to = "compartment"
	)

## 3. Look at Something - Plotting!

## TODO adjust this to account for multiple sample time series

plotter <- function(res) {
	ggplot(res) +
		aes(
			x = time / days_per_year,
			y = value,
			color = compartment,
			group = interaction(sample_id, compartment)
		) +
		geom_line() +
		theme_minimal() +
		scale_y_continuous(
			"Count of Pop."
		) +
		scale_x_continuous(
			"Sim. Year"
		) +
		scale_color_discrete(
			NULL,
			breaks = c("S", "E", "I", "V")
		) +
		theme(
			legend.position = "inside",
			legend.position.inside = c(1, 1),
			legend.justification.inside = c(1, 1),
			legend.direction = "horizontal"
		)
}

## 4. Bring it all together for rapid prototyping

## TODO maybe adjust this to dealing with sampling

test_function <- function(
	mu,
	beta,
	proportion_vaccinated,
	sigma,
	mu_r,
	years = 20,
	state_init = c(S = 680, E = 20, I = 1, V = 300),
	n = 10
) {
	pars <- list(
		mu = mu,
		beta = beta,
		vt = mu * proportion_vaccinated,
		sigma = sigma,
		mu_r = mu_r
	)
	gillespie(
		state_init,
		dRabies,
		event_map,
		pars,
		years * days_per_year,
		n = n
	) |>
		as.data.frame() |>
		pivot_longer(
			-c(time, sample_id),
			names_to = "compartment"
		) |>
		plotter()
}

test_function(
	rabies_pars$mu,
	rabies_pars$beta,
	percent_vaccinated,
	rabies_pars$sigma,
	rabies_pars$mu_r
)

# high background mortality
test_function(
	1 / (1 * days_per_year),
	rabies_pars$beta,
	percent_vaccinated,
	rabies_pars$sigma,
	rabies_pars$mu_r
)

# low background mortality
test_function(
	1 / (10 * days_per_year),
	rabies_pars$beta,
	percent_vaccinated,
	rabies_pars$sigma,
	rabies_pars$mu_r
)

# Graveyard

test_event_rates <- c(
	infection = 10,
	onset = 5,
	natural_death_S = 1,
	natural_death_E = 1,
	natural_death_I = 1,
	natural_death_V = 1,
	rabies_death = 1,
	vaccination = 1
)

revent(test_event_rates, event_map)
