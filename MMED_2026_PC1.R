
# This Will Be a Rabies ODE Model

## -1. Load Stuff / Setup
library(deSolve)
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

N <- 1000
states_init <- c(
	S = N * 0.7, E = 0, I = 1, V = N * 0.3
)

dRabies <- function(time, states, params) with(
	c(as.list(states), params), {
	# param extraction
	N <- sum(states)
	
	# processes
	infections <- beta * (S/N) * I
	vaccinations <- vt * S
	deaths <- mu * states
	onsets <- sigma * E
	rabiesdeaths <- mu_r * I
	births <- sum(deaths) + rabiesdeaths
	
	# define state changes
	dS <- +births -infections -vaccinations -deaths["S"]
	dE <- +infections -deaths["E"] -onsets
	dI <- +onsets -deaths["I"] -rabiesdeaths
	dV <- +vaccinations -deaths["V"]
	
	return(list(
		c(dS, dE, dI, dV)
	))
})

## 2. Do Something! Run the simulation

timeSteps <- seq(0, 20 * days_per_year, by = 1)

result <- ode(states_init, timeSteps, dRabies, rabies_pars) |> 
	as.data.frame() |>
	pivot_longer(
		!time,
		names_to = "compartment"
	)

## 3. Look at Something - Plotting!

plotter <- function(res, N) ggplot(res) + aes(
	x = time/days_per_year, y = value / N, color = compartment
) + geom_line() + theme_minimal() +
	scale_y_continuous(
		"Proportion of Pop.",	transform = "logit",
		limits = c(0.001, .999), sec.axis = dup_axis()
	) + scale_x_continuous(
		"Sim. Year"
	) + scale_color_discrete(
		NULL, breaks = c("S", "E", "I", "V")
	) + theme(
		legend.position = "inside",
		legend.position.inside = c(1, 1), legend.justification.inside = c(1, 1),
		legend.direction = "horizontal"
	)

## 4. Bring it all together for rapid prototyping

test_function <- function(
	mu, beta, proportion_vaccinated, sigma, mu_r,
	years = 20,
	state_init = c(S = 700, E = 0, I = 1, V = 300),
	timeSteps = seq(0, years * days_per_year, by = 1)
) {
	N <- sum(state_init)
	pars <- list(
		mu = mu, beta = beta, vt = mu * proportion_vaccinated, sigma = sigma,
		mu_r = mu_r
	)
	ode(state_init, timeSteps, dRabies, pars) |> 
		as.data.frame() |>
		pivot_longer(
			!time,
			names_to = "compartment"
		) |> plotter(N)
}

test_function(
	rabies_pars$mu, rabies_pars$beta, percent_vaccinated,
	rabies_pars$sigma, rabies_pars$mu_r
)

# high background mortality
test_function(
	1 / (1 * days_per_year), rabies_pars$beta, percent_vaccinated,
	rabies_pars$sigma, rabies_pars$mu_r
)

# low background mortality
test_function(
	1 / (10 * days_per_year), rabies_pars$beta, percent_vaccinated,
	rabies_pars$sigma, rabies_pars$mu_r
)
