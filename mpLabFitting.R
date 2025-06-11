## MMED Lab of some sort
## (C) ICI3D; some rights reserved

## Steven Walker 2025
## Jonathan Dushoff 2025
## See also https://github.com/canmod/macpan2/tree/main/inst/starter_models/hiv for the full Granich model with treatmeant

library(macpan2)

######################################################################

## Specify helper relationships and then flows; use them to make a model “specification”

fn = list(
		I ~ I1 + I2 + I3 + I4
	, N ~ S + I
	, P ~ I / N
	, lambda ~ lambda0 * exp(-alpha * P)
)

flows = list(
		mp_per_capita_flow("S", "I1", "lambda * I/N", "infection")
	, mp_per_capita_flow("I1", "I2", "rho", "progression1")
	, mp_per_capita_flow("I2", "I3", "rho", "progression2")
	, mp_per_capita_flow("I3", "I4", "rho", "progression3")
	, mp_per_capita_flow("I4", "D", "rho", "deathHIV")
	, mp_per_capita_outflow("S", "mu", "deathS")
	, mp_per_capita_outflow("I1", "mu", "deathI1")
	, mp_per_capita_outflow("I2", "mu", "deathI2")
	, mp_per_capita_outflow("I3", "mu", "deathI3")
	, mp_per_capita_outflow("I4", "mu", "deathI4")
	, mp_per_capita_inflow("N", "S", "beta", "birth")
)

hiv4spec = mp_tmb_model_spec(
		during = c(fn, flows)
)

######################################################################

## Pick some parameters and create an implementation of our specification

## parameters: all rates are per year.
## Inspired by Kretzschmar et al. 2013
## https://www.pnas.org/doi/full/10.1073/pnas.1301801110
params = list(
	# per-capita demographics
		mu = 0.018	 ## death rate
	, beta = 0.02	## birth rate

	# per-capita progression rates
	, rho = mean(c(1/0.271, 1/8.31, 1/1.184, 1/1.316)) ## untreated

	, lambda0 = 0.65	## baseline transmission rate
	, alpha = 1 ## non-linearity parameter

	# initial conditions
	, S = 1 ## susceptible
	, I1 = 1/400, I2 = 1/400, I3 = 1/400, I4 = 1/400 ## infectious per stage
	, D = 0		 ## cumulated death due to disease
)

## How would you calculate R0 for this model?
## FIXME

hiv4impl = (hiv4spec
	|> mp_tmb_update(default = params)
	|> mp_rk4()
	|> mp_simulator(time_steps = 50L
		, outputs=c("S", "I", "N", "P")
	)
)

######################################################################

## Simulate a trajectory; this is a deterministic model, so we should only need to do it once

baseSim <- (hiv4impl
	|> mp_trajectory()
)

head(baseSim) ## TEMP

## TASK: Examine baseSim in Rstudio.

## It might be good to use dplyr to manipulate it.
## Usually it's good to put library functions at the beginning of scripts to crash out early, but we're making an exception

library(dplyr)
basePrev <- (baseSim 
	|> filter(matrix=="P") 
	|> rename(prevalence=value)
)

## And now we want to use ggplot to plot these structured objects
library(ggplot2); theme_set(theme_bw())

## Modelers have historically looked at prevalence here
print(ggplot(basePrev)
	+ aes(time, prevalence)
	+ geom_line()
)

## But the inferred population effects are quite dramatic
baseState <- (baseSim 
	|> filter(matrix!="P") 
	|> rename(proportion=value, state=matrix)
)

## Showing state variables as a proportion of the _starting_ population size
print(ggplot(baseState)
	+ aes(time, proportion, color=state)
	+ geom_line()
)

quit()
	|> mutate(matrix = sub("^A([1-4])$", "Infectious and treated, stage \\1", matrix))
	|> mutate(matrix = sub("^I([1-4])$", "Infectious and untreated, stage \\1", matrix))
	|> ggplot()
	+ geom_line(aes(time, value))
	+ facet_wrap(~ matrix, ncol = 2, scales = 'free', dir = "v")
	+ scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
	+ theme_bw()
)
