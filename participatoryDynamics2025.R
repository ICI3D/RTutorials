
## Added some comments after the session

######################################################################

## You should probably use a better-supported work flow than mine (JD)
## But there are things you can learn from it:
#### Run your code frequently from the beginning in a clean environment
###### In rstudio you can do this with shortcuts for Session/restart_R
###### and Code/Region/Run_all (recommend to learn keyboard shortcuts)
#### Break long flows into files to allow you to save your state in an externally viewable, stable form
#### and to mix and match steps, easily control what you want to run when

######################################################################

## State variables: S, E, I, V
## Processes: Infection, mortality (two kinds), birth, waning immunity, vaccination, progression
## Parameters
## beta, mu_b, mu_d, gamma (progression rate), eps (waning rate)
## tau (vacc rate), b (birth flow)
## We will use lam for force of infection

## What are some things we're worried about leaving out?
## Other interventions, like sterilization
## Heterogeneity (maybe let beta decline with prevalence)
## Whether beta is changing with N (beta should decrease if N crashes)
## Immigration

######################################################################

library(deSolve)
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=14))

## Weird units trick
## You can completely skip this (but then you need to use one quantity per dimension)
#### e.g., you can't use both day and year
## OR just set the base units to 1 until you're pretty convinced it's going to work and then push from there
## OR use it as I did to enforce careful development
dog <- 0.3
day <- 2.7

year <- 365.25*day

## A function that takes state vars and parameters and returns flows

rabiesRates <- function(t, vars, params){
	with(c(as.list(vars), as.list(params)), {
		## quantities
		N <- S+E+I+V
		lam <- beta*I/N

		## process flows
		birth <- b
		infection <- lam*S
		vacc <- tau*S
		progress <- gam*E
		disDeath <- mu_d*I
		waning <- eps*V
		## bgDeath ## annoying

		## compartment flows (DO bgDeath later)
		cf <- c(
			dS = birth - infection - vacc + waning
			, dE = infection - progress
			, dI = progress - disDeath
			, dV = vacc - waning
		)

		## Apply natural death (to the whole vector)
		return(list((cf - mu_b*vars)))

		## WARNING: In deSolve _you_ are responsible for the order of state 
		## variables
	})
}

rabiesSim <- lsoda(
	times = seq(0, 10*year, by=year/12)
	, y = c(S=50000-10, E=10, I = 0, V=0) * dog
	, parms = c(
		b = 10000*dog/year
		, beta = 0.5/day
		, tau = 0.1/year
		, gam = 1/(60*day)
		, mu_d = 1/(3*day)
		, eps = 1/(2*year)
		, mu_b = 1/(4*year)
	)
	, func = rabiesRates
)

## Watch out! lsoda is returning a matrix, which can be cool
## but also will break your brain if you don't remember

rabiesLong <- (rabiesSim
	|> as.data.frame()
	|> pivot_longer(cols=c(-time), values_to="dogs", names_to="compartment")
	|> mutate(dogs=dogs/dog) ## dogs is a value; dog is a unit
)

print(rabiesLong)

pic <- (ggplot(rabiesLong)
	+ aes(time/year, dogs, color=compartment)
	+ xlab("time in years")
	+ geom_line()
)

print(pic)
print(pic + scale_y_log10())
