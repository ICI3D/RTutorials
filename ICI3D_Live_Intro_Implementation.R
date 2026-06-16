library(dplyr)
library(tidyr)

## This is a live-coding session showing how to implement an SIR model in R

## Script should discuss units!!!!
## Need to specify levels so that S, I, R show up in order on plots

## This version JD 2026
## 2025 version is here https://drive.google.com/file/d/1GFA1k7ckK5lvpml-05tW1u4xqE5EQrrJ/
## Need to track down earlier versions

## Goal: reproduce the work done in the spreadsheet, but in a way easier to check, extend and modify.

sirRates <- function(states, params){
	with(as.list(c(states, params)), {
		N <- S+I+R
		trans <- beta*S*I/N
		recov <- gam*I
		wane <- sig*R
		return(c(
			dS=wane-trans, dI=trans-recov, dR=recov-wane
		))
	})
}

## sirRates(
	## params = c(beta=0.5, gam=0.25, sig=0)
	## , states = c(S=9999, I=1, R=0)
## )

stepSim <- function(states, params, timeStep, finTime, ratefun){
	time <-0
	sim <- data.frame(as.list(c(time=time, states)))
	for (time in seq(timeStep, finTime, by=timeStep)){
		states <- states + timeStep*ratefun(params, states)
		sim <- bind_rows(sim
			, c(time=time, states)
		)
	}
	return(sim)
}

sim_wide <- stepSim(
	params = c(beta=0.5, gam=0.25, sig=0.01)
	, states = c(S=9999, I=1, R=0)
	, timeStep=1, finTime=300
	, ratefun = sirRates
)

## Tested and set aside
## simf <- (simf |> mutate(N=S+I+R))

sim_long <- (sim_wide
	|> pivot_longer(-time, names_to="compartment", values_to="indivs")
)

summary(sim_long)

library(ggplot2); theme_set(theme_bw(base_size=14))

print(ggplot(sim_long)
	+ aes(time, indivs, color=compartment)
	+ geom_line()
)
