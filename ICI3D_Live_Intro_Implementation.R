#####################################################################

## Live coding session: Introduction to model implementation in R

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: Jonathan Dushoff 2026
## Made from scratch as a live coding session, but based on work by Juliet Pulliam, Reshma Kassanjee and others

## 2025 version is here https://drive.google.com/file/d/1GFA1k7ckK5lvpml-05tW1u4xqE5EQrrJ/

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

## This is a live-coding session showing how to implement an SIR model in R

## GOAL: reproduce the work done in the spreadsheet, but in a way easier to check, extend and modify.

## Script should discuss units!!!!
## Need to specify levels so that S, I, R show up in order on plots

## Note: We ended up with an SIRS model, based on this diagram
## https://github.com/dushoff/SIR_model_family/blob/master/outputs/sirs.three.pdf

sirRates <- function(states, params){
	with(as.list(c(states, params)), {
		N <- S+I+R
		trans <- beta*S*I/N
		recov <- gam*I
		return(c(
			dS=-trans, dI=trans-recov, dR=recov
		))
	})
}

sirsRates <- function(states, params){
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

## Test our first rates function
sirRates(
	params = c(beta=0.5, gam=0.25, sig=0)
	, states = c(S=9999, I=1, R=0)
)

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

## Now running with a new function name
## (substituted sirsRates for sirsRates; could merge them by letting the waning parameter be zero to get an sir).
sim_wide <- stepSim(
	params = c(beta=0.5, gam=0.25, sig=0.01)
	, states = c(S=9999, I=1, R=0)
	, timeStep=1, finTime=300
	, ratefun = sirsRates
)

## Tested (as a code test) and set aside
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

## Next steps would be to make wrapper function to simulate and plot different models and parameters for easy comparison
