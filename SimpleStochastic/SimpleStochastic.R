## Simple stochastic models: computer exercise
## International Clinics on Infectious Disease Dynamics and Data Program
##
## Juliet R.C. Pulliam, 2016
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)
##
## BASED ON an Excel tutorial developed by Alex Welte (2012)
## Clinic on Dynamical Approaches to Infectious Disease Data
## University of Florida, Gainesville, FL, USA
##
rm(list=ls())

# This tutorial simulates a simple population model
# where mortality is the only process. The differential
# equation version of the model is:
#            dN/dt = - mortalityRate * N
# where N is the population size at a given time.

# Define a function that calculates the population
# at time t from the analytical solution to the
# differential equation

analytic <- function(initial = initialPopulationSize,
										 rate = mortalityRate,
										 times = seq(0,maxTime,timeStep),plot=T){
	ts <- data.frame(time = times,populationSize = initial * exp(-rate*times))
	lines(ts$time,ts$populationSize,type='l')
	return(ts)
}

# Define a function that calculates the population
# at time t from the discrete time approximation to
# the differential equation

discreteTime <- function(initial = initialPopulationSize,
												 rate = mortalityRate,
												 stepSize = timeStep,maxT=maxTime,plot=T){
	tt <- 0
	ts <- data.frame(time = tt,populationSize = initial)
	step <- 1
	while(tt<maxT){
		ts <- rbind(ts,
								c(ts$time[step]+stepSize,ts$populationSize[step]-rate*ts$populationSize[step]*stepSize))
		step <- step+1
		tt <- tt+timeStep
	}
	lines(ts$time,ts$populationSize,type='l',col='darkgreen')
	return(ts)
}

# Define a function that calculates the population
# at time t from a stochastic simulation of the
# process represented by the differential equation
# model

individual <- function(initial = initialPopulationSize,
											 rate = mortalityRate,
											 stepSize = timeStep,maxT=maxTime,plot=T){
	deathTimes <- rexp(initial,rate)
	ts <- data.frame(time = 0,populationSize = initial)
	for(tt in seq(stepSize,maxT,stepSize)){
		ts <- rbind(ts,
								c(time = tt,populationSize = sum(deathTimes>=tt)))
	}
	lines(ts$time,ts$populationSize,type='l',col='darkred')
	return(ts)
}

# Set the parameter values

initialPopulationSize <- 10 # number of individuals
mortalityRate <- 0.05       # per capital deaths per day
timeStep <- 1								# days
maxTime <- 30								# days

# This part of the code sets up the axes for plotting

par(bty='L',lwd=3,mar=c(4,4,1,1))
plot(NA,NA,ylim=c(0,initialPopulationSize),xlim=c(0,maxTime),
		 ylab='Population size',xlab='Time')

# Now run the three functions to see what you get

analytic()       # black
discreteTime()   # green
individual()     # red

# Try running the functions multiple times without
# resetting the parameters or re-making the plot.
# Which functions give you different outcomes each time?
#
# Now try changing the parameter values above and
# re-running the functions (you may want to re-make
# the plot as well). How does changing each of the
# values change the output? Can you get the green
# curve to diverge from the black curve? How?
