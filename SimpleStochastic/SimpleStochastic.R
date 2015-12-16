## Simple stochastic models: computer exercise
## Clinic on Dynamical Approaches to Infectious Disease Data
## International Clinics on Infectious Disease Dynamics and Data Program
## University of Florida, Gainesveille, FL, USA
##
## Juliet R.C. Pulliam, 2016
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)
## 
## BASED ON an Excel tutorial developed by Alex Welte (2012)
##
rm(list=ls())

initialPopulationSize <- 10
mortalityRate <- 0.05
timeStep <- 1
maxTime <- 30

par(bty='L',lwd=3,mar=c(4,4,1,1))
plot(NA,NA,ylim=c(0,initialPopulationSize),xlim=c(0,maxTime),
		 ylab='Population size',xlab='Time')

analytic <- function(initial = initialPopulationSize,
										 rate = mortalityRate,
										 times = seq(0,maxTime,timeStep),plot=T){
	ts <- data.frame(time = times,populationSize = initial * exp(-rate*times))
	lines(ts$time,ts$populationSize,type='l')
	return(ts)
}

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

analytic()
discreteTime()
individual()
