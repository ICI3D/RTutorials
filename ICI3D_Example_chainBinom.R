## Introduction to Infectious Disease Dynamics (Chain Binomial - Tutorial 3)
## Clinic on the Meaningful Modeling of Epidemiological Data
## African Institute for Mathematical Sciences, Muizenberg, RSA
##
## Juliet R.C. Pulliam, 2012-2015
##
## Tutorial 3, Benchmark question 3 for Math Models in Med \& PH course

rm(list=ls())                   # Clear all variables and functions

## SIR.CB() -- Function that runs a chain binomial SIR model for a given population
## size and R0 value, outputting a data frame with columns representing, time,
## the number of susceptibles at each time, and the number of cases at each time.
## The chain binomial is a stochastic model, so the output varies between function
## calls, even for the same parameter values and initial conditions.
sir.cb <- function(R0,N,MAXTIME,I0=1,S0=N-I0){
  qq <- 1-R0/N  # Pairwise probability of avoiding potentially infectious contact

  Cases <- I0
  Sus <- S0

  for(Time in 1:MAXTIME){
    Cases <- c(Cases,rbinom(1,Sus[Time],(1-qq^Cases[Time])))
    Sus <- c(Sus,Sus[Time]-Cases[Time+1])
  }
  return(data.frame(Time=0:MAXTIME,Cases,Sus))
}

# Run the function once, for an R0 of 1.5, a population size of 1000, and 60 time
# steps
epi <- sir.cb(1.5,1000,60)
# Examine the resulting data frame
head(epi)

# Plot the number of cases through time and label the plot appropriately
par(mar=c(5,5,1,1))
plot(0:60,epi$Cases,
     type="s",      # Use a 'step' plot because time is treated as discrete
     bty="n",
     lwd=3,
     cex.lab=2,
     ylim=c(0,100),
     cex.axis = 1.1,
     xlab="Time",
     ylab="Infected")
text(40,80,expression(R[0]==1.5),cex=2)

# PLOT.CB -- Function that runs SIR.CB for specified parameter values, plots
# the number of cases through time (if plot==T), and returns the vector of values
# of the number of cases through time
plot.cb <- function(R0,N,MAXTIME=60,lwd=1,col="grey",plot=T){
  cases <- sir.cb(R0,N,MAXTIME)$Cases
  if(plot) lines(0:MAXTIME,cases,type="s",lwd=lwd,col=col)
  return(cases)
}

plot.cb(1.5,1000,60)

# Set up an empty plot with pre-labelled axes
par(mar=c(5,5,1,1))
plot(0:60,epi$Cases,
     type="s",      # Use a 'step' plot because time is treated as discrete
     bty="n",
     lwd=0,
     cex.lab=2,
     ylim=c(0,100),
     cex.axis = 1.1,
     xlab="Time",
     ylab="Infected")
text(40,80,expression(R[0]==1.5),cex=2)  # Add the R0 value used to the plot

# Call plot.cb() 3 times to plot 3 runs of the SIR chain binomial; do this
# using the built-in replicate() function, which automatically concatenates the
# output into a matrix
runs <- replicate(3,plot.cb(1.5,1000,60))
# Examine the upper left portion of the stored matrix: each column represents
# a different run, and each row represents a time point in the run
head(runs)

## Calculate the average number of cases in each timestep. Do this by using
## the apply() function to apply the function mean() to the rows of the matrix,
## runs, created above:

ave.case.t <- apply(runs,1,mean)

## Now, complete the following tasks on your own.
##
## Set up an empty plot with pre-labelled axes, just like before:



# Add the R0 value used to the plot:



## Call plot.cb() 300 times to plot 300 runs of the SIR chain binomial. Again,
## do this using the built-in replicate() function to automatically concatenate
## the output:


## Calculate the average number of cases in each timestep:


## Calculate the median number of cases in each timestep:


## Add lines to your plot that represent the mean and median number of cases
## through time. Use a thick red line for the mean values and a thick blue line
## for the median values:



## Save your plot as a PDF, and email it to Steve.
