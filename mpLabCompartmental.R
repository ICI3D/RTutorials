
## Introduction to Infectious Disease Dynamics
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA

## Copyright ICI3D, 2012–2025

## The goal of this tutorial is to acquaint you with ways of
## implementing simple infectious disease models in R using macpan2
## this tutorial, you should be able to:

## Create a model object in macpan2
## Run simulations
## Plot simulation outputs
## Run a simple stochastic analogue of the same model
## Extend your model (add a compartment or a flow)

## Please make sure macpan2 is installed
## For help, see https://canmod.github.io/macpan2/#installatio)

##### Reference Material #############################################

## You can learn a bit about macpan2 here
#### https://canmod.github.io/macpan2
#### https://canmod.github.io/macpan-workshop/

## It's recommended that you watch at least the second video: Specifying simple models
#### https://drive.google.com/file/d/1BwAYWPi6e3PDn4AhqVsM14KkHie6Vx26

## If you prefer to read rather than watch, see this quick start guide
#### https://canmod.github.io/macpan2/articles/quickstart

######################################################################


## Load the library
library(macpan2)

## Build a model structure

## default values for quantities required to run simulations
dPar <- list(
	  beta = 0.2 
	, gamma = 0.1 
)

initVals <- list(
	N = 100
	, I = 1
	, R = 0
 )

## flow diagram specification
## try ?mp_per_capita_flow, and make sure you understand the arguments here
flows = list(
	  mp_per_capita_flow("S", "I", "beta * I / N", "infection")
	, mp_per_capita_flow("I", "R", "gamma", "recovery")
)

initialize_state = list(S ~ N - I - R)

## model specification
sirSpec = mp_tmb_model_spec(
	  before = initialize_state
	, during = flows
	, default = dPar
	, inits = initVals
)

print(sirSpec)

######################################################################

## Simulate 
time_steps = 100

# make a default simulator object; this will do a simple discrete-time simulation
sirDiscrete = mp_simulator(model = sirSpec
	, time_steps = time_steps
	, outputs = c("I", "S")
)

# Run
sirDiscreteTraj <- mp_trajectory(sirDiscrete, include_initial=TRUE)

## Take a look at the simulation object
View(sirDiscreteTraj) ## CONSOLE

## How would you make plots of an object like this in ggplot?
## Make a plan, and then look at the suggestion below.

######################################################################

library(ggplot2); theme_set(theme_bw())

print(ggplot(sirDiscreteTraj)
	+ aes(time, value, color=matrix)
	+ geom_line()
)

## You should have a nice epidemic curve now.
## Play with some of the parameters and see how the curve changes.
## What are the key parameters?
## Can you make a function that accepts these parameters and draws epidemic curves?
## What should you do if you want to plot incidence and recovery instead of S and I?
## Which of those four values corresponds to disease “prevalence”? In what way?
## What are some other ways to improve or extend this plot?

######################################################################

## Simulate in continuous time

# The mp_ function creates a specification for a particular algorithm
# rk4 will construct steps that more closely match a continuous process
sirContinuous = mp_simulator(model = mp_rk4(sirSpec)
	, time_steps = time_steps
	, outputs = "I"
)

# Run
sirContinuousTraj <- mp_trajectory(sirContinuous, include_initial=TRUE)
print(sirContinuousTraj)

### ADD Code to plot this. How does the curve differ from the version above?
### HINT: Maybe not very much. What does that mean?

######################################################################

## Try a simulation with demographic stochasticity

# mp_discrete_stoch uses the method underlying the chain binomial
sirDS = mp_simulator(model = mp_discrete_stoch(sirSpec)
	, time_steps = time_steps
	, outputs = "I"
)

# Run
sirDSTraj <- mp_trajectory(sirDS, include_initial = TRUE)
## head(sirDSTraj) ## Try this on the console

### ADD Code to plot this, too.
### ADD anything else you want

## What happened? What can you learn from this? What are some next steps?

######################################################################

## To properly understand a stochastic simulation, we should try it many times
## There is a macpan2 function for this, called mp_trajectory_ensemble

dsReps <- 10
sirDSTrajectories <- mp_trajectory_replicate(sirDS, include_initial=TRUE, n=dsReps)

## str(sirDSTrajectories) ## Use the console to see what you can figure out about this object

######################################################################

## the _replicate object is a list of trajectories;
## it's easy to use in ggplot if we “bind” it into big data frame

library(dplyr)

sirBig <- (sirDSTrajectories
	|> bind_rows(.id="Realization")
)

## View(sirBig) ## How about this object? What did .id= do??

print(ggplot(sirBig)
	+ aes(time, value, group=Realization)
	+ geom_line()
)

## What is group doing above? What happens if you plot without it? 
## What other things can you try with this?

######################################################################

## Can you extend this model? What would be a simple, logical extension?
## Can you make code for it and do some experiments?
