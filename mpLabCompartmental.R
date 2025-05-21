
## This is the brainstorm area, aim to remove it by early in the clinic

## This is intended as a replacement for Lab 1
#### ICI3D_Lab1_ODEmodels.R

## Make an SIR object
## simulate and plot discrete-time trajectories

## Plot

## Other implementations
#### ODE
#### Whatever Steve is calling the discrete-time demographic stochastic
#### Is there even Gillespie?

## Extensions
#### Birth-death
#### latent compartment
#### Be creative

## /brainstorm

######################################################################

## Introduction to Infectious Disease Dynamics
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA

## Copyright ICI3D, 2012–2025

## The goal of this tutorial is to acquaint you with ways of
## implementing simple infectious disease models in R using macpan2
## this tutorial, you should be able to:

##  Create a model object in macpan2
##  Run simulations
##  Plot simulation outputs

## You should already have installed macpan2
## If not, please do it (see https://canmod.github.io/macpan2/#installation)

######################################################################

## You can learn a bit about macpan2 at this workshop page
#### https://canmod.github.io/macpan-workshop/

## If you haven't already, it's recommended that you watch _at least_ the second video: Specifying simple models
#### https://drive.google.com/file/d/1BwAYWPi6e3PDn4AhqVsM14KkHie6Vx26

## Load the library
library(macpan2)

######################################################################

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

######################################################################

## Simulate 
time_steps = 100

# make a default simulator object; this will do a simple discrete-time simulation
sirDiscrete = mp_simulator(model = sirSpec
	, time_steps = time_steps
	, outputs = "I"
)

# Run
sirDiscreteTraj <- mp_trajectory(sirDiscrete, include_initial=TRUE)

######################################################################

## Simulate in continuous time

# The mp_ function creates a specification for a particular algorithm
# rk4 is a well-known algorithm for integrating in continuous time while controlling errors
sirContinuous = mp_simulator(model = mp_rk4(sirSpec)
	, time_steps = time_steps
	, outputs = "I"
)

# Run
sirContinuousTraj <- mp_trajectory(sirContinuous, include_initial=TRUE)
print(sirContinuousTraj)

######################################################################

## Try a simulation with demographic stochasticity

# rk4 is a well-known algorithm for integrating in continuous time while controlling errors
sirDS = mp_simulator(model = mp_discrete_stoch(sirSpec)
	, time_steps = time_steps
	, outputs = "I"
)

# Run
## FIXME: What should you add here to make this replicable?
sirDSTraj <- mp_trajectory(sirDS, include_initial=TRUE)
print(sirDSTraj)

######################################################################

