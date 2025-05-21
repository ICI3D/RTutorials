
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

## Now copy and paste the code for the SIR model shown in the video
#### ML please add link to video code

## default values for quantities required to run simulations
default = list(
    beta = 0.2   ## transmission rate
  , gamma = 0.1  ## recovery rate
  , N = 100      ## total population size (constant in this model)
  , I = 1        ## initial number of infectious individuals
  , R = 0        ## initial number of recovered individuals
)

## flow diagram specification
flows = list(
    mp_per_capita_flow("S", "I", "beta * I / N", "infection")
  , mp_per_capita_flow("I", "R", "gamma", "recovery")
)

## compute the initial number of susceptible individuals
initialize_state = list(S ~ N - I - R)

## model specification
spec = mp_tmb_model_spec(
    before = initialize_state
  , during = flows
  , default = default
)

# set number of time steps in simulation
time_steps = 100L

# simulator object
sir = mp_simulator(  
    model = spec
  , time_steps = time_steps
  , outputs = "I"
)



## make sure you understand it, and then go ahead and run it.

######################################################################
## Temporary code block TCB, delete these when you can
