## Heterogeneity and SIR Dynamics
## Clinic on Dynamical Approaches to Infectious Disease Data
## International Clinics on Infectious Disease Dynamics and Data Program
## University of Florida, Gainesveille, FL, USA
##
## Steve E. Bellan, 2012
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)


## This exercise will help you building intuition for how
## heterogeneity in contact mixing patterns affects infectious disease
## dynamics. We have built a continuous time stochastic S IR model for
## you to play with. Importantly, in this model not all individuals
## are the same. Each individual has their own contact rate, with
## which they meet other people in the population. Some individuals
## meet other people more often, and some meet people less often.

## If we think of the total distribution of contact mixing patterns,
## then we can characterize it using terms such as its mean, it's
## variance, how skewed it is (i.e. how long is its tail). While it is
## not so much important how we have built this distribution (we use a
## gamma distribution if you are interested), we are giving you the
## opportunity to change it the mean and variance of individuals
## contact rates. Using the code below play with the outbreak model
## and see if you can understand how heterogeneity is affecting the
## disease dynamics, and specifically the distribution of outbreak
## sizes.


rm(list=ls())                   # Clear all variables and functions from the workspace
load("HetSIR_functions.Rdata")               # Load model functions and output


par(mfrow=c(1,3))                      # initialize plot window with 1 row & 3 columns

## STEP 1
## First, let's run the model with very low variance so we can
## see what happens in a situation that is very similar to the
## homogenous population model (like the compartmental box models that
## we have been using in differential equation and discrete time
## versions of the SIR model)

het.epidemic(beta.mean = 2, beta.var = .001, runs = 5, end.time = 10, pop.size = 100, gmma = 1)

######################################################################
## EXPLANATION OF VARIABLES
###################################################################### 
## beta.mean is the mean of your contact rate distribution
## beta.var is its variance
## runs is the number of realizations to do it for
## end.time is how long to let the simulations go before cutting them off (if the outbreak hasn'nt already died)
## pop.size is the total population size
## gmma is the recovery rate (or 1/duration of infectiousness)

###################################################################### 
## OUTPUT
###################################################################### 
## The panel on the left is a histogram of the contact rate
## distribution. The middle shows the epidemic time series. The right
## panel shows the distribution of outbreak size from the runs
## (realizations you performed).

## STEP 2
## Try doing the above for 50 runs. Summarize the distribution of
## outbreak time series and final sizes.


## STEP 3
## Experimenting: Now try comparing the same model with the same mean
## mixing rate but different variance in the mixing rate. What
## happens?

## Replace the ??? with values for the variance below

par(mfrow=c(2,3))      # initialize plot window with 2 row & 3 columns
het.epidemic(beta.mean = 2, beta.var = ???, runs = 30, end.time = 10, pop.size = 100, gmma = 1) # try a small variance
het.epidemic(beta.mean = 2, beta.var = ???, runs = 30, end.time = 10, pop.size = 100, gmma = 1) # try a big one


## STEP 4

## Now change the above code so that you have a larger population
## size. Note that the bigger this is the longer the simulation is
## going to run, so be careful not to choose anything too big
## (probably not more than 500 or so). What happens to the randomness
## in the simulation as yo increase population size. How does this
## change how different levels of heterogeneity (variance in mixing
## rates) affects epidmic dynamics.

## STEP 5

## Recall that R0 = beta/gamma. What about when individuals have
## different betas? Since we're using gamma = 1, in this model R0 =
## beta, at least for a homogenous population. Explore how adding
## heterogeneity can affect outbreak dynamics when the average R
## (i.e., the number of infectious contacts each infected person
## makes) is less than 1 but heterogeneous.

## Replace the ??? with values of beta.mean < 1 with different beta.var's.

par(mfrow=c(2,3))      # initialize plot window with 2 row & 3 columns
het.epidemic(beta.mean = ???, beta.var = .1, runs = 30, end.time = 10, pop.size = 100, gmma = 1) # try a small variance
het.epidemic(beta.mean = ???, beta.var = 10, runs = 30, end.time = 10, pop.size = 100, gmma = 1) # try a big one

