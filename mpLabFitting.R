## MMED Lab of some sort
## (C) ICI3D; some rights reserved

## Steven Walker 2025
## Jonathan Dushoff 2025
## See also https://github.com/canmod/macpan2/tree/main/inst/starter_models/hiv for the full Granich model with treatmeant

## We are going to be using the macpan2 package
## You can get installation instructions and background info here:
#### https://canmod.github.io/macpan2/
library(macpan2)

######################################################################

## Specify helper relationships and then flows; use them to make a model “specification”

fn = list(
		I ~ I1 + I2 + I3 + I4
	, N ~ S + I
	, P ~ I / N
	, lambda ~ lambda0 * exp(-alpha * P)
)

flows = list(
		mp_per_capita_flow("S", "I1", "lambda * I/N", "infection")
	, mp_per_capita_flow("I1", "I2", "rho", "progression1")
	, mp_per_capita_flow("I2", "I3", "rho", "progression2")
	, mp_per_capita_flow("I3", "I4", "rho", "progression3")
	, mp_per_capita_flow("I4", "D", "rho", "deathHIV")
	, mp_per_capita_outflow("S", "mu", "deathS")
	, mp_per_capita_outflow("I1", "mu", "deathI1")
	, mp_per_capita_outflow("I2", "mu", "deathI2")
	, mp_per_capita_outflow("I3", "mu", "deathI3")
	, mp_per_capita_outflow("I4", "mu", "deathI4")
	, mp_per_capita_inflow("N", "S", "beta", "birth")
)

hiv4spec = mp_tmb_model_spec(
		during = c(fn, flows)
)

## We can make a rough sketch to confirm that we have the right model!
system.file("utils", "box-drawing.R", package = "macpan2") |> source()
layout = mp_layout_grid(hiv4spec)
(layout
  |> plot_flow_diagram(show_flow_rates = TRUE)
  |> draw_outflows(layout, show_labels = TRUE, lab = "rate")
  |> draw_inflows(layout, show_labels = TRUE, lab = "rate")
)

######################################################################

## Pick some parameters and create an implementation of our specification

## parameters: all rates are per year.
## Inspired by Kretzschmar et al. 2013
## https://www.pnas.org/doi/full/10.1073/pnas.1301801110

rho <- mean(c(1/0.271, 1/8.31, 1/1.184, 1/1.316))
print(rho)
params = list(
	# per-capita demographics
		mu = 0.018	 ## death rate
	, beta = 0.02	## birth rate

	# per-capita progression rates
	, rho = rho

	, lambda0 = 0.65	## baseline transmission rate
	, alpha = 4 ## non-linearity parameter
)

## initial conditions
inits = list(
	  S = 400 ## susceptible
	, I1 = 1, I2 = 1, I3 = 1, I4 = 1 ## infectious per stage
	, D = 0		 ## cumulated death due to disease
)

## How would you calculate R0 for this model?
## FIXME

## produce an 'initialized' specification with default parameter
## values, initial states, and specify that the RK4 differential
## equation solver be used for simulations.
hiv4init = (hiv4spec
	|> mp_rk4()
	|> mp_tmb_update(default = params, inits = inits)
)

## implement this specification by setting the number of
## time steps to simulate (i.e., years in the model) and
## the state variables and flow rates to simulate.
hiv4impl = mp_simulator(hiv4init
	, time_steps = 50L
	, outputs=c("S", "I", "N", "P", "D", "infection", "deathHIV")
)

######################################################################

## Simulate a trajectory; this is a deterministic model, so we should only need to do it once

baseSim <- (hiv4impl
	|> mp_trajectory()
)

## TASK: Examine baseSim in Rstudio.

## It might be good to use dplyr to manipulate this object.
## Usually it's good to put library functions at the beginning of scripts to crash out early, but we're making an exception

library(dplyr)
basePrev <- (baseSim 
	|> filter(matrix=="P") 
	|> rename(prevalence=value)
)

## And now we want to use ggplot to plot these structured objects (another exception)
library(ggplot2); theme_set(theme_bw())

## Modelers have historically looked at prevalence here
print(ggplot(basePrev)
	+ aes(time, prevalence)
	+ geom_line()
)

## But the inferred population effects are quite dramatic!
## Worth keeping in mind
baseState <- (baseSim 
	|> filter(matrix %in% c("S", "I", "D", "N"))
	|> rename(indivs=value, state=matrix)
)

## Showing state variables and the total population size.
print(ggplot(baseState)
	+ aes(time, indivs, color=state)
	+ geom_line()
)

######################################################################

## Now we will try calibration. This article is an introduction 
## to this topic.
## proceeding: https://canmod.github.io/macpan2/articles/calibration

## To illustrate calibration, we add negative binomial noise to the simulated 
## number of infectious individuals. Then we will attempt to fit the model to 
## these simulated data, to see if we can recover the parameters from the 
## simulations.
set.seed(1)
noisyI = (baseSim
	|> filter(matrix == "I")
	|> mutate(value = rnbinom(n(), mu = value, size = 5)) # size is the 'dispersion' parameter
)
(noisyI
  |> rename(`Observation Year` = time)
  |> rename(`Infectious Individuals` = value)
  |> ggplot()
  + geom_line(aes(`Observation Year`, `Infectious Individuals`))
  + theme_bw()
)

## Before proceeding, think about how you might fit the model to these data
## using the information you just read about in the article on calibration.

## options(macpan2_verbose = FALSE)

## The first step is to create a calibrator object, which is an object that 
## can be used to calibrate the parameters of a model specification to data.
calibrator = mp_tmb_calibrator(hiv4init
  , data = noisyI
    
    ## Assume that the noisy I is Poisson distributed
  , traj = list(I = mp_neg_bin(disp = mp_fit(1)))
  
    ## The parameter to fit is the log transform of lambda0
    ## (prefixing parameters by common transformation names
    ## allows us to fit on the transformed scale)
  , par = c(
  		log_lambda0 = mp_normal(log(0.5), sd = 0.1) ## normal pseudo-prior
  	, alpha = mp_uniform() ## flat pseudo-prior (i.e., no prior)
  	, log_rho = mp_normal(log(1), sd = 0.1) ## normal pseudo-prior
  )
  , outputs = "I"
	
  ## Change the value of the parameters so that the calibrator has to infer 
	## the true values from the noisy simulated data.
	, default = list(lambda0 = 0.2, alpha = 3, rho = 1.1)
)

## Optimize parameters in the calibrator.
## The mp_optimize() function modifies the sir_calibrator object in place,
## updating the parameter values to their optimized estimates.
mp_optimize(calibrator)

## what is the convergence code? what does this code mean?

## Recall the true values of the parameters.
print(mp_default_list(hiv4init)[c("lambda0", "alpha", "rho")])

## Did calibration result in reasonable estimates of these parameters?
fittedCoefs = (mp_tmb_coef(calibrator, conf.int = TRUE)
 |> select(-term, -row, -col, -type)
)
print(fittedCoefs)

## The simulated data (black) that we fit to matches the predictions of the 
## fitted model (red) with 95% confidence intervals for the point prediction).
(calibrator
	
 ## macpan2 trajectory function that produces confidence intervals
 ## using standard maximum likelihood theory.
 |> mp_trajectory_sd(conf.int = TRUE)
	
 |> ggplot()
 + geom_line(aes(time, value), colour = "red")
 + geom_ribbon(
 			aes(time, ymin = conf.low, ymax = conf.high)
		, alpha = 0.2
		, fill = "red"
 )
 + geom_line(aes(time, value), data = noisyI)
 + theme_bw()
)


## Joint likelihood example
## Simulate infection (aka incidence) and number of new
## deaths due to HIV.

set.seed(1)
twoNoisyVars = (baseSim
	|> filter(matrix %in% c("infection", "deathHIV"))
	|> mutate(value = rnbinom(n(), mu = value, size = 5))
)
(twoNoisyVars
  |> rename(`Observation Year` = time)
	|> mutate(matrix = ifelse(matrix == "infection", "incidence", matrix))
  |> ggplot()
  + geom_line(aes(`Observation Year`, value))
	+ facet_wrap(~matrix, scales = "free")
  + theme_bw()
)

twoVarCalibrator = mp_tmb_calibrator(hiv4init
  , data = twoNoisyVars
  , traj = list(
  			infection = mp_neg_bin(disp = mp_fit(1))
  		, deathHIV = mp_neg_bin(disp = mp_fit(1))
  )
  , par = list(
	  		log_lambda0 = mp_uniform()
	  	, alpha = mp_uniform()
	  	, log_rho = mp_uniform()
  )
  , outputs = c("infection", "deathHIV")
  , default = list(lambda0 = 0.2, alpha = 3, rho = 1.1)
)
mp_optimize(twoVarCalibrator)
fittedCoefs = (mp_tmb_coef(twoVarCalibrator, conf.int = TRUE)
 |> select(-term, -row, -col, -type)
)
print(fittedCoefs)

(twoVarCalibrator
 |> mp_trajectory_sd(conf.int = TRUE)
 |> ggplot()
 + geom_line(aes(time, value), colour = "red")
 + geom_ribbon(
 			aes(time, ymin = conf.low, ymax = conf.high)
		, alpha = 0.2
		, fill = "red"
 )
 + geom_line(aes(time, value), data = twoNoisyVars)
 + facet_wrap(~matrix, scales = "free")
 + theme_bw()
)
