## Introduction to MCMC 2: Fitting an SI model to an HIV epidemic with adaptive block MCMC
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
## Steve Bellan 2015
###################################################################### 

## By the end of this tutorial you should…

## * Understand how to simulate cross-sectional prevalence data around a simulated epidemic.
## * Calculate a binomial likelihood from these prevalence data and a fully specified epidemic model.
## * Be able to explain sequential, block, and adaptive Metropolis Hastings sampling algorithms.
## * Know how to assess multivariate MCMC chains for convergence, both with trace plots and the Gelman-Rubin diagnostic.
## * Create 95% credible intervals (CrI's) and contours from the posterior


library(deSolve)
library(ggplot2)
library(boot)
library(ellipse)
library(coda)
library(parallel)
library(mnormt)
library(emdbook)

#' @title Create Disease Parameters for HIV SI4 Model
#' 
#' @description
#' Provides defaults for creating the necessary model parameters for the
#' SI4 model from HIV in Harare tutorial.
#' 
#' @examples
#' disease_params()
#' disease_params(Beta = .2) 
disease_params <- function(
	Beta = 0.9,
	alpha = 8,         ## rate of beta decline with prevalence
  progRt = (1/10)*4, ## rate of of progression through each of the I classes, for 10 years total
  birthRt = .03,     ## birth rate, 3% of people give birth per year
  deathRt = 1/60     ## 60 year natural life expectancy
) {
  return(as.list(environment())) # this converts the arguments into a list
}

reference_params <- disease_params()

## modeling proportion of population

initial_prevalence <- exp(-7) ## infected at start
init <- c(
	# population states: susceptible + infectious (in 4 boxcar compartments)
	S = 1, I1 = initial_prevalence, I2 = 0, I3 = 0, I4 = 0,
	# accumulator variables: cumulative infections (CI) and deaths (CD)
	CI = 0, CD = 0
)

## Define the SI4 ODE model
## This is equivalent to the one we used for MLE fitting (from HIV tutorial and Lab 6)
hiv_SI4 <- function(tt, yy, parms) with(c(parms, as.list(yy)), {

  ## State variables are: S, I1, I2, I3, I4
  ## derived quantities
  I <- I1 + I2 + I3 + I4           ## total infectious
  N <- I + S                       ## total population
  
  ## processes
  infection <- Beta * exp(-alpha * I / N) * S * I / N
  birth <- birthRt*N
  progression <- progRt * c(I1, I2, I3, I4)
  background_death <- deathRt * c(S, I1, I2, I3, I4)

  # Susceptibles are born, and are lost to infection    
  dSdt <- +(birth) - (infection)
  # new infections arrive in I1; progressing infections arrive in I2-I4 from I1-I3
  # all Infectious compartments progress
  dIdt <- +c(infection, progression[1:3]) - (progression)

  # accumulator variables: infections into CI, disease deaths (departure from I4) into CD
  dCIdt <- +infection
  dCDdt <- +progression[4]
  
  return(list(
  	c(
  		c(dSdt, dIdt) - background_death, # all must die
  		dCIdt, dCDdt # but accumulator variables do not
  	)
  ))
})

#' @title Simulates an HIV Epidemic
#' 
#' @description
#' Wraps [deSolve::ode()] to compute additional values & set default values
#' 
#' @inheritParams deSolve::ode
#' 
#' @return a data.frame
simEpidemic <- function(
	y = init, 
	times = seq(1976, 2015, by = 1/12), # report outputs monthly from 1976 to 2015
	func = hiv_SI4, # use the model we're considering
	parms = reference_params
) {
	# solve the system using deSolve, & then ...
	result <- deSolve::ode(y, times, func, parms) |>
  	as.data.frame() |> # convert to data.frame for easy manipulation, & then ...
    within({ # compute total infectious, total population, and prevalence
    	I <- I1 + I2 + I3 + I4
    	N <- S + I
    	P <- I/N
    })
  return(result)
}

#' @title Sample from a Time Series
#' 
#' @description
#' Given time series of the prevalence of states, simulate a series of survey
#' times and samples.
#' 
#' @param dt a data.frame; the data, as produced by [simEpidemic()]
#' 
#' @param times a numeric vector; the times - must be present in `dt`
#' 
#' @param n an integer vector; the number of individuals to sample at `times`
#' 
#' @return a data.frame, columns `time`, `n`, `positive`, `est_prevalence` `l95_prevalence`, `u95_prevalence`
#'
sampleEpidemic <- function(
	dt = simEpidemic(),
	times = seq(1980, 2010, by = 3),
	n = rep(80, length(times)),
	seed = 8675309
) {
	# TODO: write stop-checks
	target_CI <- 0.95
	set.seed(seed)
	# get the relevant subset, & then ...
	res_dt <- subset(dt, time %in% times)[, c('time', 'P')] |>
		within({ # within that subset
			positive <- rbinom(length(n), n, P) # simulate the survey
			n <- n # store `n`
			# compute the binomial confidence intervals
			tmp <- mapply(\(x, n) { binom.test(
				x, n, conf.level = target_CI
			)$conf.int }, positive, n)
			# store est. + confidence intervals
			est_prevalence <- positive / n
			l95_prevalence <- tmp[1,]
			u95_prevalence <- tmp[2,]
			# drop now-unused variables
			tmp <- NULL
			P <- NULL
		})

	return(res_dt)
	
}

## Run system of ODEs for "true" parameter values
reference_dt <- simEpidemic(parms = reference_params) # Simulated epidemic (underlying process)
## get a sample for prevalence
samp_dt <- sampleEpidemic(reference_dt)

print(
	ggplot() + aes(x=time) +
	geom_line(aes(y = P, color = "latent"), data = reference_dt, linetype = "dashed") +
	geom_errorbar(
		aes(ymax = u95_prevalence, ymin = l95_prevalence, color = "observed"),
		data = samp_dt
	) + geom_point(
		aes(y = est_prevalence, color = "observed"),
		data = samp_dt
	) +
	theme_minimal() + theme(
		legend.position = c(0, 1), legend.justification = c(0, 1)
	) +
	scale_x_continuous("Year") + scale_y_continuous("Prevalence") +
	scale_color_manual(NULL, values = c(latent = "firebrick", observed = "black"))
)

#' To start, we need to write a log-likelihood function that gives the
#' probability that a given parameter set (Beta, alpha values) would generate the observed data. Remember that we are assuming that there is some
#' true underlying epidemic curve that is deterministic and the data we observe are only noisy
#' because of sampling/observation error (not because the underying curve is also
#' noisy--i.e. process error--which would be particularly likely for epidemics in small populations).

#' We assume binomial sampling errors. So we can write the log-likelihood as the probability of
#' observing the observed number positive out of each sample if the prevalence is the value generated by
#' a model parameterized by a given set of parameters:

loglikelihood <- function(
	params, obs_dt, defaults = reference_params
) {
	testvals <- defaults
	testvals[names(params)] <- params
	sim_dt <- simEpidemic(times = c(1976, obs_dt$time), parms = testvals)[-1, ]
  return(with(obs_dt, {
  	sum(dbinom(positive, n, prob = sim_dt$P, log = T))
  }))
}

loglikelihood(reference_params, samp_dt) ## loglikelihood of the true parameters (which we usually never know)
loglikelihood(
	list(Beta = 3, alpha = 1),
	samp_dt
)  ## vs some random guessed parameters
 

## Convenience function that sums log-likelihood & log-prior for
## evaluation inside MCMC sampler.
llikePrior <- function(
	params, ## parameters to evaluate
  obs_dt,
	defaults = reference_params, ## for any other parameters needed
	lprior = 0 # assume the log-prior of the proposed params is uniform
) {
	loglikelihood(params, obs_dt, defaults) + lprior
}

llikePrior(reference_params, obs_dt = samp_dt)

logBounds <- data.frame(
	parameter = c("Beta", "alpha", "progRt"), # the parameters we're trying to estimate
	lower = c(.2, 1, 1/10) |> log(), # the (log) lower bound of those parameters
	upper = c(2, 30, 1) |> log() # their (log) upper bound
)

sampleBounds <- function(
	bounds = logBounds, # from these bounds (default logBounds defined above)
	what = bounds$parameter # for these parameters (default: all defined in by bounds$parameter)
) {
	with(subset(bounds, parameter %in% what), {
		mapply(\(min, max) runif(n = 1, min, max), min = lower, max = upper) |> exp() |> setNames(parameter)
	})
}

## give it parameter vector and it will simulate random values within the bounds for those values
sampleBounds() # default, 1 draw of everything
sampleBounds(what = c("Beta")) # for example, just Beta
sampleBounds(what = c("alpha", "Beta")) # alpha and Beta
sampleBounds(what = c("Beta", "alpha")) # order doesn't matter




## Flexible Metropolis-Hastings Sampler
mcmcSampler <- function(
	guess, ## initial parameter guess; if missing, randomly drawn
	proposal_sd = rep(.15, length(guess)) |> setNames(names(guess)), ## named numeric vector, for proposal standard deviation
  seed = 1, ## RNG seed
  defaults = reference_params, ## defaults for all non-fitted parameters
  obs_dt, ## the observed data
  proposer = c("sequential", "block"),
  niter = 100, ## MCMC iterations
  nburn = 0, ## iterations to automatically burn
  adaptiveMCMC = FALSE, ## adapt proposal distribution?
  startAdapt = 150, ## start adapting at what iteration?
  adptBurn = 200
) {
	# TODO: other assert / stop / match checks on arguments
	proposer <- match.arg(proposer)
	
	set.seed(seed)
	
	# if initial guess not provided, get one at random
	if (missing(guess)) guess <- sampleBounds(what = names(proposal_sd))
	
	# setting up the output:
	# one row for each iteration, one column for each parameter + the ll
	out <- matrix(NA, nrow = niter, ncol = length(guess)+1)
	colnames(out) <- c(names(guess), 'll') ## name columns
	
	# start with the guess
	accepted_ll <- llikePrior(guess, obs_dt, defaults)
	out[1,] <- c(guess, accepted_ll)
	
  accept <- 0 ## counter for iterations accepted
  nfitted <- length(guess) ## number fitted parameters
  
  current_covariance <- matrix(0, nrow = nfitted, ncol = nfitted)
  diag(current_covariance) <- proposal_sd^2
  
  if (proposer == 'block') {
  	original_covariance <- current_covariance
  	propose <- function(current, covariance, ...) {
  			return(
  				(log(current) + rmnorm(1, mean = 0, varcov = covariance)) |> exp()
  			)
  	}
  } else { # use the sequential proposer
  	propose <- function(current, covariance, step) {
  	  proposal <- log(current)
  	  target <- (step %% nfitted) + 1 ## only updating one parameter with proposal
  	  proposal[target] <- proposal[target] + rnorm(1, mean = 0, sd = sqrt(covariance[target, target]))
  	  return(exp(proposal))
    }
  }
  
  for (vv in seq(2, niter)) {
  	if (proposer == 'block' & adaptiveMCMC & (vv > startAdapt) & (vv %% 50 == 0)) {
  		adptBurn <- min((startAdapt - 50), adptBurn)
  		## Below equation gives ideal covariance-variance matrix based on posterior
  		current_covariance <- 2.38^2 / nfitted * cov.wt(log(out[adptBurn:(vv - 1), 1:nfitted]))$cov
  		## Take a weighted average of the original & the empirical cov-var matrices to ensure
  		## that we never let the matrix collapse to zero (ie if the empirical one is zero
  		## because we haven't accepted anything yet)
  		current_covariance <- current_covariance * .95 + original_covariance * .05 ## 95% adapted & 5% original
  	}
  	
  	proposal <- propose(guess, current_covariance, vv)
  	proposal_ll <- llikePrior(proposal, obs_dt, reference_params)
  	llratio <- proposal_ll - accepted_ll
  	## log-likelihoods are negative; if proposal is less negative (i.e. more maximizing), then llratio is positive
  	if (!is.na(llratio)) { ## might update guess
  		## if better fit, always accept; if not better, sometimes accept based on random draw & how much worse
  		if ( (llratio >= 0) || (runif(1, 0, 1) <= exp(llratio)) ) {
  			guess <- proposal
  			accepted_ll <- proposal_ll
  			if (vv > nburn) accept <- accept + 1 ## only track acceptance after burn-in
  		}
  	}
  	out[vv, ] <- c(guess, accepted_ll)
  }
  
  samp <- as.mcmc(out[(nburn+1):nrow(out),], start = nburn+1)
	aratio <- accept/nrow(samp)
	return(list(
		seed = seed, samp = samp, aratio = aratio
	))

}

samp_Seq <- mcmcSampler(
	guess = c(alpha = 8, Beta = .9), seed = 1, niter = 100, obs_dt = samp_dt
)

class(samp_Seq$samp)
## The coda package already knows how to plot MCMC objects by default
par('ps'=16, las = 1)
plot(samp_Seq$samp)


## Parallelization in R
?mclapply

out1 <- mclapply(X = 1, function(x) rnorm(10^7)) ## What does this do?
out2 <- mclapply(X = 1:2, function(x) rnorm(10^7)) ## What does this do?

class(out1)
class(out2)

lapply(out1, summary)
lapply(out2, summary)

## Let's parallel chain sampling in the same way. First, rather than
## specify the parameters every time, let's make a list and edit it as
## we tweak the algorithm.
mcmcParams <- list(
	proposal_sd = c(alpha = .15, Beta = .15), niter = 10, obs_dt = samp_dt
)

## Parallel MCMC: Let's write a function that uses mclapply to call on
## mcmcSampler once for each seed on a different core. We need a
## different seed to make sure each chain has different random number
## generation. It's good practice to always initiate a seed so that
## bugs are always reproducible.
doChains <- function(x, mcmcParams) {
    print(system.time(
        ## Below line uses mclapply to parallelize do.call over seeds
    chains <- mclapply(x, function(x) do.call(mcmcSampler, within(mcmcParams, { seed <- x })))
    ))
    aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains
    chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
    chains <- as.mcmc.list(chains) ## make into mcmc.list
    return(list(chains = chains, aratio = aratio))
}

## How does the compute time increase with number of chains?
run1 <- doChains(1, mcmcParams) ## do one chain with a seed at 1
run2 <- doChains(1:2, mcmcParams) ## do two chains with seeds at 1:2
run3 <- doChains(1:3, mcmcParams) ## do three chains with seeds at 1:3
run4 <- doChains(1:4, mcmcParams) ## do four chains with seeds at 1:4
run5 <- doChains(1:5, mcmcParams) ## do five chains with seeds at 1:5

## Can you explain this by how many cores your computer has?
detectCores() ## often gives double the # of what you actually have.

class(run4$chains)
## You can index mcmc.lists by variables
run4$chains[,c('alpha','Beta')]
run4$aratio

plot(run4$chains)
gelman.diag(run4$chains)
## Question: Have your chains converged? How can you tell?

## Question: Why does the trace of the loglikelihood (ll) look different than other traces?

####################################################################################################
## Adaptive proposals
####################################################################################################
mcmcParams <- within(mcmcParams, {
  niter <- 2000 ## let's increase the # of iterations
  proposal_sd <- c(alpha = 0.31, Beta = 0.31) ## and get a wider sampling distribution
})
# and now define an alternative that uses the block sampler
mcmcParams_Adaptive <- within(mcmcParams, {
  proposer <- "block"
  adaptiveMCMC <- TRUE
})

# this bit takes a minute, so don't want to do it multiple times
if (!file.exists('MCMC_SI_runs.Rdata')) {
	run4 <- doChains(1:4, mcmcParams) ## do four chains with seeds at 1:4
	run4A <- doChains(1:4, mcmcParams_Adaptive) ## do four chains with seeds at 1:4
	save(run4, run4A, file = 'MCMC_SI_runs.Rdata')
} else {
	load(file = 'MCMC_SI_runs.Rdata')
}

run4$aratio
run4A$aratio

par(oma = c(0,0,2,0), bty='n', 'ps' = 18)
plot(run4$chains)
mtext('Sequential Sampling', side = 3, outer = T, line = 0)
 
plot(run4A$chains)
mtext('Adaptive Sampling', side = 3, outer = T, line = 0)

gelman.diag(run4$chains[,c('alpha','Beta')])
gelman.diag(run4A$chains[,c('alpha','Beta')])

summary(run4$chains) ## Posterior credible intervals
summary(run4A$chains)

par(mar = c(5,6,1,1), las = 1, 'ps' = 18, mfrow = c(2,1))
for(nm in c('4','4A')) {
    res <- get(paste0('run', nm))$chains
    plot(unlist(res[,'alpha']), unlist(res[,'Beta']),
         xlab = expression(alpha),
         ylab = expression(beta),
         log = 'xy',
         type = 'p',
         cex = .7, pch = 16,
         col = gray(.5, alpha = .1),
         xlim = c(2,15),
         ylim = c(.3, 2))
    ## Bayesian 95% credible contour calculated by finding highest posterior density region.
    HPDregionplot(res, 1:2,
                  prob = .95,
                  n = 40,
                  lwd = 2,
                  col = 'red',
                  add = T) ## add to current plot
}

