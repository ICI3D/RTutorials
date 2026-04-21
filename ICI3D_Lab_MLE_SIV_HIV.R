
## Introduction to Likelihood 2: Fitting an SI model to an HIV epidemic with Maximum Likelihood Estimation
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
## (C) Steve Bellan, 2015, Seth Blumberg 2025

## By the end of this tutorial you should…

## * Understand how to simulate cross-sectional prevalence data around a simulated epidemic.
## * Calculate a binomial likelihood from these prevalence data and a fully specified epidemic model.
## * Use R's "optim" function to do multivariate optimization over transformed parameter space.
## * Understand the difference betweeen SANN and Nelder-Mead algorithms
## * Create 95% CIs and contours from the hessian matrix
## * Create 95% CIs and contours based on the likelihood ratio

library(boot); library(deSolve); library(ellipse);

## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9 ## transmission coefficient when prevalence is 0
                           , alpha = 8 ## for transmission coefficient: decline with prevalence
                           , progRt = (1/10)*4 ## rate of of progression through each of the I classes, for 10 years total
                           , birthRt = .03 ## birth rate, 3% of people give birth per year
                           , deathRt = 1/60 ## 60 year natural life expectancy
){
  return(as.list(environment()))
}

disease_params()
disease_params(Beta = .2)

initPrev <- exp(-7) ## infected at start
tseqMonth <- seq(1976, 2015, by = 1/12)
init <- c(S=1-initPrev, I1=initPrev, I2=0, I3=0, I4=0, CI = 0, CD = 0) ## modeling proportion of population
Is <- paste0('I',1:4) ## for easy indexing

## Define the SI ODE model. This model is equivalent to the third model in the HIV in Harare tutorial
## 	(though some parameters may have different names)
SImod <- function(tt, yy, parms) with(c(parms,as.list(yy)), {
  ## State variables are: S, I1, I2, I3, I4
  ## derived quantitties
  I <- I1+I2+I3+I4           ## total infected
  N <- I + S                 ## total population
  transmissionCoef <- Beta * exp(-alpha * I/N) ## Infectious contact rate
  ## state variable derivatives (ODE system)
  deriv <- rep(NA,7)
  deriv[1] <-	birthRt*N - deathRt*S - transmissionCoef*S*I/N ## Instantaneous rate of change: Susceptibles
  deriv[2] <-	transmissionCoef*S*I/N - progRt*I1 - deathRt*I1 ## Instantaneous rate of change: Infection class I1
  deriv[3] <-	progRt*I1 - progRt*I2 - deathRt*I2 ## Instantaneous rate of change:  Infection class I2
  deriv[4] <-	progRt*I2 - progRt*I3 - deathRt*I3 ## Instantaneous rate of change: Infection class I3
  deriv[5] <-	progRt*I3 - progRt*I4 - deathRt*I4 ## Instantaneous rate of change: Infection class I4
  deriv[6] <-	transmissionCoef*S*I/N ## Instantaneous rate of change: Cumulative incidence
  deriv[7] <-	progRt*I4 ## Instantaneous rate of change: Cumulative mortality
  return(list(deriv))
})

## Function to run the deterministic model simulation, based on the ODE system defined in SImod().
simEpidemic <- function(init, tseq = tseqMonth, modFunction=SImod, parms = disease_params()) {
  simDat <- as.data.frame(lsoda(init, tseq, modFunction, parms=parms))
  simDat$I <- rowSums(simDat[, Is])
  simDat$N <- rowSums(simDat[, c('S',Is)])
  simDat$P <- with(simDat, I/N)
  return(simDat)
}

## Function to 'sample' the population:
## From a simulated epidemic, measure prevalence at several time points by drawing
## cross-sectional samples of individuals at each time, testing them, and then calculating sample
## prevalence and associated binomial confidence intervals

sampleEpidemic <- function(simDat # Simulated "data" which we treat as real
                           , sampleDates = seq(1980, 2010, by = 3) # Sample every 3 years
                           , numSamp = rep(80, length(sampleDates)) # Number of individuals sampled at each time point
){
  prev_at_sample_times <- simDat[simDat$time %in% sampleDates, 'P']
  numPos <- rbinom(length(numSamp), numSamp, prev_at_sample_times)
  lci <- mapply(function(x,n) binom.test(x,n)$conf.int[1], x = numPos, n = numSamp)
  uci <- mapply(function(x,n) binom.test(x,n)$conf.int[2], x = numPos, n = numSamp)
  return(data.frame(time = sampleDates, numPos, numSamp, sampPrev =  numPos/numSamp,
                    lci = lci, uci = uci))
}

## Run system of ODEs for "true" parameter values
trueParms <- disease_params() # Default model parameters are defined in lines 20-26
simDat <- simEpidemic(init, parms = trueParms) # Simulated epidemic (underlying process)

par(bty='n', lwd = 2)
# Plot simulated prevalence through time:
with(simDat, plot(time, P, xlab = '', ylab = 'prevalence', type = 'l', ylim = c(0,.4), col='red', las = 1))
## Take cross-sectional sample of individuals to get prevalence estimates at multiple time points:
set.seed(1) # Initiate the random number generator (so everyone's simulation looks the same)
myDat <- sampleEpidemic(simDat) # Simulate data from the sampling process (function defined above)
points(myDat$time, myDat$sampPrev, col = 'red', pch = 16, cex = 2) # Plot sample prevalence at each time point
arrows(myDat$time, myDat$uci, myDat$time, myDat$lci, col = 'red', len = .025, angle = 90, code = 3) # Plot 95% CIs around the sample prevalences

## To start, we need to write a likelihood function that gives the probability that a given parameter set
## (Beta, alpha values) would generate the observed data. Remember that we are assuming that there is some
## true underlying epidemic curve that is deterministic and the data we observe are only noisy
## because of sampling/observation error (not because the underlying curve is also
## noisy--i.e. process error--which would be particularly likely for epidemics in small populations).

## We assume binomial sampling errors. So we can write the -log-likelihood as the probability of
## observing the observed number positive out of each sample if the prevalence is the value generated by
## a model parameterized by a given set of parameters:

## Return the negative log of likelihood by convention
nllikelihood <- function(parms = disease_params(), obsDat=myDat) {
  simDat <- simEpidemic(init, parms=parms)
  ## What are the rows from our simulation at which we have observed data?
  matchedTimes <- simDat$time %in% obsDat$time
  nlls <- -dbinom(obsDat$numPos, obsDat$numSamp, prob = simDat$P[matchedTimes], log = T)
  return(sum(nlls))
}
nllikelihood(trueParms) ## loglikelihood of the true parameters (which we usually never know)
nllikelihood(disease_params(Beta = 3, alpha = 1))  ## vs some random guessed parameters

## First look up how optim() works. The more you read through the help
## file the easier this will be!!! In particular make sure you understand that
## the first argument of optim must be the initial values of the parameters to
## be fitted (i.e. Beta & alpha) and that any other parameters to be fixed are
## given as additional arguments (in the help file under "...")
## ?optim

## Since we need to be able to easily separate fitted and fixed parameters,
## let's make a function that takes fitted and fixed parameters together and
## puts them back in a parameter list (similar to the output of
## disease_params()). We also want it to be able to take logged parameter values
## and unlog them automatically back into the parameter list, so that we can fit
## on a log scale, but run our model with the unlogged values.
subsParms <- function(fit.params, fixed.params=disease_params()) {
  within(fixed.params, {
    loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
    unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]
    for(nm in unloggedParms) assign(nm, as.numeric(fit.params[nm]))
    for(nm in loggedParms) assign(gsub('log_','',nm), exp(as.numeric(fit.params[nm])))
    rm(nm, loggedParms, unloggedParms)
  })
}
guess.params <- c(log_Beta = log(5), log_alpha = log(8))
subsParms(guess.params, disease_params())

## Make likelihood a function of fixed and fitted parameters.
objFXN <- function(fit.params ## paramters to fit
                   , fixed.params =disease_params() ## fixed paramters
                   , obsDat=myDat) {
  parms <- subsParms(fit.params, fixed.params)
  nllikelihood(parms, obsDat = obsDat) ## then call likelihood
}
objFXN(guess.params, disease_params())

## Select initial values for fitted parameters from which optimization routine
## will start. If you select bad initial values the algorithm can get stuck on a
## bad set of parameters. You can always try the true values as a starting point
## for this problem, although that's rarely possible in real problems.

init.pars <- c(log_alpha = log(30), log_Beta = log(.1))
## We will start with SANN optimization since it is stochastic and therefore
## less likely to get stuck in a local minima. But then finish with Nelder-Mead
## optimization which is much faster.

###  NOTE: for trace >0 you see more progress report, bigger numbers show more
###  update
trace <- 3

## SANN: This is stochastic, be CAREFUL -- sometimes it gets stuck at local minima
## for unreasonble parameters. If you see this happen, run it again!
optim.vals <- optim(par = init.pars
                    , objFXN
                    , fixed.params = disease_params()
                    , obsDat = myDat
                    , control = list(trace = trace, maxit = 150)
                    , method = "SANN")
exp(unname(optim.vals$par))
trueParms[c('alpha','Beta')]

## We feed the last parameters of SANN in as the first values of Nelder-Mead
optim.vals <- optim(par = optim.vals$par
                    , objFXN
                    , fixed.params = disease_params()
                    , obsDat = myDat
                    , control = list(trace = trace, maxit = 800, reltol = 10^-7)
                    , method = "Nelder-Mead"
                    , hessian = T)
optim.vals
MLEfits <- optim.vals$par
trueParms[c('alpha','Beta')]
exp(unname(MLEfits))

log_alpha.fit <- MLEfits["log_alpha"]
log_Beta.fit <- MLEfits["log_Beta"]

## Look at the output of optim. Understand what it means. Did the algorithm
## converge? Look at ?optim to understand it.

## Plot MLE fit time series
par(bty='n', lwd = 2, las = 1)
with(simDat, plot(time, P, xlab = '', ylab = 'prevalence', type = 'l', ylim = c(0,.4), col='red'))
fitDat <- simEpidemic(init, parms = subsParms(optim.vals$par, trueParms))
with(fitDat, lines(time, P, col='blue'))
points(myDat$time, myDat$sampPrev, col = 'red', pch = 16, cex = 2)
arrows(myDat$time, myDat$uci, myDat$time, myDat$lci, col = 'red', len = .025, angle = 90, code = 3)
legend("topleft", c('truth', 'observed', 'fitted'), lty = c(1, NA, 1), pch = c(NA,16, NA),
       col = c('red', 'red', 'blue'), bty = 'n')

######################################################################
## Contour plots with the hessian
######################################################################
## The Hessian matrix gives you the curvature of the likelihood function at the maximum likelihood
## estimate (MLE) of the fitted parameters. In other words, it tells you the second derivative around
## MLE, which can be used to estimate the covariance variance matrix of the estimator. This estimate of
## the covariance varance matrix is known as the Fisher information matrix and can be obtained by
## inverting the Hessian.
fisherInfMatrix <- solve(optim.vals$hessian) ## invert the Hessian, to estimate the covar-var matrix of parameter estimates

## Initialize plot of parameters
plot(1,1, type = 'n', log = 'xy',
     ## xlim = range(alpha.seq), ylim = range(Beta.seq),
     xlim = c(2,15), ylim = c(.5,2),
     las = 1,
     xlab = expression(alpha), ylab = expression(beta),
     main = "-log(likelihood) contours", bty = "n")
## Add true parameter values to the plot
with(trueParms, points(alpha, Beta, pch = 16, cex = 2, col = 'red'))
## Add MLE to the plot
points(exp(MLEfits['log_alpha']), exp(MLEfits['log_Beta']), pch = 16, cex = 2, col = 'black')
## Add 95% contour ellipse from Hessian
lines(exp(ellipse(fisherInfMatrix, centre = MLEfits, level = .95)))
legend("topleft", c('truth', 'MLE', '95% Confidence Region'), lty = c(NA, NA, 1), pch = c(16,16, NA),
       col = c('red', 'black', 'black'), bg='white', bty = 'n')

######################################################################
## Contour plots with likelihood profiles
######################################################################
## With all other parameters fixed to their initial values, lets look at a
## contour likelihood plot over Beta and alpha.  To do this we write wrapper
## functions of log_Beta and log_alpha to feed to outer() and then
## contour(). This is confusing so make sure you understand every function.

## This function simply takes values log_Beta and log_alpha and feeds them into
## objFXN above as a single variable called logpars, you'll see why this is
## useful below.
objXalpha_Beta <- function(alpha, Beta, fixed.params = disease_params(), browse=F) {
  objFXN(fit.params = c(log_alpha = log(alpha), log_Beta = log(Beta))
         , fixed.params = fixed.params)
}


## Now instead of giving a single argument on the log scale we give 2
## on the untransformed scale.
objFXN(c(log_alpha = log(1/5), log_Beta = log(25)))
objXalpha_Beta(1/5, 25)

## If we try to give this function multiple values of alpha and Beta,
## however, it gets confused and only analyzes the first one.
objXalpha_Beta(c(1/5:8), 25:30)

## So we "Vectorize" this function so it can take in vectors of the parameters
## and return the output. objXalpha_BetaVEC then calls on objXalpha_Beta() to
## take xx,yy as pairwise values and returns objXalpha_Beta() for all pairs
objXalpha_BetaVEC <- Vectorize(objXalpha_Beta, list("alpha", "Beta"))

## Task: Explain how the following three lines are related.
objXalpha_Beta(25, 1/5)
objXalpha_Beta(26, 1/6)
objXalpha_Beta(8, .9)
objXalpha_BetaVEC(c(25:26), c(1/5,1/6))

## Now we use the R function outer() to evaluate objXalpha_BetaVEC() over a grid
## of {alpha, Beta} combinations. This can take a long time because we have to do
## res^2 evaluations of nllikelihood(), and recall that each time we do this we are
## running lsoda() inside simEpidemic()

## Grid resolution resXres, increasing it makes contours smoother but takes a lot longer
res <- 15

## Now create a sequence of alpha values for the grid
alpha.seq <- exp(seq(log_alpha.fit-1, log_alpha.fit+1, l = res))
alpha.seq

## Now create a sequence of Beta values for the grid
Beta.seq <- exp(seq(log_Beta.fit-1, log_Beta.fit + 1, l = res))
Beta.seq

## The function outer() now evaluates objXalpha_BetaVEC on this grid. ?outer
mat <- outer(alpha.seq, Beta.seq, objXalpha_BetaVEC) # this can take a long time

## Make a contour plot that shows the confidence regions in red.  Likelihood
## Ratio Test confidence regions use the chi squared distribution cutoff with
## degrees of freedom 2 (2 parameters)
ml.val <- optim.vals$value
conf.cutoff <- ml.val + qchisq(.95,2)/2

## Show likelihood contours
par(cex = 1.2)
plot(1,1, type = 'n', log = 'xy',
     ## xlim = range(alpha.seq), ylim = range(Beta.seq),
     xlim = c(3,15), ylim = c(.5,2),
     xlab = expression(alpha), ylab = expression(beta),
     main = "-log(likelihood) contours", bty = "n")
.filled.contour(alpha.seq, Beta.seq, mat, levels = seq(min(mat), max(mat), l=20), col = topo.colors(20))
## Add contour for 95% CI from likelihood ratio
contour(alpha.seq, Beta.seq, mat, levels = c(conf.cutoff),
        col = "black", lwd = 2, labels = "", labcex = .2, add = T)
## Add contour for 95% CI from Hessian
lines(exp(ellipse(fisherInfMatrix, centre = MLEfits, level = .95)), lty = 2)
## Add MLE to the plot
points(exp(log_alpha.fit), exp(log_Beta.fit), pch = 16, cex = 1, col = 'black')
## Add true parameter values to the plot
with(trueParms, points(alpha, Beta, pch = 16, cex = 1, col = 'red'))
legend("topleft",
       c('truth', 'MLE', '95% contour (profile likelihood)', '95% contour (Fisher information matrix)')
       , lty = c(NA, NA, 1, 2), pch = c(16,16, NA, NA),
       col = c('red', rep('black',3)), bg='white', bty = 'n')

######################################################
# EXERCISES
#
# 1. Evaluate how the MLE estimate and confidence interval changes when there is more or lest observational noise.
# (Hint: Change the number of people sampled during each surveillance study in the sampleEpidemic function.)
#
# 2. Evaluate how the MLE estimate and confidence interval changes when the frequency of surveillance.
#
# 3. Evaluate how the MLE estimate and confidence changes when the model used
# to conduct the estimation differs from the 'true' model. Specifically, see what
# happens if you only use two boxcars in the model, rather than four.
# (Hint: You will need to make a new function for the model used to calculate the likelihood)

