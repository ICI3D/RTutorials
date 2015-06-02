## Introduction to MCMC 1: Estimating a posterior binomial probability
## Steve Bellan 2015

## Meaningful Modeling of Epidemiologic Data, 2012 AIMS, Muizenberg

###################################################################### 

## By the end of this tutorial you should…

## * Understand how to write a flexible prior function for the binomial
## * Understand the Metropolis-Hasting algorithm and explain it step by step.
## * Know how the parameter proposal distribution affects MCMC convergence
## * Know how to assess MCMC convergence with the Gelman-Rubin diagnostic and with trace plots

library(boot); library(coda)
defaultPar <- par() ## Save the default graphic parameter settings for use later.

## First, let's pretend that we are sampling a population of 100 individuals with a 30% prevalence
## for some disease and testing each of them. From this, we have a sample prevalence, which is our
## estimate of the true prevalence. The problem considered in the simulation is to estimate the
## posterior probability distribution of the prevalence from this sample and from a specified
## (informative or uninformative) prior probability distribution for the prevalence.

size <- 100
truePrev <- .3
sampPos <- rbinom(1,size,truePrev) ## Sample from this distribution once.
sampPrev <- sampPos/size

## Now we need to specify our prior probability distribution. This designates our prior (before we
## conducted the study) beliefs of what we think the prevalence of this population is. Frequently,
## we have insufficient prior information to specify and informative prior. In these cases, we will
## use a flat prior, meaning that for the study we think every possible value of the prevalence has
## equal probability.

## Our parameter of interest is prevalence, which is bounded between 0 and 1. The beta probability
## distribution is particularly suited for such parameters and is, in fact, the most commonly used
## probability distribution for parameters that are, themselves, probabilities. It is uninformative
## (flat) with parameters shape1=1, shape2=1, and can otherwise have many shapes.

## We calculate both the prior and the likelihood on a log scale because it is easier to take the
## sum of their log's, than to take their product (particularly in problems with more data).
logBetaPrior <- function(prevalence
                         , shape1 = 1  ## Change to make informative (try 8)
                         , shape2 = 1) ## Change to make informative (try 40)
    dbeta(prevalence, shape1=shape1, shape2=shape2, log = T)

## The likelihood is simply the probability of observing that many individuals test positive given
## the number tested and a specified prevalence.
logLikelihood <- function(prevalence,
                          data = list(size=size, sampPos=sampPos))
    dbinom(data$sampPos, size = data$size, prob = prevalence, log = T)

## A convenience function that sums the log-likelihood and the log-prior.
logLikePrior <- function(prevalence
                         , shape1=1
                         , shape2=1
                         , data = list(size=size, sampPos=sampPos))
    logBetaPrior(prevalence, shape1, shape2) + logLikelihood(prevalence, data)

## Convenience functions that thought the un-logged functions above.
Prior <- function(x) exp(logBetaPrior(x))
Likelihood <- function(x) exp(logLikelihood(x))
LikePrior <- function(x) exp(logLikePrior(x))


par(mfrow = c(2,2) ## panels
    , mar = c(3,6,1,1) ## panel margins
    , bty='n' ## no box around plots
    , oma = c(1.5,0,0,0)) ## outer margins
curve(Prior(x), 0, 1, ylab = 'prior')
curve(Likelihood, 0, 1, ylab = 'likelihood')
curve(logLikePrior, 0,1, ylab = 'log(likelihood X prior)')
curve(LikePrior, 0, 1, ylab = 'likelihood X prior')
mtext('prevalence', 1, 0, outer=T)

## Explain what each of these plots represents in words.


## This functions runs a Markov chain Monte Carlo (MCMC) Metropolis-Hastings algorithm to
## numerically estimate the posterior probability distribution of the prevalence. For problems this
## simple, the posterior probability distribution can be solved for analytically. However, for
## problems more complex than this one (such as the Introduction to MCMC-SI_HIV tutorial), numerical
## integration using MCMC may be the only way to calculate the posterior probability distribution
## for parameters.

## We sample the prevalence parameter on the logistic-scale so that it is bounded by [-Inf, Inf] and not [0,1]
runMCMC <- function(iterations
                    , startvalue = runif(1, logit(.01), logit(.99)) ## random starting value
                    , proposerSD = .5 ## standard deviation of the gaussian proposal distribution
                    , verbose = 0){ ## for debugging
    if(verbose > 0) browser()
    chain <- array(dim = c(iterations+1, 1)) ## initialize empty iterations X 1 array
    chain[1,] <- startvalue ## set first value
    for(ii in 1:iterations){ 
        proposal <- rnorm(1, chain[ii,], proposerSD) ## propose next value
        MHratio <- exp(logLikePrior(inv.logit(proposal)) - 
                       logLikePrior(inv.logit(chain[ii,])))
        ## If the MH-ratio is > 1, accept new value. Otherwise, accept it with probability equal to
        ## the the MH-ratio.
        if(runif(1) < MHratio){ 
            chain[ii+1,] <- proposal
        }else{ ## If rejecting it, stay at the last state.
            chain[ii+1,] <- chain[ii,]
        }
    }
    return(inv.logit(chain)) ## 
}

posteriorSample <- runMCMC(1000, proposerSD = .1) ## sample 1000 times from posterior
par(defaultPar, bty = 'n')
plot(posteriorSample, xlab='iteration', ylab = 'prevalence', main = 'posterior sample',
     type = 'l', las = 1)

## Compare proposal distributions
par(mfrow = c(2,2), oma = c(0,0,2,0))
for(sdVal in c(.05, .1, .5, 1)) {
    posteriorSample <- runMCMC(1000, proposerSD = sdVal) 
    plot(posteriorSample, xlab='iteration', ylab = 'prevalence',
         main = bquote(sigma==.(sdVal)),
         ylim = c(0,1),
         type = 'l', las = 1)
}
mtext('posterior sample by proposer sd', side=3, line=0, outer=T)

####################################################################################################
## Questions
####################################################################################################

## Question 1: Write code to compare histograms of posterior samples
## from proposal distributions with the 4 different proposal standard
## deviations above for 3000 samples. Make sure each histogram has the
## same breaks and x-axis limits.

## Question 2: Sample 4 chains for 3000 iterations using the same
## proposal distribution and plot each chains' trace on the same plot.

## Question 3: Use the Gelman-Rubin diagnostic (gelman.diag) to assess
## whether those four chains have reached convergence. You will need
## to conver the chains from 2 into "mcmc" objects (as.mcmc()), and
## then put them into an mcmc.list (as.mcmc.list())

## Challenge Question: (A) Plot the Gelman-Rubin diagnostic as a function
## of chain length. (B) Do the same plot, but after discarding the first 100 iterations as a "burnin"

####################################################################################################
## Answers
####################################################################################################

## Q1
par(mfrow = c(2,2), oma = c(0,0,2,0))
for(sdVal in c(.05, .1, .5, 1)) {
    posteriorSample <- runMCMC(3000, proposerSD = sdVal) 
    hist(posteriorSample, xlab='prevalence', col = 'black',
         main = bquote(sigma==.(sdVal)),
         las = 1,
         breaks = seq(0,1,by=.01),
         xlim = c(0,1))
}
mtext('posterior sample by proposer sd', side=3, line=0, outer=T)

## Q2
numChains <- 4
numIter <- 3000
for(ii in 1:numChains) 
    assign(paste0('posteriorSample',ii), ## assign is like "<-" but can be done with text strings to name things
           as.mcmc(runMCMC(numIter, proposerSD = .1))) ## use as.mcmc to help with Gelman-Rubin diagnostic function below

dev.off()
par(defaultPar, bty = 'n')
plot(0,0, type = 'n', ## intialize
     xlim = c(1,numIter), ylim = c(0,1),
     xlab='iteration', ylab = 'prevalence', main = 'posterior sample',
     las = 1)
for(ii in 1:numChains) 
    lines(get(paste0('posteriorSample',ii)), col = rainbow(numChains)[ii]
          )

## Q3
chainList <- as.mcmc.list(list(posteriorSample1, posteriorSample2, posteriorSample3, posteriorSample4))
class(chainList)
gelman.diag(chainList)

## Challenge Question (A)
chainLengthVector <- seq(50, 3000, by = 10)
GRdVector <- c()
for(chainLength in chainLengthVector) {
    chainList <- as.mcmc.list(list(as.mcmc(posteriorSample1[1:chainLength,]),
                                   as.mcmc(posteriorSample2[1:chainLength,]),
                                   as.mcmc(posteriorSample3[1:chainLength,]),
                                   as.mcmc(posteriorSample4[1:chainLength,])))
    GRdVector <- c(GRdVector, as.numeric(gelman.diag(chainList)$psrf[,1]))
}

dev.off()
par(defaultPar, bty = 'n')
plot(chainLengthVector, GRdVector, xlab = 'chain length', ylab = 'Gelman-Rubin diagnostic',
     type = 'l', las = 1)

## Challenge Question (B)
burnin <- 100
chainLengthVector <- seq(150, 3000, by = 10)
GRdVector <- c()
for(chainLength in chainLengthVector) {
    chainList <- as.mcmc.list(list(as.mcmc(posteriorSample1[burnin:chainLength,]),
                                   as.mcmc(posteriorSample2[burnin:chainLength,]),
                                   as.mcmc(posteriorSample3[burnin:chainLength,]),
                                   as.mcmc(posteriorSample4[burnin:chainLength,])))
    GRdVector <- c(GRdVector, as.numeric(gelman.diag(chainList)$psrf[,1]))
}

dev.off()
par(defaultPar, bty = 'n')
plot(chainLengthVector, GRdVector, xlab = 'chain length', ylab = 'Gelman-Rubin diagnostic',
     type = 'l', las = 1)


