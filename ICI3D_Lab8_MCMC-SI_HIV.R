## Introduction to MCMC 2: Fitting an SI model to an HIV epidemic with adaptive block MCMC
## Steve Bellan 2015

## Meaningful Modeling of Epidemiologic Data, 2015 AIMS, Muizenberg

###################################################################### 

## By the end of this tutorial you should…

## * Understand how to simulate cross-sectional prevalence data around a simulated epidemic.
## * Calculate a binomial likelihood from these prevalence data and a fully specified epidemic model.
## * Be able to explain sequential, block, and adative Metropolis Hastings sampling algorithms.
## * Know how to assess multivariate MCMC chains for convergence both with trace plots and the Gelman-Rubin diagnostic.
## * Create 95% credible intervals (CrI's) and contours from the posterior

require(boot); require(deSolve); require(ellipse); require(coda); require(parallel); require(mnormt); require(emdbook)

## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9
                           , alpha = 8 ## rate of beta decline with prevalence
                           , progRt = (1/10)*4 ## rate of of progression through each of the I classes, for 10 years total
                           , birthRt = .03 ## birth rate, 3% of people give birth per year
                           , deathRt = 1/60 ## 60 year natural life expectancy
                           )
    return(as.list(environment()))

disease_params()
disease_params(Beta = .2)

initPrev <- exp(-7) ## infected at start
tseqMonth <- seq(1976, 2015, by = 1/12)
init <- c(S=1, I1=initPrev, I2=0, I3=0, I4=0, CI = 0, CD = 0) ## modeling proportion of population
Is <- paste0('I',1:4) ## for easy indexing

## Define the SI ODE model. This model is equivalent to the model in HIV Spreadsheet #3. 
SImod <- function(tt, yy, parms) with(c(parms,as.list(yy)), {
    ## State variables are: S, I1, I2, I3, I4
    ## derived quantitties
    I <- I1+I2+I3+I4           ## total infecteds
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
## From a simulated epidemic, measure prevalence at several time points by performing
## cross-sectional samples of individuals at each time, testing them, and then calculating sample
## prevalence and associated binomial confidence intervals
sampleEpidemic <- function(simDat # Simulated data (produced by a call to simEpidemic()) representing the 'true' underlying epidemic trajectory
                           , sampleDates = seq(1980, 2010, by = 3) # Sample every 3 years from 1980 to 2010
                           , numSamp = rep(80, length(sampleDates)) # Number of individuals sampled at each time point
                           ) {
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
arrows(myDat$time, myDat$uci, myDat$time, myDat$lci, col = 'red', len = .025, angle = 90, code = 3) # Plot 95% CI's around the sample prevalences

## To start, we need to write a -log likelihood function that gives the probability that a given parameter set
## (Beta, alpha values) would generate the observed data. Remember that we are assuming that there is some
## true underlying epidemic curve that is deterministic and the data we observe are only noisy
## because of sampling/observation error (not because the underying curve is also
## noisy--i.e. process error--which would be particularly likely for epidemics in small populations).

## We assume binomial sampling errors. So we can write the -log-likelihood as the probability of
## observing the observed number positive out of each sample if the prevalence is the value generated by
## a model parameterized by a given set of parameters:

nllikelihood <- function(parms = disease_params(), obsDat=myDat) {
    simDat <- simEpidemic(init, parms=parms)
    ## What are the rows from our simulation at which we have observed data?
    matchedTimes <- simDat$time %in% obsDat$time
    nlls <- - dbinom(obsDat$numPos, obsDat$numSamp, prob = simDat$P[matchedTimes], log = T)
    return(sum(nlls))
}
nllikelihood(trueParms) ## loglikelihood of the true parameters (which we usually never know)
nllikelihood(disease_params(Beta = 3, alpha = 1))  ## vs some random guessed parameters
 
## Log-Prior (assume uninformative)
lprior <- function(parms=disease_params()) with(parms, {
    lp <- 0     ## whatever the parameters are, assume they have the same probability
    return(lp)
})

## Convenience function that sums log-likelihood & log-prior for
## evaluation inside MCMC sampler.
llikePrior <- function(fit.params=NULL, ## parameters to fit
                       ref.params = disease_params(), ## reference parameters
                       obsDat=myDat) { ## observed data
    parms <- within(ref.params, { ## subs fitting parameters into reference parameter vector
        for(nm in names(fit.params)) assign(nm, as.numeric(fit.params[nm]))
        rm(nm)
    })
    -nllikelihood(parms, obsDat=obsDat) + lprior(parms)
}
llikePrior(obsDat=myDat)

## Want to be able to easily log and unlog parameters
logParms <- function(fit.params) {
    fit.params <- log(fit.params)
    names(fit.params) <- paste0('log',names(fit.params))
    return(fit.params)
}
logParms(c(alpha = 3, Beta=.3))
unlogParms <- function(fit.params) {
    fit.params <- exp(fit.params)
    names(fit.params) <- sub('log','', names(fit.params))
    return(fit.params)
}
unlogParms(logParms(c(alpha = 3, Beta=.3)))

## set bounds on initial parameter guesses
initBounds <- data.frame(rbind( ## for initial conditions
                               c(log(.2),log(2)) ## beta
                               ,c(log(1), log(30)) ## alpha
                               ,c(log(1),log(1/10)))) ## progRt
colnames(initBounds) <- c('lower','upper')
rownames(initBounds) <- c('logBeta','logalpha','logprogRt')
class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'
initBounds

##  randomly select a value that is uniformly distributed between these bounds
initRand <- function(fit.params) {
    fit.params <- logParms(fit.params)
    tempnm <- names(fit.params)
    for(nm in tempnm) fit.params[nm] <- runif(1, min = initBounds[rownames(initBounds)==nm, 'lower'], 
                                              max =  initBounds[row.names(initBounds)==nm, 'upper'])
    return(unlogParms(fit.params))
}
## give it parameter vector and it will simulate random values within the bounds for those values
initRand(c(alpha = 3, Beta = 1)) 
initRand(c(alpha = NA, Beta = NA)) ## independent of the inputs, just requires the parameter vector names

## Flexible Metropolis-Hastings Sampler
mcmcSampler <- function(init.params, ## initial parameter guess
                        randInit = T, ## if T then randomly sample initial parameters instead of above value
                        seed = 1, ## RNG seed
                        ref.params=disease_params(), ## fixed parameters
                        obsDat = myDat, ## data
                        proposer = sequential.proposer(sdProps=sdProps), ## proposal distribution
                        niter = 100, ## MCMC iterations
                        nburn = 0, ## iterations to automatically burn
                        adaptiveMCMC = F, ## adapt proposal distribution?
                        startAdapt = 150, ## start adapting at what iteration?
                        adptBurn = 200, ## ignore first so many iterations for adapting posterior
                        verbose=0, ## if >2 browses, if >1 prints progress
                        tell = 100) { ## how often to print progress
    if(verbose>2) browser()
    if(randInit) init.params <- initRand(init.params)
    current.params <- init.params
    nfitted <- length(current.params) ## number fitted parameters
    vv <- 2 ## mcmc iteration (started at 1 so we're already on 2
    accept <- 0 ## initialize proportion of iterations accepted
    ## Calculate log(likelihood X prior) for first value
    curVal <- llikePrior(current.params, ref.params = ref.params, obsDat=obsDat)
    ## Initialize matrix to store MCMC chain
    out <- matrix(NA, nr = niter, nc=length(current.params)+1)
    out[1,] <- c(current.params, ll = -curVal) ## add first value
    colnames(out) <- c(names(current.params), 'll') ## name columns
    ## Store original covariance matrix
    if(proposer$type=='block') originalCovar <- get('covar', envir = environment(proposer$fxn)) 
    while(vv <= niter) {
        if ((verbose > 1) || (verbose && (vv%%tell == 0))) print(paste("on iteration",vv,"of", niter + 1))
        ## Adaptive MCMC: adapt covariance every 50 iterations (don't
        ## do it more often because it adds to coputational burden.
        if(adaptiveMCMC & proposer$type=='block' & vv > startAdapt & vv %% 50 == 0) {
            adptBurn <- min((startAdapt-50), adptBurn)
            ## Below equation gives ideal covariance-variance matrix based on posterior
            adaptedCovar <- 2.38^2 / nfitted * cov.wt(log(out[adptBurn:(vv-1),1:nfitted]))$cov
            ## Take a weighted average of the original & the empirical cov-var matrices to ensure
            ## that we never let the matrix collapse to zero (ie if the empirical one is zero
            ## because we haven't accepted anything yet)
            adaptedCovar <- adaptedCovar*.95 + originalCovar*.05 ## 95% adapted & 5% original
            rownames(adaptedCovar) <- colnames(adaptedCovar) <- names(current.params)
            assign('covar', adaptedCovar, envir = environment(proposer$fxn))
        }
        proposal <- proposer$fxn(logParms(current.params))
        proposal <- unlogParms(proposal)
        propVal <- llikePrior(proposal, ref.params = ref.params, obsDat=obsDat)
        lmh <- propVal - curVal ## likelihood ratio = log likelihood difference
        if (is.na(lmh)) { ## if NA, print informative info but don't accept it
            print(list(lmh=lmh, proposal=exp(proposal), vv=vv, seed=seed))
        } else { ## if it's not NA then do acception/rejection algorithm
            if (verbose > 1) print( c(lmh=lmh, propVal=propVal) )
            ## if MHR >= 1 or a uniform random # in [0,1] is <= MHR, accept otherwise reject
            if ( (lmh >= 0) | (runif(1,0,1) <= exp(lmh)) ) {
                current.params <- proposal
                if (vv>nburn) accept <- accept + 1 ## only track acceptance after burn-in
                curVal <- propVal
            }
        }
        out[vv, ] <- c(current.params, ll=curVal)
        vv <- vv+1
        aratio <- accept/((vv-nburn))
    }
    colnames(out) <- c(names(current.params), 'll')
    samp <- as.mcmc(out[1:nrow(out)>(nburn+1),])
    return(list(ref.params=ref.params
              , seed = seed
              , init.params = init.params
              , aratio = aratio
              , samp = samp
                ))
}

## Sequential proposal function: Propose one parameter at a time
sequential.proposer <- function(sdProps) {
    nfitted <- length(sdProps)
    on <- 0
    return(list(sdProps = sdProps, type = 'sequential',
                fxn = function(current) {
                    proposal <- current
                    proposal[on + 1] <- proposal[on + 1] + rnorm(1, mean = 0, sd = sdProps[on + 1])
                    on <<- (on+1) %% nfitted
                    proposal
                }))
}


## Propose parameters within blocks
multiv.proposer <- function(covar, blockLS = list(rownames(covar))) {
    nblocks <- length(blockLS)
    on <- 0
    return(list(type = 'block',
                fxn = function(current) {
                    proposal <- current + rmnorm(1, mean = 0, varcov = covar)
                    propsosal <- as.vector(proposal)
                    names(proposal) <- names(current)
                    proposal
                }))
}

samp_Seq <- mcmcSampler(init.params = c(alpha=8, Beta=.9)
                      , seed = 1
                      , proposer = sequential.proposer(sdProps=c(.15,.15))
                      , randInit = T
                      , niter = 100)


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
mcmcParams <- list(init.params = c(alpha=8, Beta=.9)
                      , seed = 1
                      , proposer = sequential.proposer(sdProps=c(.15,.15))
                      , randInit = T
                      , niter = 10)

## Parallel MCMC: Let's write a function that uses mclapply to call on
## mcmcSampler once for each seed on a different core. We need a
## different seed to make sure each chain has different random number
## generation. It's good practice to always initiate a seed so that
## bugs are always reproducible.
doChains <- function(x, mcmcParams) {
    print(system.time(
        ## Below line uses mclapply to parallelize do.call over seeds
    chains <- mclapply(x, function(x) do.call(mcmcSampler, within(mcmcParams, { seed <- x})))
        ))
    aratio <- mean(unlist(lapply(chains, '[[', 'aratio'))) ## average across chains
    chains <- lapply(chains, '[[', 'samp') ## pull out posterior samples only
    chains <- as.mcmc.list(chains) ## make into mcmc.list
    return(list(chains=chains, aratio = aratio))
}

## How does the compute time increase with number of chains?
run1 <- doChains(1, mcmcParams) ## do one chain with a seed at 1
run2 <- doChains(1:2, mcmcParams) ## do two chains with seeds at 1:2
run3 <- doChains(1:3, mcmcParams) ## do two chains with seeds at 1:3
run4 <- doChains(1:4, mcmcParams) ## do two chains with seeds at 1:4
run5 <- doChains(1:5, mcmcParams) ## do two chains with seeds at 1:5

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
    verbose <- 1
    tell <- 100
})
mcmcParams_Adaptive <- within(mcmcParams, {
                      proposer <- multiv.proposer(covar=matrix(c(.1,0,0,.1),2,2))
                  })

## run4 <- doChains(1:4, mcmcParams) ## do two chains with seeds at 1:2
## run4A <- doChains(1:4, mcmcParams_Adaptive) ## do two chains with seeds at 1:2
## save(run4, run4A, file = 'MCMC_SI_runs.Rdata')
load(file = 'MCMC_SI_runs.Rdata')

run4$aratio
run4A$aratio

par(oma = c(0,0,2,0), bty='n', 'ps' = 18)
plot(run4$chains)
mtext('Sequential Sampling', side = 3, outer = T, line = 0)
 
plot(run4A$chains)
mtext('Adaptive Sampling', side = 3, outer = T, line = 0)

graphics.off()

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

