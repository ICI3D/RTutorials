## Introduction to MCMC 2: Fitting an SI model to an HIV epidemic with adaptive block MCMC
## Steve Bellan 2015

## Meaningful Modeling of Epidemiologic Data, 2015 AIMS, Muizenberg

###################################################################### 

## By the end of this tutorial you should…

## * Understand how to simulate cross-sectional prevalence data around a simulated epidemic.
## * Calculate a binomial likelihood from these prevalence data and a fully specified epidemic model.
## * Be able to explain sequential, block, and adative Metropolis Hastings sampling algorithms.
## * Know how to assess multivariate MCMC chains for convergence both with trace plots and the Gelman-Rubin diagnostic.

library(boot); library(deSolve); library(coda)

## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.3
                           , alpha = 1 ## rate of beta decline with prevalence
                           , progRt = (1/10)*4 ## rate of of progression through each of the I classes, for 10 years total
                           , birthRt = .03 ## birth rate, 3% of people give birth per year
                           , deathRt = 1/60 ## 60 year natural life expectancy
                           )
    return(as.list(environment()))

disease_params()
disease_params(Beta = .2)

initPrev <- 10^-4 ## 1/10,000 infected at start
init <- c(S=1-initPrev, I1=initPrev, I2=0, I3=0, I4=0, CI = 0, CD = 0) ## modeling proportion of population
tseq <- seq(1970, 2015, by = 1/12)
Is <- paste0('I',1:4) ## for easy indexing

## SI ODE model
SImod <- function(tt, yy, parms) with(parms, {
    ## state variables
    S <- yy[1]  ## Susceptibles
    I1 <- yy[2] ## HIV stage 1
    I2 <- yy[3] ## HIV stage 2
    I3 <- yy[4] ## HIV stage 3
    I4 <- yy[5] ## HIV stage 4
    CI <- yy[7] ## cumulative incidence
    CD <- yy[8] ## cumulative mortality
    ## derived quantitties
    I <- I1+I2+I3+I4           ## total infecteds
    N <- I + S                 ## total population
    mort <- progRt * I4 / N ## HIV-related mortality
    FOI <- Beta * exp(-alpha * I/N) ## Force of infection
    ## state variable derivatives (ODE system)
    deriv <- rep(NA,5)  
    deriv[1]<-	birthRt*N - deathRt*S - FOI*S*I/N
    deriv[2]<-	FOI*S*I/N - progRt*I1 - deathRt*I1
    deriv[3]<-	progRt*I1 - progRt*I2 - deathRt*I2
    deriv[4]<-	progRt*I2 - progRt*I3 - deathRt*I3
    deriv[5]<-	progRt*I3 - progRt*I4 - deathRt*I4
    deriv[6]<-	FOI*S*I/N 
    deriv[7]<-	progRt*I4
    return(list(deriv))
})

## From a simulated epidemic, measure prevalence at several time points by performing
## cross-sectional samples of individual at each time, testing them, and then calculated sample
## prevalence and associated binomial confidence intervals
sampleEpidemic <- function(simDat, tseq = seq(1978, 2010, by = 2), numSamp = rep(100, length(tseq)), verbose=0) {
    if(verbose>0) browser()
    simDat$I <- rowSums(simDat[, Is])
    prev_at_sample_times <- simDat[simDat$time %in% tseq, 'I']
    numPos <- rbinom(length(numSamp), numSamp, prev_at_sample_times)
    lci <- mapply(function(x,n) binom.test(x,n)$conf.int[1], x = numPos, n = numSamp)
    uci <- mapply(function(x,n) binom.test(x,n)$conf.int[2], x = numPos, n = numSamp)    
    return(data.frame(time = tseq, numPos, numSamp, sampPrev =  numPos/numSamp,
                      lci = lci, uci = uci))
}

## Run system of ODEs for "true" parameter values
set.seed(4)
trueParms <- disease_params(Beta = .9, alpha = 8, progRt = 1/2.5)
simDat <- as.data.frame(lsoda(init, tseq, SImod, parms=trueParms))
simDat$I <- rowSums(simDat[, Is])
simDat$N <- rowSums(simDat[, c('S',Is)])
plot(simDat$time, simDat$I, xlab = '', ylab = 'prevalence', type = 'l', ylim = c(0,.4), col='red')

## Take cross-sectional sample of individuals to get prevalence estimates at multiple time points
obsDat <- sampleEpidemic(simDat, verbose = 0)
points(obsDat$time, obsDat$sampPrev, col = 'red', pch = 16, cex = 2)
arrows(obsDat$time, obsDat$uci, obsDat$time, obsDat$lci, col = makeTransparent('red'), len = .025, angle = 90, code = 3)

## Log-Likelihood
llikelihood <- function(parms = disease_params(), obsDat, verbose = 0) {
    simDat <- as.data.frame(lsoda(init, tseq, SImod, parms=parms))
    simDat$I <- rowSums(simDat[, Is])
    if(verbose > 5) browser()
    lls <- dbinom(obsDat$numPos, obsDat$numSamp, prob = simDat$I[simDat$time %in% obsDat$time], log = T)
    return(sum(lls))
}
llikelihood(obsDat=obsDat)

## Log-Prior (assume uninformative)
lprior <- function(parms=disease_params()) with(parms, {
    lp <- 0
    return(lp)
})

llikePrior <- function(fit.params=NULL, ## parameters to fit
                       ref.params = disease_params(), ## reference parameters
                       obsDat, verbose = 0) { ## observed data
    parms <- within(ref.params, { ## subs fitting parameters into reference parameter vector
        for(nm in names(fit.params)) assign(nm, as.numeric(fit.params[nm]))
        rm(nm)
    })
    llikelihood(parms, obsDat=obsDat, verbose = verbose) + lprior(parms)
}
llikePrior(obsDat=obsDat)

logParms <- function(fit.params) {
    fit.params <- log(fit.params)
    names(fit.params) <- paste0('log',names(fit.params))
    return(fit.params)
}
unlogParms <- function(fit.params) {
    fit.params <- exp(fit.params)
    names(fit.params) <- sub('log','', names(fit.params))
    return(fit.params)
}
unlogParms(logParms(c(alpha = 3, Beta=.3)))

initBounds <- data.frame(rbind( ## for initial conditions
                               c(log(.2),log(2)) ## beta
                               ,c(log(1), log(30)) ## alpha
                               ,c(log(1),log(1/10)))) ## progRt
colnames(initBounds) <- c('lower','upper')
rownames(initBounds) <- c('logBeta','logalpha','logprogRt')
class(initBounds[,2]) <- class(initBounds[,1]) <- 'numeric'
initBounds

initRand <- function(fit.params) {
    fit.params <- logParms(fit.params)
    tempnm <- names(fit.params)
    for(nm in tempnm) fit.params[nm] <- runif(1, min = initBounds[rownames(initBounds)==nm, 'lower'], 
                                              max =  initBounds[row.names(initBounds)==nm, 'upper'])
    return(unlogParms(fit.params))
}
initRand(c(alpha = 3, Beta = 1))


mcmcSampler <- function(current.params, ref.params=disease_params(), obsDat, seed = 1,
                        proposer = sequential.proposer(sdProps=sdProps),
                        adaptiveMCMC = F, startAdapt = 300,
                        plotterLoops = NULL, ## repeat plot of some iterations more to lengthen their display
                        showing = showingFXN(),
                        plotter = plotterTS, randInit = T, niter = 100, nburn = 0, adptBurn = 200,
                        verbose=0, plotNM=NULL, tell = 100) {
    if(verbose>2) browser()
    if(randInit) current.params <- initRand(current.params)
    nfitted <- length(current.params)
    vv <- 2 ## mcmc iteration
    accept <- 0
    curVal <- llikePrior(current.params, ref.params = ref.params, obsDat=obsDat, verbose = verbose)
    out <- matrix(NA, nr = niter, nc=length(current.params)+1)
    out[1,] <- c(current.params, nll = -curVal)
    colnames(out) <- c(names(current.params), 'nll')
    max_index <- dim(out)[1]
    last.it <- 0
    ## Store original covariance matrix
    if(proposer$type=='block') originalCovar <- get('covar', envir = environment(proposer$fxn)) 
    while(vv <= max_index) {
        if ((verbose > 1) || (verbose && (vv%%tell == 0))) print(paste("on iteration",vv,"of",last.it + niter + 1))
        ## Adaptive MCMC
        ## adapt covariance every 50 iterations
        if(adaptiveMCMC & proposer$type=='block' & vv > startAdapt & vv %% 50 == 0) {
            adptBurn <- min((startAdapt-50), adptBurn)
            save(list = ls(all.names = TRUE), file='dbgAdapt.Rdata')
            ## load(file='dbgAdapt.Rdata')
            adaptedCovar <- 2.38^2 / nfitted * cov.wt(log(out[adptBurn:(vv-1),1:nfitted]))$cov ## will converge for vv large
            adaptedCovar <- adaptedCovar*.95 + originalCovar*.05 ## 95% adapted & 5% original
            rownames(adaptedCovar) <- colnames(adaptedCovar) <- names(current.params)
            assign('covar', adaptedCovar, envir = environment(proposer$fxn))
        }
        proposal0 <- proposer$fxn(logParms(current.params))
        onpar <- proposal0$onpar
        propt <- proposal0$type
        proposal <- unlogParms(proposal0$proposal)
        propVal <- llikePrior(proposal, ref.params = ref.params, obsDat=obsDat, verbose = verbose)
        lmh <- propVal - curVal ## likelihood ratio = log likelihood difference
        if (is.na(lmh)) { ## if NA, print informative info but don't accept it
            print(list(lmh=lmh, proposal=exp(proposal), vv=vv, seed=seed))
        } else { ## if it's not NA then do acception/rejection algorithm
            if (verbose > 1) print( c(lmh=lmh, propVal=propVal) )
            ## if MHR >= 1 or a uniform random # in [0,1] is <= MHR, accept otherwise reject
            if ( (lmh >= 0) | (runif(1,0,1) <= exp(lmh)) ) {
                current.params <- proposal
                if (vv>nburn) accept <- accept + 1 #only track acceptance after burn-in
                curVal <- propVal
            }
        }
        out[vv, ] <- c(current.params, nll=curVal)
        vv <- vv+1
        aratio <- accept/((vv-nburn))
        if(!is.null(plotter)) {
                par(opar)
                plotter(out, vv, ref.params=ref.params, obsDat=obsDat, proposer = proposer, proposal = proposal,
                        onpar=onpar,proptype=propt, aratio = aratio, showing = showing, plotterLoops=plotterLoops)
        }
    }
    if(!is.null(plotNM)) graphics.off()
    colnames(out) <- c(names(current.params), 'nll')
    return(list(out = out[1:nrow(out)>(nburn+1),], aratio = aratio, current.params = current.params,
                ref.params=ref.params))
}
