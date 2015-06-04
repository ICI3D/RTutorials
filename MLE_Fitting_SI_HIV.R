## Introduction to Likelihood 2: Fitting an SI model to an HIV epidemic with Maximum Likelihood Estimation
## Steve Bellan 2015

## Meaningful Modeling of Epidemiologic Data, 2015 AIMS, Muizenberg

###################################################################### 

## By the end of this tutorial you should…

## * Understand how to simulate cross-sectional prevalence data around a simulated epidemic.
## * Calculate a binomial likelihood from these prevalence data and a fully specified epidemic model.
## * Use R's "optim" function to do multivariate optimization over transformed parameter space.
## * Understand the difference betweeen SANN and Nelder-Mead algorithms
## * Create 95% CI's and contours from the hessian matrix
## * Create 95% CI's and contours from profile likelihoods

library(boot); library(deSolve)

## Function that makes a list of disease parameters with default values
disease_params <- function(Beta = 0.9
                           , alpha = 8 ## rate of beta decline with prevalence
                           , progRt = 1-exp(-1/2.77) #(1/10)*4 ## rate of of progression through each of the I classes, for 10 years total
                           , birthRt = .029 #.03 ## birth rate, 3% of people give birth per year
                           , deathRt = .018 #1/60 ## 60 year natural life expectancy
                           )
    return(as.list(environment()))

disease_params()
disease_params(Beta = .2)

initPrev <- exp(-7) #10^-7 ## 1/10,000 infected at start
init <- c(S=1, I1=initPrev, I2=0, I3=0, I4=0, CI = 0, CD = 0) ## modeling proportion of population
tseq <- seq(1976, 2015, by = 1/12)
Is <- paste0('I',1:4) ## for easy indexing

## SI ODE model
SImod <- function(tt, yy, parms) with(parms, {
    ## state variables
    S <- yy[1]  ## Susceptibles
    I1 <- yy[2] ## HIV stage 1
    I2 <- yy[3] ## HIV stage 2
    I3 <- yy[4] ## HIV stage 3
    I4 <- yy[5] ## HIV stage 4
    CI <- yy[6] ## cumulative incidence
    CD <- yy[7] ## cumulative mortality
    ## derived quantitties
    I <- I1+I2+I3+I4           ## total infecteds
    N <- I + S                 ## total population
    transmissionCoef <- Beta * exp(-alpha * I/N) ## Infectious contact rate
    ## state variable derivatives (ODE system)
    deriv <- rep(NA,5)
    deriv[1] <-	birthRt*N - deathRt*S - transmissionCoef*S*I/N
    deriv[2] <-	transmissionCoef*S*I/N - progRt*I1 - deathRt*I1
    deriv[3] <-	progRt*I1 - progRt*I2 - deathRt*I2
    deriv[4] <-	progRt*I2 - progRt*I3 - deathRt*I3
    deriv[5] <-	progRt*I3 - progRt*I4 - deathRt*I4
    deriv[6] <-	transmissionCoef*S*I/N 
    deriv[7] <-	progRt*I4
    return(list(deriv))
})

simEpidemic <- function(init, tseq, SImod=SImod, parms = disease_params()) {
    simDat <- as.data.frame(lsoda(init, tseq, SImod, parms=parms))
    simDat$I <- rowSums(simDat[, Is])
    simDat$N <- rowSums(simDat[, c('S',Is)])
    simDat$P <- with(simDat, I/N)
    simDat
}

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
trueParms <- disease_params()#Beta = .9, alpha = 8, progRt = 1/2.5)
simDat <- simEpidemic(init, tseq, SImod, parms = trueParms)
par(bty='n', lwd = 2)
with(simDat, plot(time, P, xlab = '', ylab = 'prevalence', type = 'l', ylim = c(0,.4), col='red'))

## Take cross-sectional sample of individuals to get prevalence estimates at multiple time points
myDat <- sampleEpidemic(simDat, verbose = 0)
points(obsDat$time, obsDat$sampPrev, col = 'red', pch = 16, cex = 2)
arrows(obsDat$time, obsDat$uci, obsDat$time, obsDat$lci, col = 'red', len = .025, angle = 90, code = 3)

## Log-Likelihood
llikelihood <- function(parms = disease_params(), obsDat=myDat, verbose = 0) {
    simDat <- as.data.frame(lsoda(init, tseq, SImod, parms=parms))
    simDat$I <- rowSums(simDat[, Is])
    simDat$N <- rowSums(simDat[, c('S',Is)])
    simDat$P <- with(simDat, I/N)
    if(verbose > 5) browser()
    matchedTimes <- simDat$time %in% obsDat$time
    lls <- dbinom(obsDat$numPos, obsDat$numSamp, prob = simDat$P[matchedTimes], log = T)
    return(sum(lls))
}
llikelihood()

objFXN <- function(fit.params ## paramters to fit
                   , ref.params =disease_params() ## fixed paramters
                   , obsDat=myDat
                   , verbose=0) {
    if(verbose > 3) browser()
    parms <- within(ref.params, { ## subs fitting parameters into reference parameter vector
        loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
        unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]        
        for(nm in unloggedParms) assign(nm, as.numeric(fit.params[nm]))
        for(nm in loggedParms) assign(gsub('log_','',nm), exp(as.numeric(fit.params[nm])))
        rm(nm)
    })
    - llikelihood(parms, obsDat = obsDat, verbose) ## then call likelihood
}
objFXN(c(log_Beta = log(5), log_alpha = log(8)), ref.params = disease_params())

optim.vals <- optim(par = c(log_Beta = .2, log_alpha = 3)
                    , objFXN
                    , ref.params = disease_params()
                    , obsDat = obsDat
                    , control = list(trace = 0, maxit = 150)
                    , method = "SANN")
exp(optim.vals$par)
trueParms[c('Beta','alpha')]

optim.vals <- optim(par = optim.vals$par
                    , objFXN
                    , ref.params = disease_params()
                    , obsDat = obsDat
                    , control = list(trace = 1, maxit = 500, reltol = 10^-7)
                    , method = "Nelder-Mead"
                    , hessian = T)
exp(optim.vals$par)
trueParms[c('Beta','alpha')]
######################################################################
## Contour plots with the hessian

optim.vals$hessian

######################################################################
## Contour plots with likelihood profiles
######################################################################
## With all other parameters fixed to their initial values, lets look at a contour likelihood plot
## over Beta and alpha.  To do this we write wrapper functions of log_Beta and log_alpha to feed to
## outer() and then contour(). This is confusing so make sure you understand every function.

## This function simply takes values log_Beta and log_alpha and feeds them into objFXN above as a
## single variable called logpars, you'll see why this is useful below.
objXBeta_alpha <- function(Beta, alpha, ref.params = disease_params(), browse=F)
    objFXN(fit.params = c(log_Beta = log(Beta), log_alpha = log(alpha))
               , ref.params = ref.params)

## Now instead of giving a single argument on the log scale we give 2
## on the untransformed scale.
objFXN(c(log_Beta = log(25), log_alpha = log(1/5)))
objXBeta_alpha(25, 1/5)

## If we try to give this function multiple values of R0 or gamma,
## however, it gets confused and only analyzes the first one.
objXBeta_alpha(c(25:30), c(1/5:8))

## So we "Vectorize" this function so it can take in vectors of the
## parameters and return the output. objXBeta_alphaVEC then calls on objXBeta_alpha() to take
## xx,yy as pairwise values and objXBeta_alpha() for all pairs
objXBeta_alphaVEC <- Vectorize(objXBeta_alpha, list("Beta","alpha"))

## Task 4: Explain how the following three lines are related.
objXBeta_alpha(25, 1/5)
objXBeta_alpha(26, 1/6)
objXBeta_alphaVEC(c(25:26), c(1/5,1/6))

## Now we use the R function outer() to evaluate objXBeta_alphaVEC() over a grid of
## {R0, gamma} combinations. This can take a long time because we have
## to do res^2 evaluations of nll.fn(), and recall that each time we
## do this we are running lsoda() inside nll.fn()

## Grid resolution resXres, increasing it makes contours smoother but takes a lot longer
res <- 15

## Now create a sequence of Beta values for the grid
log_Beta.fit <- optim.vals$par["log_Beta"]
## let's have the sequence be spaced arouund that
Beta.seq <- exp(seq(log_Beta.fit-.5, log_Beta.fit + .5, l = res))
Beta.seq

## Now create a sequence of alpha values for the grid
log_alpha.fit <- optim.vals$par["log_alpha"]
alpha.seq <- exp(seq(log_alpha.fit-.4, log_alpha.fit+.4, l = res))
alpha.seq

## The function outer() now evaluates objXBeta_alphaVEC on this grid.
?outer
mat <- outer(Beta.seq, alpha.seq, objXBeta_alphaVEC) # this can take a long time

## Make a contour plot that shows the confidence intervals in red.  Likelihood Ratio Test confidence
## intervals uses chi squared distribution cutoff with degrees of freedom 2 (2 parameters)
ml.val <- optim.vals$value
conf.cutoff <- ml.val + qchisq(.95,2)/2

## Show likelihood contours
contour(alpha.seq, Beta.seq, mat, xlab = expression(alpha), ylab = expression(beta),
        main = "-log(likelihood) contours", bty = "n")
## Add red contour for 95% CI
contour(alpha.seq, Beta.seq, mat, levels = c(conf.cutoff),
        col = "red", lwd = 2, labels = "", labcex = .2, add = T)
## Add MLE to the plot
points(exp(log_alpha.fit), exp(log_Beta.fit), pch = 16, cex = 1, col = 'red')
legend("topright", c('MLE', "95% Confidence Interval"), lty = c(NA, 1), pch = c(16, NA),
       col = "red")
