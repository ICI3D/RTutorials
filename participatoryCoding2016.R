##################################################
## Introduction to Sampling & Variability
## Steve Bellan
## Meaningful Modeling of Epidemiological Data 2016
## African Institute of Mathematical Sciences, Muizenberg, South Africa

##################################################
## Participatory Coding
##################################################

## How does travel amongst Haitian immigrants in Florida impact active TB prevalence?

## Case-control: compare TB cases & matched controls in terms of whether they've traveled?

## Alternative question phrasing: How would the prevalence of active TB be different if none of this population had traveled? i.e. indirect effects

## consider other indirect relationships between travel and having active TB (trt disruption)

##############################
## We'll go with this question to begin
## 
## Cross-sectional: screen for active TB amongst a population and also ask people if they've traveled in the past 2 years?

## think about selection biases?

npop <- 1000
travelProb <- 0.63
baselineTB <- .01
truePrevR <- 2.1

tbPrev <- c(notravel = baselineTB,
            travel = baselineTB*truePrevR)

set.seed(1)

dat <- data.frame(id = 1:npop)
dat$travel <- rbinom(npop, 1, prob = travelProb)
table(dat$travel)
dat$travel <- factor(dat$travel, labels = names(tbPrev))
dat$tb <- rbinom(npop, 1, prob = tbPrev[dat$travel])
head(dat); tail(dat)
xtabs(~ tb + travel, data = dat)

prevR <- function(dat) {
    prevTrav <- with(dat, mean(tb[travel=='travel']))
    prevNoTrav <- with(dat, mean(tb[travel=='notravel']))
    return(prevTrav/prevNoTrav)
}
realPR <- prevR(dat)

permTest <- function(dat, nperms = 999, browse=F) {
    if(browse) browser()
    permPrevR <- rep(NA, nperms)
    for(ii in 1:nperms) {
        permDat <- dat
        permDat$tb <- sample(permDat$tb, size = npop,
                             replace=F)
        permPrevR[ii] <- prevR(permDat)
    }
    return(permPrevR)
}

nullPrevR <- permTest(dat, nperms =9999, browse=F)
par('ps'=25, mar = rep(6,4))
hist(log(nullPrevR), xlab = 'prevalence ratio (trav/notrav)',
     col = 'black', breaks = 100
     ) ## make sure PR is shown on a scale so that 2 and 0.5 look reasonably similar
abline(v = log(1), lty = 2, col = 'red', lwd = 3) ## null hypothesis
abline(v = log(realPR), lty = 1, col = 'orange', lwd = 5) ## real PR

pValFxn <- function(FullVector, ObsVal)
    2*min(mean(FullVector >= ObsVal), mean(FullVector <= ObsVal))

pValFxn(c(nullPrevR,realPR), realPR)

powCalc <- function(nruns = 10, npop = 1000, travelProb = 0.63,
                    ## tbPrev = c(notravel = 0.01, travel = 0.021),
                    baselineTB = .01,
                    truePrevR = 2.1,
                    nperms = 999, browse=F) {
    pvalV <- rep(NA, nruns)
    if(browse) browser()
    tbPrev <- c(notravel = baselineTB,
                travel = baselineTB*truePrevR)
    for(jj in 1:nruns) {
        print(jj)
        ## simulate "real data"
        dat <- data.frame(id = 1:npop)
        dat$travel <- rbinom(npop, 1, prob = travelProb)
        dat$travel <- factor(dat$travel, labels = names(tbPrev))
        dat$tb <- rbinom(npop, 1, prob = tbPrev[dat$travel])
        realPR <- prevR(dat)
        if(browse) {
            print(xtabs(~ tb + travel, data = dat))
            print(realPR)
        }
        ## calculate p value from real data
        nullPrevR <- permTest(dat, nperms=nperms, browse=F)
        ## hist(log(nullPrevR),
        ##      xlab = 'prevalence ratio (trav/notrav)',
        ##      col = 'black', breaks = 100
        ##      )
        pvalV[jj] <- pValFxn(c(nullPrevR,realPR), realPR)
    }
    return(mean(pvalV <= .05))
}

## default parameters
pow <- powCalc(nruns = 100, nperms = 199, browse=F)


pow <- powCalc(nruns = 100, nperms = 199, browse=F, baselineTB = .1)

baselineTBprevs <- seq(0.001, .1, length = 10)
numPows <- length(baselineTBprevs)
powV <- rep(NA, numPows)
for(bb in 1:numPows) {
    powV[bb] <- powCalc(baselineTB = baselineTBprevs[bb],
                        truePrevR = 2.1)
}

par('ps'=25, mar = rep(6,4), lwd = 2)
plot(baselineTBprevs, powV, xlab = 'baseline TB prev',
     type = 'l', lwd = 3, ylim = c(0,1),
     ylab = 'statistical power', bty = 'n')
















