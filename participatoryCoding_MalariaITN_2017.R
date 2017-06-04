## MMED 2017
## Steve Bellan
## Participatory Coding for Simulation of Study Design


## Research Question: How does the proportion of people sleeping under ITNs affect the incidence of clinical malaria?

sampleSize <- 10^5
incidenceRate <- .3 # per person per year
studyDuration <- 2 # years
numVillages <- 200

## myDat <- data.frame(id = 1:sampleSize, 
##                     villageID = sample(1:200, size = sampleSize, replace=T)
##                     )
## head(myDat)

## incidence ~ a0*exp(-beta * bednet coverage + noise)
incidenceFXN <- function(a0, betaITN, pITN, sd=10^-6) { ## nonlinear function of incidence rate vs proportion sleeping with ITNs with noise & interecept
    incidence <- a0 * exp(- betaITN * pITN + rnorm(length(pITN), 0, sd))
    return(incidence)
}

## let's look at what this plot looks like with some values
pITNs <- seq(.25, .75, l = 100)
incidenceSeq <- incidenceFXN(a0 = 1, betaITN = .2, pITN = pITNs) ## default small sd so we can just see the trend
plot(pITNs, incidenceSeq, xlab = 'proportion sleeping under bednets',
     ylab = 'incidence (per person per year)', type='l', lwd = 2, 
     ylim = c(0, max(incidenceSeq))
              )
head(myDat)

myDat <- data.frame(villageID = 1:numVillages, ## create some villages
                    propBedNets = runif(numVillages, .25, .75)
                    )
myDat$incidence <- incidenceFXN(a0=1, betaITN = 3, pITN = myDat$propBedNets, sd = .5) 

head(myDat) 
myMod <- lm(log(incidence) ~ propBedNets, data = myDat) ## linear model of logincidence
summary(myMod) ## look at model
IDR <- exp(coefficients(myMod)['propBedNets']) # get incidence density ratio (exponent of linear coefficient)
IDRcis <- exp(confint(myMod))['propBedNets',] # CIs on IDR

## function that does the study many times
doStudy <- function(numRuns = 1, numVillages = 200,
                    minPropBednets = .25,  maxPropBednets = .75, 
                    a0 = 1, betaITN = 3, sd = .5, browse=F) {
    if(browse) browser() ## if argument "browse" is set to true, then we step inside the function
    ## initialize these vectors to store output
    IDRseq <- rep(NA, numRuns)
    IDRlb <-  rep(NA, numRuns)
    IDRub <-  rep(NA, numRuns)
    for(run in 1:numRuns) { ## for each run
        ## create set of villages to sample with randomly distributed ITN coverage
        myDat <- data.frame(villageID = 1:numVillages, 
                            propBedNets = runif(numVillages, minPropBednets, maxPropBednets)
                            )
        myDat$incidence <- incidenceFXN(a0=a0, betaITN = betaITN, pITN = myDat$propBedNets, sd = sd)  ## calculate incidence with some noise as our data
        myMod <- lm(log(incidence) ~ propBedNets, data = myDat) ## fit model to the data
        IDRseq[run] <- exp(coefficients(myMod)['propBedNets'])  ## store values
        IDRcis <- exp(confint(myMod))['propBedNets',]
        IDRlb[run] <- IDRcis[1]
        IDRub[run] <- IDRcis[2]
    }
    power <- mean(IDRub<1) ## power is the proportion of times the upper bound of our CI is below 1 for the IDR
    return(power)
    ##    return(cbind(IDRseq, IDRlb, IDRub)) ## could also return the raw values as a matrix
}

villageNumSeq <- seq(10, 100, by =10) ## for different numbers of villages
powerSeq <- rep(NA, length(villageNumSeq)) ## calculate power
for(vv in 1:length(villageNumSeq)) { ## for each village number sample size
    currentVillageNumber <- villageNumSeq[vv] ## set the current village number
    powerSeq[vv] <- doStudy(1000, numVillages = currentVillageNumber, sd = .5, betaITN = .2) ## calculate power by doing a lot of runs
}

## Plot power vs village sample size
plot(villageNumSeq, powerSeq, xlab= 'number of villages', ylab = 'power', type = 'l', lwd = 2,
ylim = c(0,1))
lines(villageNumSeq, powerSeq, col = 'blue')

