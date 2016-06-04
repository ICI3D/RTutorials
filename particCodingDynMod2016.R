## How does increased susceptiblility to TB
## amongst diabetics affect incidence of active
## TB?
require(deSolve)

## Big population
 
## SLAT
slatDB <- function(t,y,parms, browse=F){
    with(c(as.list(y),parms),{
        if(browse) browser()
        N <- S+L+A+T+SDB+LDB+ADB+TDB
        TotBirthRate <- birthRate*N + tbDeathRate*A + tbDeathRate*ADB

        foi <- transmissionCoeff*(A+ADB)/N ## force of infection
        
        ## non-diabetics
        dSdt <- -foi*S + clearanceRate*T + TotBirthRate*(1-diabPrev) - 
            naturalDeathRate*S
        dLdt <- foi*S - activationRate*L  -
            naturalDeathRate*L
        dAdt <- activationRate*L - treatmentRate*A - tbDeathRate*A  -
            naturalDeathRate*A
        dTdt <- treatmentRate*A - clearanceRate*T - naturalDeathRate*T
        dSdt+dLdt+dAdt+dTdt

        ## diabetics
        dSdtDB <- -foi*SDB + clearanceRate*TDB + TotBirthRate*diabPrev - 
            naturalDeathRate*SDB
        dLdtDB <- foi*SDB - activationRate*idrActivationDB*LDB  -
            naturalDeathRate*LDB
        dAdtDB <- activationRate*idrActivationDB*LDB - treatmentRate*ADB -
            tbDeathRate*ADB  - naturalDeathRate*ADB
        dTdtDB <- treatmentRate*ADB - clearanceRate*TDB - naturalDeathRate*TDB
        ## dSdt+dLdt+dAdt+dTdt+dSdtDB+dLdtDB+dAdtDB+dTdtDB

        list(c(dSdt,dLdt,dAdt,dTdt,dSdtDB,dLdtDB,dAdtDB,dTdtDB))
    })
}

params <- c(
    transmissionCoeff = 10 ## [per year]
    , activationRate = 1/2 
    , treatmentRate = 1/0.5
    , clearanceRate = 1/0.5
    , tbDeathRate = 1/3
    , birthRate = 1/60
    , naturalDeathRate = 1/60
    , idrActivationDB = 3 ## [unitless]
    , diabPrev = 0.12 ## proportion
    , N0 = 52*10^6) ## [people]
params
## activationRate*idrActivationDB

time <- 0
initCond <- params['N0'] * c(
    S = 2/3
    , L = 1/3
    , A = 0 
    , T = 0)
initCondDB <- c(initCond*(1-params['diabPrev']), initCond*params['diabPrev'])
names(initCondDB)[5:8] <- paste0(names(initCondDB)[5:8], 'DB')

slatDB(t=time,y=initCondDB,parms=params, browse=F)
args(slatDB)
timeseq <- seq(1950, 1970, by = .02)

params['transmissionCoeff'] <- 7
simdat <- data.frame(lsoda(
    y = initCondDB,               # Initial conditions for population
    times = timeseq,             # Timepoints for evaluation
    func = slatDB,                   # Function to evaluate
    parms = params# Vector of parameters
    ))

simdat <- within(simdat, {        N <- S+L+A+T+SDB+LDB+ADB+TDB})
simdat <- within(simdat, {        PA <- (A+ADB)/N})
head(simdat); tail(simdat)

par('ps'=16, lwd = 5, mfrow=c(1,2))
## non-diab
    plot(0,0, type = 'n', xlim = range(timeseq), ylim = c(0, params['N0']),
         xlab = 'time', ylab = 'people', bty='n')
    with(simdat, lines(time, S, col ='black'))
    with(simdat, lines(time, L, col ='orange'))
    with(simdat, lines(time, A, col ='red'))
    with(simdat, lines(time, T, col ='blue'))
    legend('topright', leg = c('S','L','A','T'), col = c('black','orange','red','blue'), lwd = 5, cex = 1.5)
## diab
    plot(0,0, type = 'n', xlim = range(timeseq), ylim = c(0, params['N0']),
         xlab = 'time', ylab = 'people', bty='n')
    with(simdat, lines(time, SDB, col ='black'))
    with(simdat, lines(time, LDB, col ='orange'))
    with(simdat, lines(time, ADB, col ='red'))
    with(simdat, lines(time, TDB, col ='blue'))
    legend('topright', leg = c('SDB','LDB','ADB','TDB'), col = c('black','orange','red','blue'), lwd = 5, cex = 1.5)


par('ps'=16, lwd = 5, mfrow=c(1,2))
## non-diab
with(simdat, plot(time,PA, type='l'))

## compare PA for differennt levels of diabetes prevalence
diabPrevVec <- seq(0, .4, by = .05)
eqPA <- rep(NA, length(diabPrevVec))
for(jj in 1:length(diabPrevVec)) {
    params['transmissionCoeff'] <- 7
    params['diabPrev'] <- diabPrevVec[jj]
    simdat <- data.frame(lsoda(
        y = initCondDB,               # Initial conditions for population
        times = timeseq,             # Timepoints for evaluation
        func = slatDB,                   # Function to evaluate
        parms = params# Vector of parameters
        ))
    simdat <- within(simdat, {        N <- S+L+A+T+SDB+LDB+ADB+TDB})
    simdat <- within(simdat, {        PA <- (A+ADB)/N})
    eqPA[jj] <- tail(simdat$PA,1)
}

par(cex=1.5, lwd = 3, bty = 'n')
plot(diabPrevVec, eqPA, type='l', xlab = 'diabetes prevalence',
     ylab = 'equilibrium prevalence of active TB',
     ylim = c(0, max(eqPA)))

