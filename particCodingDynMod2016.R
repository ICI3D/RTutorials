## How does increased susceptiblility to TB
## amongst diabetics affect incidence of active
## TB?
require(deSolve)

## Big population
 
## SLAT
slatDB <- function(t,y,parms, browse=F){
  with(c(as.list(y),parms),{
      if(browse) browser()
      N <- S+L+A+T
      TotBirthRate <- birthRate*N + tbDeathRate*A
    dSdt <- -transmissionCoeff*S*A/N + clearanceRate*T + TotBirthRate - 
        naturalDeathRate*S
    dLdt <- transmissionCoeff*S*A/N - activationRate*L  -
        naturalDeathRate*L
    dAdt <- activationRate*L - treatmentRate*A - tbDeathRate*A  -
        naturalDeathRate*A
    dTdt <- treatmentRate*A - clearanceRate*T - naturalDeathRate*T
      dSdt+dLdt+dAdt+dTdt
    list(c(dSdt,dLdt, dAdt, dTdt))
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
    , N0 = 52*10^6) ## [people]
params
## activationRate*idrActivationDB

time <- 0
initCond <- params['N0'] * c(
    S = 2/3
    , L = 1/3
    , A = 0 
    , T = 0)

slatDB(t=time,y=initCond,parms=params)
args(slatDB)
timeseq <- seq(1950, 1970, by = .02)

params['transmissionCoeff'] <- 7
simdat <- data.frame(lsoda(
    y = initCond,               # Initial conditions for population
    times = timeseq,             # Timepoints for evaluation
    func = slatDB,                   # Function to evaluate
    parms = params# Vector of parameters
    ))

simdat <- within(simdat, {N <- S +L+A+T})
head(simdat); tail(simdat)

par('ps'=18, lwd = 5)
plot(0,0, type = 'n', xlim = range(timeseq), ylim = c(0, params['N0']),
     xlab = 'time', ylab = 'people', bty='n')
with(simdat, lines(time, S, col ='black'))
with(simdat, lines(time, L, col ='orange'))
with(simdat, lines(time, A, col ='red'))
with(simdat, lines(time, T, col ='blue'))
legend('topright', leg = c('S','L','A','T'), col = c('black','orange','red','blue'), lwd = 5, cex = 5)
