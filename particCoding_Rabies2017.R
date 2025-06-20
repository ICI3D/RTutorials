library(deSolve)                # Load libary to be used for numerical integration
rm(list=ls())                   # Clear all variables and functions

seiv <- function(t,y,parms){
    with(c(as.list(y),parms),{
      ##  browser() ##
        N <-  S + E + I + V
        totalBirthRate <- naturalDeathRate*(S+E+I+V) + rabiesDeathRate*I - totalImportationRate
        dSdt <- totalBirthRate-beta*S*I/N - vaccinationRate*S - naturalDeathRate*S
        dEdt <- beta*S*I/N - progressionRate*E - naturalDeathRate*E
        dIdt <-  progressionRate*E - (rabiesDeathRate + naturalDeathRate)*I +  totalImportationRate
        dVdt <- vaccinationRate*S - naturalDeathRate*V
        list(c(dSdt,dEdt,dIdt,dVdt))
    })
}

time <- 0

N0 <- 1*10^5  ## initial 

pop.SI <- c(S = 0.9*N0,  # Initially 6.5% of the population is susceptible
            E = 0,
            I = 0, 
            V = .1*N0)


values <- c(beta = .5,             # 1/day
##            totalBirthRate = (5*.84)/2/365*N0*1/3,# dogs/day (litter size = 5, litter freq = .84/yr), survival rate of 1/3 for pups
            vaccinationRate = 1/100, # 1/day, equivalent to 100 days susceptible before vacc
            naturalDeathRate = 1/(2.5*365), ## per day, average life expectancy of 2.5 years
            progressionRate = 1/20, ## per day, 20 day latent period
            rabiesDeathRate = 1/3, ## per day, infectious period
            totalImportationRate = 1/30, ## dogs/day
            N0 = N0)            # population size (constant)
R0 <- as.numeric(values["beta"]*(1/values["rabiesDeathRate"]))
print(R0)

#seiv(t=0, y=pop.SI, parms = values)

time.out <- seq(0,5*365, 1)               # in days
myts <- data.frame(lsoda(
    y = pop.SI,               # Initial conditions for population
    times = time.out,             # Timepoints for evaluation
    func = seiv,                   # Function to evaluate
    parms = values                # Vector of parameters
))

head(myts)

doRun <- function(vaccRate) {
    values['vaccinationRate'] <- vaccRate
    myts <- data.frame(lsoda(
        y = pop.SI,               # Initial conditions for population
        times = time.out,             # Timepoints for evaluation
        func = seiv,                   # Function to evaluate
        parms = values                # Vector of parameters
    ))
    myts$N <- myts$S + myts$E + myts$I + myts$V
    return(myts)
}

myts <- doRun(vaccRate = 0)
head(myts)
tail(myts)

par('ps'=15)

plot(myts$time,               # Time on the x axis
     myts$I,                  # Number infected (I) on the y axis
     xlab = "Time in days",     # Label the x axis
     ylab = "Number dogs",  # Label the y axis
     main = "Rabies in Tanzania",    # Plot title
     xlim = c(0,max(time.out)),           # 
     ylim = c(0, max(myts$I, myts$V, myts$S)),
     type = "l",                # Use a line plot
     col =  "red", lwd = 2,
     bty = "n")                 # Remove the box around the plot
lines(myts$time,               # Time on the x axis
      myts$V, col = 'blue', lwd=2)                  # Number infected (I) on the y axis
lines(myts$time,               # Time on the x axis
      myts$S, col = 'green', lwd=2)                  # Number infected (I) on the y axis
legend('topright', leg = c('rabid dogs', 'vaccinated dogs','susceptible'), lwd = 2, lty = 1, 
col = c('red','blue', 'green'))

plot(myts$time,               # Time on the x axis
     myts$N,                  # Number infected (I) on the y axis
     xlab = 'days', ylab = 't otal dogs', lwd = 2, bty = 'n', col = 'black', type = 'l', 
     ylim = c(0, max(myts$N)))

####################################################################################################
## Sensitivity analysis
seqLength <- 100
vaccRatesSeq <- seq(0, 1/180, length.out = seqLength)
1/vaccRatesSeq
finalInfSeq <- rep(NA, seqLength)
for(vv in 1:seqLength) {
    print(paste0('on the ', vv, '-th run'))
    myts <- doRun(vaccRate = vaccRatesSeq[vv])
    finalInf <- tail(myts$I+ myts$E,1)
    finalInfSeq[vv] <- finalInf
}

plot(1/vaccRatesSeq, finalInfSeq, xlab = 'average duration before vaccinated (days)', ylab = 'final number exposed/infected', bty = 'n', lwd = 2, type = 'l', xlim = c(5000,0))
tail(doRun(1800))
