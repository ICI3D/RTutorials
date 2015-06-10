library(deSolve)
##      *** Key assumptions ***
## * No HIV yet
## * No vaccination yet
## * No XDR TB yet
## * Total birth rate is constant
## * Beta (transmission) doesn't change with pop. size
## * No fast progressors (S->I)
## * Assuming that attrition means treated people become latent
##   again (later could consider that they go directly back into
##   infectious class)
## * Infectiousness & diseases are the same thing here (I)

rm(list=ls())                   # Clear all variables and functions
## sir() -- Function for numerical analysis of model 1
slitr <- function(t,y,parms){
    with(c(as.list(y),parms),{
        N <- S+L+I+T+R
        dSdt <- -beta*S*I/N + TotalBirthRate -
            natDeathRate*S
        dLdt <- beta*S*I/N - activationRate*L + Rel_Reinf_Ratio*beta*R*I/N +
            attritionRate*T -
                natDeathRate*L 
        dIdt <- activationRate*L - treatmentRate*I -
            natDeathRate*I - TBdeathRate*I
        dTdt <- treatmentRate*I - attritionRate*T - recovRate*T -
            natDeathRate*T
        dRdt <- recovRate*T - Rel_Reinf_Ratio*beta*R*I/N -
            natDeathRate*R
        dNdt <- TotalBirthRate - natDeathRate*N - TBdeathRate*I
        list(c(dSdt, dLdt, dIdt, dTdt, dRdt))
    })
}

time <- 0
N0 <- 10^5
initPop <- c(S = NA,
            L = .01*N0,
            I = 0, 
            T = 0,
            R = 0)
initPop['S'] <- N0 - sum(initPop, na.rm=T)

daysPerYear <- 365.25
param.vals <- c(
    TotalBirthRate = NA, ## people per year
    beta = 1*daysPerYear, ## infectious contact events per unit year
    natDeathRate = .016,  # people/ ( people * year)
    activationRate = 1/7, # people/ ( people * year)
    Rel_Reinf_Ratio = .8, # ratio of beta for reinfection vs first infection
    attritionRate = .3*2,# people/ ( people * 6 months) * (12 months/ yr)
    TBdeathRate = 1/1, # people/ ( people * year)
    treatmentRate = 1/.25, ## people/ ( people * year)
    recovRate = 1/.5 # people/ ( people * year)
    )
param.vals['TotalBirthRate'] <- param.vals['natDeathRate']*N0

time.out <- seq(1800,1990, by = 1/12) ## years

tsTB <- data.frame(lsoda(
  y = initPop,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = slitr,                   # Function to evaluate
  parms = param.vals                # Vector of parameters
  ))
tsTB <- within(tsTB, {
        N <- S+L+I+T+R
})

head(tsTB)

par(bty ='n', 'ps' = 18, lwd = 2, mfrow = c(2,2))
with(tsTB, plot(time, N, xlab = 'year', ylab = 'population size',
                type = 'l',
                ylim = c(0, N0)))
with(tsTB, plot(time, I, xlab = 'year', ylab = 'I',
                type = 'l', col = 'red',
                ylim = c(0, max(tsTB$I))))
with(tsTB, plot(time, (I+L+T)/N, xlab = 'year', ylab = 'P',
                type = 'l', col = 'red',
                ylim = c(0, 1)))
with(tsTB, plot(time, T, xlab = 'year', ylab = 'T',
                type = 'l', col = 'purple',
                ylim = c(0, max(T))))
