
rm(list=ls())                   # Clear all variables and functions
## sir() -- Function for numerical analysis of model 1
sir <- function(t,y,parms){
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(c(as.list(y),parms),{
    
    dSdt <- -beta*S*I/N + TotalBirthRate -
        natDeathRate*S
    dLdt <- beta*S*I/N - activationRate*L + reinfectionRate*R -
        natDeathRate*L 
    dIdt <- activationRate*L - treatmentRate*I -
            (natDeathRate+TBdeathRate)*I
    dTdt <- treatmentRate*I - recovRate*T
    dRdt <- recovRate*T - reinfectionRate*R
        
    # Note: Population size is constant, so don't need to specify dRdt
    list(c(dSdt,dIdt, dTdt, dRdt))
  })
}

time <- 0
N0 <- 7781984
pop.SI <- c(S = NA,
            L = 0,
            I = .01*N0, ## initial infected
            T = 0,
            R = 0)
pop.SI$S <- 1-sum(pop.SI, na.rm=T)
      

values <- c(beta = 3.6,        # Transmission coefficient
            gamma = 1/5,       # 1 / infectious period = 1/5 days
            N = N0)            # population size (constant)
time.out <- seq(0,365,???)     # INCLUDE THE APPROPRIATE VALUE TO GET A SEQUENCE

ts.sir <- data.frame(lsoda(
  y = pop.SI,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = sir,                   # Function to evaluate
  parms = values                # Vector of parameters
  ))
