
## JD: 
## sir_ode is beautifully clean and clear
## but I later decided I don't love the name

## ODES

sir_ode <- function(parms, x){
  
  S <- x[['S']]
  I <- x[['I']]
  R <- x[['R']]
  
  N <- S + I + R
  
  beta <- parms$beta 
  gamma <- parms$gamma 
  
  infections <- beta*S*I/N  
  recoveries <- gamma*I
  
  dSdt <- -infections
  dIdt <- +infections -recoveries
  dRdt <- +recoveries  
  
  ## Note that these names are lost; you need to check the order of input and output values here
  dXdt <- c(dSdt, dIdt, dRdt)
  
  return(dXdt)
  
}


# in_N <- 100
# sir_ode(list(beta =  0.5, gamma = 1/7)
#          , c(S = in_N, I = 100, R = 0))



## step through time

sir_run <- function(parms, x0, deltat, maxt){
  
  ## JD: I don't understand browser, but this seemed to break the code (did not run)
  ## browser()
  
  x <- x0 
  y <- tibble(time = 0, S = x[['S']], I = x[['I']], R  = x[['R']])
  
  for (currenttime in seq(deltat, maxt, deltat)) {
    
    x <- x + sir_ode(parms, x)*deltat
    y <- bind_rows(y
                   , c(time = currenttime, S = x[['S']], I = x[['I']], R  = x[['R']]))
    
  }
  
  return(y)
  
}

# sir_run(list(beta =  0.5, gamma = 1/7)
#           , c(S = in_N-10, I = 10, R = 0)
#         , 0.5, 10)

# # first step
# 
# x <- x + sir_ode(parms, x)*deltat
# y <- bind_rows(y
#               , c(time = 0+deltat, S = x['S'], I = x['I'], R  = x['R']))
# 
# # second loop
# 
# x <- x + sir_ode(parms, x)*deltat
# y <- bind_rows(y
#                , c(time = 0+deltat*2, S = x['S'], I = x['I'], R  = x['R']))
# 
# 
