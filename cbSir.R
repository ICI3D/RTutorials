
library(tidyverse)
library(ggplot2)

## SIR chain binomial stochastic versus deterministic discrete steps 

## input parameters

in.parms <- list(beta = 0.5, gamma = 1/20) 
in.parms$beta/in.parms$gamma # R0

## initial state

# S, I, R [people]

in.x0 <- c(S = 980, I = 20, R = 0) 

## time step and time horizon [days]

in.deltat <- 0.5
in.maxt <- 400

## FUNCTIONS for stochastic CHAIN binomial 

# Update state

sir.cb.update <- function(parms,x){

  beta <- parms$beta
  gamma <- parms$gamma

  S <- x[['S']]
  I <- x[['I']]
  R <- x[['R']]
  N <- sum(x) # S + I + R

  p.infected <- 1-exp(-beta*I/N*in.deltat)
  p.recovered <- 1-exp(-1/gamma*in.deltat)

  x.infected <- rbinom(1,S,p.infected)
  x.recovered <- rbinom(1,I,p.recovered)

  x.new <- x + x.infected*c(-1,1,0) + x.recovered*c(0,-1,1)

  return(x.new)
}

# loop through time

sir.cb.loop <- function(parms,x0){

  x <- x0

  S <- x['S']
  I <- x['I']
  R <- x['R']
  N <- sum(x) # S + I + R

  y <- data.frame(time = 0, S = S, I = I, R = R, N = S+I+R)

  # 

  for (currenttime in seq(from = in.deltat, to = in.maxt, by = in.deltat)) {

    x <- sir.cb.update(parms,x)
    y <- rbind(y
               , c(time = currenttime, S = x[['S']], I = x[['I']], R = x[['R']], N= sum(x)))

  }

  return(y)

}

# chain binomial
out.cb.y <- sir.cb.loop(in.parms,in.x0 )

print(out.cb.y)

