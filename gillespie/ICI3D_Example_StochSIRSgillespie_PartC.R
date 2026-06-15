#####################################################################

## Example: Stochastic SIRS model using the Gillespie algorithm
## Part C: SIRS model implementation

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: Juliet R.C. Pulliam (2009, 2011, 2012)
## Last update: Lauren Brown (2026)

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

rm(list=ls())

library(tidyverse)

## event(): advances the simulation by one Gillespie event and returns
## the updated state as a tibble
event <- function(time, S, I, R, params) {
  with(as.list(params), {
    trans.rate <- R0 * S * I / N
    recov.rate <- I
    loss.rate  <- rho * R
    tot.rate   <- trans.rate + recov.rate + loss.rate

    event.time <- time + rexp(1, tot.rate)

    dd <- runif(1)
    if (dd < trans.rate / tot.rate) {
      S <- S - 1
      I <- I + 1
    } else if (dd < (trans.rate + recov.rate) / tot.rate) {
      I <- I - 1
      R <- R + 1
    } else {
      R <- R - 1
      S <- S + 1
    }

    tibble(time=event.time, S=S, I=I, R=R)
  })
}

params <- list(
  N   = 1000,
  R0  = 4,
  rho = 0.01
)
MAXTIME <- 150

## Initialise and run the simulation, accumulating rows in a list
## to avoid the cost of growing a data frame in the loop

ts <- list(tibble(time=0, S=params$N - 1, I=1, R=0))
current <- ts[[1]]
i <- 1
while (current$time < MAXTIME && current$I > 0) {
  current <- event(current$time, current$S, current$I, current$R, params)
  i <- i + 1
  ts[[i]] <- current
}
ts <- bind_rows(ts)

ggplot(ts, aes(x=time, y=I)) +
  geom_step() +
  labs(x="Time", y="Number infectious") +
  theme_classic(base_size=14)
