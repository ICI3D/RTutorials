#####################################################################

## Example: Stochastic SIRS model using the Gillespie algorithm
## Part D: SIRS model with external source of infection

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

## event(): advances the simulation by one Gillespie event, with an optional
## external introduction rate; returns the updated state as a tibble

event <- function(time, S, I, R, params, external=0, final.time=Inf) {
  with(as.list(params), {
    trans.rate <- R0 * S * I / N + external
    recov.rate <- I
    loss.rate  <- rho * R
    tot.rate   <- trans.rate + recov.rate + loss.rate

    if (tot.rate == 0) {
      event.time <- final.time
    } else {
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
    }

    tibble(time=event.time, S=S, I=I, R=R)
  })
}

## run_sirs(): runs one full simulation and returns a tibble labelled with
## a run identifier and the model variant name

run_sirs <- function(run_id, external, params, MAXTIME) {
  ts <- list(tibble(time=0, S=params$N - 1, I=1, R=0))
  current <- ts[[1]]
  i <- 1
  while (current$time < MAXTIME) {
    current <- event(current$time, current$S, current$I, current$R,
                     params, external=external, final.time=MAXTIME)
    i <- i + 1
    ts[[i]] <- current
  }
  bind_rows(ts) |> mutate(run=run_id)
}

params <- list(
  N   = 1000,
  R0  = 4,
  rho = 0.01  # Try increasing this to 0.05 -- what happens?
)
MAXTIME  <- 100
NUMRUNS  <- 5

## Collect runs for both model variants into a single tidy tibble
runs <- bind_rows(
  map_dfr(1:NUMRUNS, run_sirs, external=1/5, params=params, MAXTIME=MAXTIME) |>
    mutate(model="With external source"),
  map_dfr(1:NUMRUNS, run_sirs, external=0,   params=params, MAXTIME=MAXTIME) |>
    mutate(model="Without external source")
)

ggplot(runs, aes(x=time, y=I, group=run, colour=factor(run))) +
  geom_step() +
  facet_wrap(~model, ncol=1) +
  coord_cartesian(ylim=c(0, params$N / 2)) +
  labs(x="Time", y="Number infectious") +
  theme_classic(base_size=14) +
  theme(legend.position="none")
