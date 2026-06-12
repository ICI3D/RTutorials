#####################################################################

## Example: Stochastic SIR model using the Chain Binomial Algorithm

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: Juliet R.C. Pulliam (2012-2015)
## Last update: Lauren Brown (2026)

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

## Clear all variables and functions

#####################################################################

## Section 1: Chain Binomial SIR model

#####################################################################

rm(list=ls())

library(tidyverse)

## sir.cb(): Function that runs a chain binomial SIR model for a given population
## size and R0 value, outputting a tibble with columns representing time,
## the number of susceptibles at each time, and the number of cases at each time.
## The chain binomial is a stochastic model, so the output varies between function
## calls, even for the same parameter values and initial conditions.

sir.cb <- function(R0, N, MAXTIME, I0=1, S0=N-I0){
  qq <- 1 - (R0/N)  # Pairwise probability of avoiding potentially infectious contact

  Cases <- I0
  Sus <- S0

  for(Time in 1:MAXTIME){
    Cases <- c(Cases, rbinom(1, Sus[Time], (1 - qq^Cases[Time])))
    Sus <- c(Sus, Sus[Time] - Cases[Time + 1])
  }
  return(data.frame(Time=0:MAXTIME, Cases, Sus))
}

## Run the function once, for an R0 of 1.5, a population size of 1000, and 60 time
## steps
epi <- sir.cb(1.5, 1000, 60)

## Examine the resulting data frame
head(epi)

## Plot the number of cases through time and label the plot appropriately;
## geom_step() is used because time is treated as discrete
ggplot(epi, aes(x=Time, y=Cases)) +
  geom_step(linewidth=1.2) +
  annotate("text", x=40, y=80, label="R[0] == 1.5", parse=TRUE, size=6) +
  coord_cartesian(ylim=c(0, 100)) +
  labs(x="Time", y="Infected") +
  theme_classic(base_size=14)

## run.cb(): runs sir.cb() for specified parameter values and returns the cases
## through time as a tibble, labelled with a run identifier
run.cb <- function(run_id, R0, N, MAXTIME=60){
  sir.cb(R0, N, MAXTIME) |>
    select(Time, Cases) |>
    mutate(run=run_id)
}

## Use map_dfr() to collect 3 runs into a single tidy tibble; each row is one
## time point from one run
runs <- map_dfr(1:3, run.cb, R0=1.5, N=1000, MAXTIME=60)

## Examine the first few rows: Time, Cases, and run columns
head(runs)

## Plot all 3 runs as step lines
ggplot(runs, aes(x=Time, y=Cases, group=run)) +
  geom_step(colour="grey") +
  annotate("text", x=40, y=80, label="R[0] == 1.5", parse=TRUE, size=6) +
  coord_cartesian(ylim=c(0, 100)) +
  labs(x="Time", y="Infected") +
  theme_classic(base_size=14)

## Calculate the average number of cases in each timestep:
ave.case.t <- runs |>
  group_by(Time) |>
  summarise(mean_cases=mean(Cases))

######################################################################

## Section 2: Try it yourself

######################################################################

## Use map_dfr() to collect 300 runs into a single data frame:


## Calculate the mean and median number of cases in each timestep:


## Plot all 300 runs as grey step lines. Add a thick red line for the mean
## and a thick blue line for the median number of cases through time.
## Hint: you can supply a different data source to a geom using the `data`
## argument, e.g. geom_step(data=summaries, aes(...), colour="red"):


## Save your plot as a PDF using ggsave():
