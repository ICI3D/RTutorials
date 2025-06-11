
## Stochastic SIR simulation with spillover introductions
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
#
## Rebecca Borchering 2015 & 2018
## Reshma Kassanjee 2025
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)

# In this tutorial you will use a Gillespie algorithm 
# implementation of an SIR model to explore the impact of
# introducing spillover infection events. 

# You will be asked to extend code - though we provide example
# solutions, we encourage you take the time to first try. 

library(tidyverse)


## PART 1: SIR 
## --------------------------------------------------------------------

# We begin with a closed population of animals, with infection spreading 
# as per an SIR model. 

# Take some time to understand the implementation below. 

# Compartments:
# (S,I,R) = (susceptible, infectious, removed)

# Transitions:
# Event                           Change        										Rate
# Infection (S)                   (S,I,R)->(S-1,I+1,R)             beta*I*S/N
# Recovery/Removal (I)            (S,I,R)->(S,I-1,R+1)             gamma*I

## Function to step forward in time to next event and update states:

event_sir <- function(time, S, I, R, params, t_end, count.inf) {
  with(as.list(params), {
    
    rates <- c(
      infect = beta * I * S / N,
      recover = gamma * I
    )
    
    total_rate <- sum(rates)
    
    if (total_rate == 0) {
      
      event_time <- t_end
      
    } else {
      
      event_time <- time + rexp(1, total_rate)
      event_type <- sample(c("Infect", "Recover"), 1, prob = rates / total_rate)
      
      switch(event_type,
             "Infect" = {
               S <- S - 1
               I <- I + 1
               count.inf = count.inf+1
             },
             "Recover" = {
               I <- I - 1
               R <- R + 1
             })
    }
    
    return(data.frame(time = event_time, S = S, I = I, R = R, count.inf = count.inf))
  })
}

## Function to simulate states from time 0 to t_end:

simulate_sir <- function(t_end, y, params) {
  with(as.list(y), {
    
    count.inf <-  I
    ts <- data.frame(time = 0, S = S, I = I, R = R, count.inf = I)
    next_event <- ts
    
    while (next_event$time < t_end) {
      next_event <- event_sir(next_event$time, next_event$S, next_event$I, next_event$R, params, t_end, count.inf)
      ts <- rbind(ts, next_event)
    }
    
    return(ts)
  })
}

## Run the model for specified inputs:

N <- 50                                 # population size
params <- c(beta = 0.3, gamma = 0.1)    # parameter values
final_time <- 400                       # end time
y0 <- c(S = N - 1, I = 1, R = 0)        # initial state

ts1 <- simulate_sir(final_time, y0, params)

## And plot:

ts1_long <- ts1 |> pivot_longer(cols = c(S, I, R), names_to = "compartment", values_to = "count")

ggplot(ts1_long, aes(x = time, y = count, color = compartment)) +
  geom_step(linewidth = 1.2) +
  labs(title = "SIR dynamics without spillover", y = "Count", x = "Time") +
  theme_minimal(base_size = 14)

# Generate the data and produce the plot multiple times, by running the relevant
# lines above. What do you see?


## PART 2: SIR with spillover
## --------------------------------------------------------------------

# We now introduce the possibility of infections occurring in the
# population due to other exposures - for example, due to infected animals
# in some other maintenance population briefly entering the territory. 

# TASK: Make a copy of the code presented for part 1 and modify it to 
# introduce spillover events - 
# make functions event_sirspill and simulate_sirspill.
# More specifically, assume that, in addition to the transmission already 
# occurring in the model above, there is an additional rate of infection of 
# susceptibles of lambda*S/N (total rate). Here lambda is the rate at which, 
# for example, animals from outside of the population manage to make contact 
# with animals in our population of interest. 

# When you are ready, scroll down to the bottom of the tutorial to find an example
# solution, and then move onto the next parts of the tutorial using it.

# Here are some hints if needed: 
#   Update the list of '## Transitions'
#   In the function event_sirspill, include a third event type 
#   Also try to include a counter for spillovers (count.spillovers) - 
#     this may prove handy if you get time to explore. 
#   To run the model, think about whether any of the inputs needs to change.


## PART 3: Explore patterns
## --------------------------------------------------------------------

# Let's plot S, I, R over time, for different parameter values
# Let's write a function so that it is easy to make these plots, many times.

plot_sirspill <- function(N, params, final_time, y0){
  
  ts1 <- simulate_sirspill(final_time, y0, params)
  
  ts1_long <- ts1 |> pivot_longer(cols = c(S, I, R), names_to = "compartment", values_to = "count")
  
  gg <- ggplot(ts1_long, aes(x = time, y = count, color = compartment)) +
    geom_step(linewidth = 1.2) +
    labs(title = "SIR dynamics without spillover", y = "Count", x = "Time") +
    coord_cartesian(xlim=c(0, final_time)) +
    theme_minimal(base_size = 14)
  
  print(gg)
  
  return(gg)
  
  
}

# We can now easily now easily plot trajectories - here we plot 20 trajectories for a set of inputs

N <- 50
for (ii in 1:20){
  print(ii)
  readline() 
  plot_sirspill(N, c(beta = 0.3, gamma = 0.1,lambda = 0.01), 400, c(S = N - 1, I = 1, R = 0))
}

# You need to press enter for the next plot to appear (because of readline())
# Once you have them all, you can scroll through the plots use the right and left arrows 
# in the plot tab

# Use the above code to produce output for the following comparisons of parameter values and 
# think about the patterns you see. Also think about how the patterns you see relate 
# to R0 = beta/gamma, and, later in time, Reff = R0*S/N

# Don't forget to erase your graphs every now and then using graphics.off().

# (1) 1 infected animal to begin, with different beta and gamma values
# c(beta = 0.3, gamma = 0.1,lambda = 0.01), 400, c(S = N - 1, I = 1, R = 0))
# versus 
# c(beta = 0.2, gamma = 0.2,lambda = 0.01), 400, c(S = N - 1, I = 1, R = 0))

# (2) as for (1) but with 0 infected animal to begin
# c(beta = 0.3, gamma = 0.1,lambda = 0.01), 400, c(S = N, I = 0, R = 0))
# versus 
# c(beta = 0.2, gamma = 0.2,lambda = 0.01), 400, c(S = N, I = 0, R = 0))

# (3) as for (1) but with a higher spillover rate
# c(beta = 0.3, gamma = 0.1,lambda = 0.04), 400, c(S = N-1, I = 1, R = 0))
# versus 
# c(beta = 0.2, gamma = 0.2,lambda = 0.04), 400, c(S = N-1, I = 1, R = 0))

# Lastly, would we have been able to study these patterns using 
# a deterministic model? If you have time, refer back to previous labs, 
# and try fit and plot a corresponding deterministic model. 









## PART 2: Solution 
## --------------------------------------------------------------------

# Compartments:
# (S,I,R) = (susceptible, infectious, removed)

# Transitions:
# Event                           Change        									 Rate
# Spillover (S)									  (S,I,R)->(S-1,I+1,R)						 lambda*S/N
# Infection (S)                   (S,I,R)->(S-1,I+1,R)             beta*I*S/N
# Recovery/Removal (I)            (S,I,R)->(S,I-1,R+1)             gamma*I

## Function to step forward in time to next event and update states:

event_sirspill <- function(time, S, I, R, params, t_end, count.inf, count.spill) {
  with(as.list(params), {
    
    rates <- c(spillover = lambda*S/N, 
               infect = beta * I * S / N,
               recover = gamma * I
    )
    
    total_rate <- sum(rates)
    
    if (total_rate == 0) {
      
      event_time <- t_end
      
    } else {
      
      event_time <- time + rexp(1, total_rate)
      event_type <- sample(c("Spillover","Infect","Recover"), 1, prob = rates / total_rate)
      
      switch(event_type,
             "Spillover" = {
               S <- S-1
               I <- I+1
               count.spill = count.spill+1
             },
             "Infect" = {
               S <- S-1
               I <- I+1
               count.inf = count.inf+1
             },
             "Recover" = {
               I <- I-1
               R <- R+1
             })
    }
    
    return(data.frame(time = event_time, S = S, I = I, R = R, count.inf = count.inf, count.spill = count.spill))
  })
}

## Function to simulate states from time 0 to t_end:

simulate_sirspill <- function(t_end, y, params) {
  with(as.list(y), {
    
    count.inf <-  I
    count.spill <-  0
    ts <- data.frame(time = 0, S = S, I = I, R = R, count.inf = I, count.spill = 0)
    next_event <- ts
    
    while (next_event$time < t_end) {
      next_event <- event_sirspill(next_event$time, next_event$S, next_event$I, next_event$R, params, t_end, count.inf, count.spill)
      ts <- rbind(ts, next_event)
    }
    
    return(ts)
  })
}

## Run the model for specified inputs:

N <- 50                                 # population size
params <- c(beta = 0.3, gamma = 0.1
            , lambda = 0.02)            # parameter values
final_time <- 400                       # end time
y0 <- c(S = N - 1, I = 1, R = 0)        # initial state

ts1 <- simulate_sirspill(final_time, y0, params)

## And plot:

ts1_long <- ts1 |> pivot_longer(cols = c(S, I, R), names_to = "compartment", values_to = "count")

ggplot(ts1_long, aes(x = time, y = count, color = compartment)) +
  geom_step(linewidth = 1.2) +
  labs(title = "SIR dynamics without spillover", y = "Count", x = "Time") +
  theme_minimal(base_size = 14)

