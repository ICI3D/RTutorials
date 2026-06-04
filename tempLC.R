
# Preliminaries -----------------------------------------------------------

library(tidyverse)

# Inputs ------------------------------------------------------------------

## Initial state

# S, I, R [people]

in_N <- 1e6
in_x0 <- c(S = in_N*(1-0.001), I = 0.001*in_N, R = 0)

## Model parameters

# beta, gamma [1/day]

in_parms <- list(beta =  0.5
                 , gamma = 1/7)

in_parms$beta/in_parms$gamma

## Time step and total time

# deltat, maxt [days]

in_deltat <- 2
in_maxt <- 100

# Run model ---------------------------------------------------------------

# call a user-defined function

out_d <- sir_run(in_parms, in_x0, in_deltat, in_maxt)

# View(out_d)

# Plot outputs ------------------------------------------------------------

out_d_long <- (out_d 
               |> pivot_longer(cols = c(S,I,R), values_to = 'count'
                               , names_to = 'Compartment')
               |> mutate(Compartment = factor(Compartment, levels = c('S','I','R')))
)

head(out_d_long)

( ggplot(data = out_d_long, mapping = aes(x = time, y = count, colour = Compartment))
  + geom_line(lwd = 2)
  + theme_minimal(base_size = 14)
)


