# Preliminaries -----------------------------------------------------------

## Generally seen as good practice to only use packages you are going to use, especially when scripting (as opposed to just playing)
library(dplyr)
library(tidyr)

## Putting the theme at the top is simpler and clearer unless you really have theme elements changing from fig to fig
library(ggplot2); theme_set(theme_minimal(base_size = 14))

shellpipes::sourceFiles() ## I need this to run; you can comment out.

# Inputs ------------------------------------------------------------------

## Initial state

# S, I, R [people]

## Can we just say N0? in_ feels clunky to me. Alternatively, we can go with in_i (little because it's a proportion, or we could instead define I0 (on in_I))
in_N <- 1e6
i0 <- 1e-3

## I always avoid unnamed numbers, and especially if I'm apparently using the same number twice.
in_x0 <- c(S = in_N*(1-i0), I = i0*in_N, R = 0)

## Model parameters

# beta, gamma [1/day]

in_parms <- list(beta =  0.5 , gamma = 1/7)

print(c(R0 = in_parms$beta/in_parms$gamma))

## Time step and total time

# deltat, maxt [days]

## Why in_? I feel now like I'm missing something.
in_deltat <- 2
in_maxt <- 100

# Run model ---------------------------------------------------------------

## JD: Why do you put it this way? Are we going to walk them through the function? This seems oddly abstract.
# call a user-defined function

out_d <- sir_run(in_parms, in_x0, in_deltat, in_maxt)

# View(out_d)

# Plot outputs ------------------------------------------------------------

## Super-long tabs make things hard to read for people with bad eyes and thus narrow logical screens
## Four spaces is good, although I personally like my tabs to be tabs, and then people can adjust.
out_d_long <- (out_d 
               |> pivot_longer(cols = c(S,I,R), values_to = 'Indivs'
                               , names_to = 'Compartment')
               |> mutate(Compartment = factor(Compartment, levels = c('S','I','R')))
)

## head(out_d_long)

## Not sure why tabs are short here now.
## I changed the aes syntax; this is purely aesthetic and you should absolutely go back if you like it better your way.
## I do like the idea of using explicit rather than implicit print in contexts like this
print(ggplot(data = out_d_long)
  + aes(x = time, y = Indivs, colour = Compartment)
  + geom_line(lwd = 2)
)


