## Introduction to Infectious Disease Dynamics
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
##
## Juliet R.C. Pulliam, 2012-2018
##
##
## The goal of this tutorial is to acquaint you with ways of
## analysing simple infectious disease models in R. By the end of
## this tutorial, you should be able to:
##
##  * create a function describing a system of ODE's
##  * use the package deSolve to numerically analyze a system of ODEs
##  * plot the output of different types of functions
##
## NOTE: The comments will guide you through the tutorial but you
## should make sure you understand what the code is doing.  Some
## function arguments are assigned to ?.  This will give you an
## error. You should try out values for these arguments as
## suggested in the comments or find them yourselves in a help file.

######################################################################
## Section 1: Creating a function to describe the behavior of an ODE
######################################################################

## Before you start, it is a good idea to clear the workspace of things
## you have defined previously. Do this now:

rm(list=ls())                   # Clear all variables and functions

## We will now define a function to describe a simple SIR model with
## constant population size and no population turnover (that is, no
## births and deaths). This model was discussed in "Introduction to dynamic
## modeling of infectious diseases," and slides are available here:
## https://ndownloader.figshare.com/files/8541817
##
## The version of the model we'll work with here has 3 state variables (though
## only two of these need to be specified to define the state of the system,
## since the population size is constant):
##
## S - the number of susceptibles in the population
## I - the number of infected/infectious individuals in the population
## R - the number of recovered/immune individuals in the population
##
## and 3 parameters:
##
## N - the total population size; N = S + I + R
## beta - the transmission coefficient; beta = the contact rate * infectivity
## gamma - 1 / the infectious period
##
## If you do not remember the model, review the lecture notes before you proceed
## with this tutorial. If you're not entirely comfortable with the distinction
## between state variables and parameters, you may also want to review the
## DAIDD Glossary:
## https://github.com/ICI3D/MMEDparticipants/raw/master/Resources/DAIDD2016_Glossary.pdf

## We will now define a function that describes the change in the state
## variables with time. The function takes the input t (the current time), y (a
## vector giving the current value of the state variables - that is, their
## value at time t), and params (a vector that defines the parameter values);
## the function outputs the values of dS/dt and dI/dt at time t:
##
##-- Model 1: SIR model --##
##
## sir() -- Function for numerical analysis of model 1
sir <- function(t,y,parms){
  # The with() function gives access to the named values of parms within the
  # local environment created by the function
  with(c(as.list(y),parms),{
    dSdt <- -beta*S*I/N
    dIdt <- beta*S*I/N - gamma*I
    # Note: Population size is constant, so don't need to specify dRdt
    return(list(c(dSdt,dIdt)))
  })
}

######################################################################
## Section 2: Understanding the function that defines our ODE model
######################################################################

## We have defined the function, but we now need to make sure we
## understand how it works. To do this, let's think about how we would
## approximate the model using discrete timesteps, similar to the way we
## used spreadsheets to analyze epidemic models on Monday.
##
## Since the function takes t, y, and parms as inputs, we first need to define
## these. Let us start at time zero:

time <- 0

## We will use parameter values that reflect a measles outbreak that occurred
## in New York City (many years ago!), so let our initial population size (N0) be

N0 <- 7780000

## The second argument of the sir() function defined above is y, which I have
## said is a vector giving the current value of the state variables. Let's
## assume that the initial population has 20.5 infecteds and that 6.5% of the
## population is susceptible. (Remember that this model assumes a large
## population size and represents population averages, so it is ok that these
## values are not integers!) We will define a vector pop.SI that gives the
## initial values of our 2 state variables:

pop.SI <- c(S = 0.065*N0,  # Initially 6.5% of the population is susceptible
                I = ???)       # ENTER THE NUMBER INITIALLY INFECTED (20.5) ## FIXME

## Notice that I've named the two values in the initial population vector to
## help us keep track of which value is which. This also allows us to take
## advantage of the with() function called within the sir() function because I
## have used the same names that are used to refer to the state variables
## within the function.
##
## The final input our function needs is a vector of parameter values, which we
## can create in the same way:

values <- c(beta = 3.6,        # Transmission coefficient
            gamma = 1/5,       # 1 / infectious period = 1/5 days
            N = N0)            # population size (constant)

## Note that we can calculate the value of the basic reproduction number,
## R0 = beta / gamma, as follows:

values["beta"]/values["gamma"] # value of R0
                               # HINT: Try putting this command inside the
                               # function as.numeric() to keep R from
                               # confusingly retaining the name of the first
                               # value.

## Now that we have defined the inputs, we are ready to try out our function!

sir(t=time,y=pop.SI,parms=values)

## Look at the output. What does this mean?
##
## Recall that I said the function outputs the time derivatives of S and I at
## the time t. This means that in order to get the (approximate) values of S
## and I at some time delta.t in the future, I need to calculate the equivalent
## of
##
## S(t+delta.t) = S(t) - (beta*S(t)*I(t)/N) * delta.t
##
## I(t+delta.t) = I(t) + (beta*S(t)*I(t)/N - gamma*I(t)) * delta.t
##
## This can be done as follows:

delta.t <- 0.1                    # Set a small value for delta.t (0.1 day)

pop.next <- pop.SI + unlist(sir(t=time,y=pop.SI,parms=values)) * delta.t

## We could then iterate this process, updating our values of pop.SI and time
## with each iteration to get a discrete-time approximation of the values of the
## state variables through time. Remember, however, that the differential
## equations describe the rates of change in the limit as delta.t goes to zero.
## It turns out that using the above discrete time approximation of this
## process, as we have done on the spreadsheets (Track B) and live coding
## example, leads to rapid accumulation of error in our estimate of the state
## variables through time, even if we set our value of delta.t to be very
## small. Luckily for us, there are a number of algorithms that have been
## developed to come up with better (both more accurate and more
## computationally efficient) approximations to the values of the state
## variables through time.

######################################################################
## Section 3: Numerical integration of our ODE model
######################################################################

## In R, we can access these algorithms through the deSolve library. We should
## now load this library so we can access its functions:

library(deSolve)                # Load libary to be used for numerical integration

## The function within deSolve that we will be using is called lsoda(). Let's
## look at the help file for this function:

## Things like help and View statements, which are for looking at now, may be better typed directly into the console below.
## ?lsoda ## CONSOLE

## This help file gives some detail on the history of the algorithm as well as
## describing the function's usage. For now, take a look at the "Usage" section
## to look at the different types of arguments the function can take. There are
## a lot of them! For now, we will focus on the first 4 inputs: y, times, func,
## and parms. Read through the "Arguments" section for the descriptions of these
## four inputs. Because we have already defined our function and seen how it
## works, these arguments should seem familiar.

## We have already defined the vectors that we will use for two of the arguments
## (y, the vector of the initial values of the state variables, and parms, the
## vector of parameter values). We have also defined a third argumet, func,
## which provides the model definition; it is our function sir(). All that
## remains is to define the vector of times, which tells lsoda() the timepoints
## for which we would like to know the values of S and I. (NOTE: This is
## different from specifying the time points at which to evaluate our function,
## which is determined within the lsoda() function and optimized to efficiently
## provide an accurate approximation.)
##
## Let's ask for output every 0.1 days for 365 days, starting at time 0:

# INCLUDE APPROPRIATE CODE TO GET A SEQUENCE FROM 0 to 365 BY STEPS OF 0.1
time.out <- seq(0,365,???)  ## FIXME  

## Now let's see what happens if we plug our inputs into lsoda()...

lsoda(
  y = pop.SI,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = sir,                   # Function to evaluate
  parms = values                # Vector of parameters
  )

## Well, that seems to have done something, but it's hard to understand the
## output. Let's make it easier to look at by saving the output as a data.frame
## named ts.sir ("ts" being short for timeseries).

ts.sir <- data.frame(lsoda(
  y = pop.SI,               # Initial conditions for population
  times = time.out,             # Timepoints for evaluation
  func = sir,                   # Function to evaluate
  parms = values                # Vector of parameters
  ))

## Now let's look at the output:

head(ts.sir)

## Our data frame has 3 columns, conveniently labeled "time", "S", and "I",
## showing the values of time and the two state variables through time. We can
## now look at the output in other ways. For example, we saw yesterday that we
## expect the infection to go extinct because there is no source of new
## susceptibles in our equations (eg, through births). Let's see what has
## happened after a year:

subset(ts.sir,time==365)

## There are more infecteds after a year than there were at time 0, so it looks
## like it takes more than a year for the infection to burn out with these
## initial conditions. If we want to see how the epidemic has progressed
## through time, we can plot to output from lsoda(). Remember that the
## parameters we have used are the same as in the live coding example, so
## this is a model of a measles epidemic in New York. We'll label the plot
## accordingly. (You'll learn a lot more about plotting - and modifying the
## appearance of plots - in Tutorial 4.)

plot(ts.sir$time,               # Time on the x axis
     ts.sir$I,                  # Number infected (I) on the y axis
     xlab = "Time in days",     # Label the x axis
     ylab = "Number infected",  # Label the y axis
     main = "Measles in New York",    # Plot title
     xlim = c(0,400),           #
     type = "l",                # Use a line plot
     bty = "n")                 # Remove the box around the plot

## While there are many more infecteds at time 365 than there were at time 0,
## there are a lot fewer than there were at time 200, and it looks like the
## epidemic is nearly done.

######################################################################
## BENCHMARK QUESTIONS
######################################################################

## Question 1:
##
## Run the SIR model above out to 5 years to convince yourself that the epidemic
## is ending and will not come back.
##
## Question 2:
##
## Create a new function called sir.bd() that describes the SIR model with
## births and deaths (but constant population size), and compare the output
## to the model defined above. You may want to refer to yesterday's lecture
## notes for the model definition. Assume that the life expectancy in the
## population is 60 years.
##
## Question 3:
##
## Use lsoda() to evaluate the values of S and I through time for your new
## function defining the model with births and deaths over 5 years, with the
## same initial conditions as before. How do the dynamics compare?
##
## Question 4:
##
## Now change the value of the life expectancy for the population in the model.
## Compare the long-term dynamics of the model with different values for the
## life expectancy.
