## Gillespie algorithm benchmark question
## Meaningful Modeling of Epidemiological Data, 2012
## AIMS, Muizenberg
## Juliet Pulliam, 2009,2012

## This question leads you through the construction of a stochastic
## SIRS model using the Gillespie algorithm.
##
## A key is available for part (c).

## ODE models of infectious disease dynamics treat individuals as
## if they were continuous entities, which of course is not the
## case. The Gillespie algorithm is a method that produces a discrete
## analogue of an ODE system and incorporates demographic
## stochasticity (randomness that results from the discrete nature of
## individuals). The following model represents an infection that
## produces transient immunity in its host:

## dS/dt = ro*R - R_0*S*I/N

## dI/dt = R_0*S*I/N - I

## dR/dt = I - ro*R

#################### 
## 	a. What is the biological meaning of each of the parameters?

#################### 
## 	b. Fill in the following table:

## EVENT                        RATE            TIME TO EVENT
## (S,I,R)->(S-1,I+1,R)         lambda_1 = ?    tau_1 = ?
## (S,I,R)->(S,I-1,R+1)         lambda_2 = ?    tau_2 = ?
## (S,I,R)->(S+1,I,R-1)         lambda_3 = ?    tau_3 = ?

	
## The algorithm is implemented as follows:

## Step 1: Given your parameters and the current state of all
## variables (t, S(t), I(t), R(t)), calculate the rate at which events
## will occur:

##      lambda_event = lambda_1 + lambda_2 + lambda_3

## Step 2: Select a random value from the exponential distribution
## with rate parameter lambda_event to determine the time to the next
## event, tau_event.

## Step 3: Determine which event occurs at time t+tau_event , given
## the probability that the event was event i is equal to
## lambda_i/lambda_event.

## Step 4: Update the values of all variables (t, S(t), I(t), R(t))
## and repeat steps 1-4.

#################### 
## c. Write an R script to implement the Gillespie algorithm for the
## SIRS model described above, with the initial conditions

## S(t=0) = N-1
## I(t=0) = 1

## Include a time limit in your algorithm, telling it to stop when
## time reaches a maximum of t = 150 disease generations. Also include
## a plotting function in your script, and run the algorithm multiple
## times for N=10,000 R0=4 and Ï=0.01, until you have an idea of the
## different patterns it produces. Save plots of the number of
## infections through time for 3 realizations of your algorithm.

#################### 
## d. Modify the SIRS model above to include reintroduction of the
## infection from some external source, once every 5 disease
## generations (on average). Fill in the table for this new version of
## the model:

## EVENT                        RATE            TIME TO EVENT
## (S,I,R)->(S-1,I+1,R)         lambda_1 = ?    tau_1 = ?
## (S,I,R)->(S,I-1,R+1)         lambda_2 = ?    tau_2 = ?
## (S,I,R)->(S+1,I,R-1)         lambda_3 = ?    tau_3 = ?

## Copy your R script to a new file and modify your algorithm for the
## new model.  How does an external source of infection affect the
## dynamics observed? Create plots that illustrate the differences
## between this model and the previous one.

