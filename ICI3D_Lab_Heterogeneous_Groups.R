#####################################################################

## Lab: Heterogeneity and SIR Dynamics

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: Jonathan Dushoff May 2025
## Last update: Jun 2026

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

## The GOAL of this lab is to help you build intuition for how heterogeneity in contact mixing patterns affects infectious disease dynamics. 

## You will explore TODO:

######################################################################

## TODO: are there question marks?

## NOTE: The comments will guide you through the lab but you
## should make sure you understand what the code is doing.  Some
## code may have question marks and cause errors. 
## You should fill these in; often you will find suggestions in the comments

## Before you start, it is a good idea to clear your workspace.
## In rstudio, do this with Session/Restart
## There is also a hotkey, which you should memorize and use frequently
## ctrl-shift-F10 (or cmd-shift-F10 on Mac)
## In plain R, just quit `q()` and then start again

######################################################################
## Section 1: TODO:
######################################################################

library(deSolve) ## For integrating ODEs

## We have created functions to simulate a well-mixed disease model
## where some individuals have higher contact rates than others

## You can load these functions:
source("ICI3D_Heterogeneous_Groups.R")
## A useful trick to help R find the file is to use
## “Set Working Directory / Source File Location” in rstudio
## The functions file is in the same directory as this script, so that should work

## We do these simulations by making a continuous number of groups
## makeGroups() makes a list of contact rates with a given mean and “kappa”
## kappa represents the squared CV (σ²/μ²) of the mixing rates in the population

## You can see the arguments of any function with args()
## Try args(makeGroups) [ignore that it tells you NULL after]

## Try: 
makeGroups(n=10, m=2, kappa=1)
boxplot(makeGroups(n=10, m=2, kappa=1)
	, main="Activity rate by group"
	, ylab = "Group reproductive number"
)

## Even kappa=1 (standard deviation equals mean) gives big differences between groups

## Try different values for n, m and kappa
## Do you think boxplot is a good way to look at this distribution? can you think oof other ways?
## Estimates for human sexual mixing typically have values of kappa>1

## Our simple simulator function is called groupSim
## The required arguments are cbar (a mean mixing rate), and kappa
## Optional arguments include final time Tfinal, number of groups nGroups and steps (check with args)

## The model is parameterized so that each time unit is one disease generation, and the mixing rate is an _effective_ mixing rate (so that if all groups were the same (kappa=0)  R0 would be equal to cbar).

## How do you think R0 changes when kappa > 0?

## We can do a simple simulation with no heterogeneity:

homo <- groupSim(cbar=2, kappa=0)

## You can use args() to find the default values of parameters.

## Examine the output (use View, or print with an option to see more lines): 
## groupSim returns the total number of susceptible and infected individuals, and also the mean mixing rate in each of these groups. Does the pattern here make sense?

## Now do a simple simulation _with_ heterogeneity:

hetero <- groupSim(cbar=2, kappa=0) ## FIXME what do you need to change to get a heterogeneous population??

## What patterns do you see with View? Is it what you expected?

######################################################################

## We can also try to make pictures
library(ggplot2); theme_set(theme_bw(base_size=14))

base <- (ggplot(homo)
	+ aes(x=time)
	+ geom_line(aes(y=I))
	+ geom_line(aes(y=S), color="blue")
	+ ylab("proportion of pop")
)
print(base)

## What do the blue and black lines show here?

## An advantage of naming our plot is that we can add or replace 
## graphical elements with "+"
## Or even replace data!

print(base + hetero)

## Which of your simulations had a faster increase?
## Which had a higher peak?
## Which had a larger number of total infections? What's a good way to tell?

######################################################################

## Experimenting

## It's now pretty easy to try scenarios with different parameters, e.g.,

print(base + groupSim(cbar=1, kappa=1, nGroups=20, Tfinal=15))

## Do you have questions about this model that you could check by experimenting?

######################################################################

## Benchmarks

## FIND an example where increasing kappa changes a non-epidemic to an epidemic

## Can you find an opposite example??

## FIND an example where increasing kappa increases the total size of an epidemic that is already happening (NOT easy)
## How can you measure total size?

## FIND an example where increasing kappa _reduces_ the total size of an epidemic

## What are reasons why increasing variance might increase epidemic size? What about reducing epidemic size?

######################################################################

## Extra

## Plot how the mean contact rates (cI and cS) change through time in some of your simulations FIXME

## What functions could you write to make your explorations easier or more efficient?
