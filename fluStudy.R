library(tidyverse)
library(ggplot2)

## Does vaccinating schoolchildren protect elders from influenza

## First, do a trial
numCommunities <- 24

## We assume a random number of elders in each community 
## Using a negative binomial with a shape parameter
## Shape is 1/CV^2 of "extra" (beyond Poisson) variance
aveElders <- 100
shapeElders <- 5
foiMean <- 0.5
foilsd <- 0.1
protection <- 0.3

set.seed(431)

## treat half of the communities (and make sure numbers are whole numbers)
numVacc <- floor(numCommunities/2)
numControl <- numCommunities - numVacc

## Name the communities, randomize them, 
## pick a random number of elders and a random force of infection
## We're going to do a hazard-based model;
## that means we describe risk as total Force of Infection (foi)
dat <- tibble(
  village = as.character(1:numCommunities)
  , treatment = sample(c(
    rep("Vaccinate", numVacc)
    , rep("Control", numControl)
  ))
  , numElders = rnbinom(numCommunities, mu=aveElders, size=shapeElders)
  , foi = rlnorm(numCommunities, foiMean, foilsd)
)

summary(dat)

## Reduce foi for vaccinated communities
## Randomly infect people and calculate the proportion infected
## The probability of _surviving_ infection is 1 - exp(-foi)
dat <- (dat
        |> mutate(
          foi = ifelse(treatment=="Vaccinate", foi*(1-protection), foi)
          , infected = rbinom(numCommunities, numElders, 1-exp(-foi))
          , propInf = infected/numElders
        )
)

## Now examine the table visually in rstudio by clicking on the right

## Check the distribution of populations 
print(dat 
      |> summarise(
        meanSize=mean(numElders)
        , sdSize = sd(numElders)
      )
)

## Visualize the results
print(
  ggplot(dat, aes(treatment, propInf))
  + geom_boxplot()
)

## Make a model and fit it
m <- glm(propInf ~ treatment
         , data=dat, weights=numElders
         , family=binomial(link="cloglog")
)

##

## Look at the overall model summary
summary(m)

## Look at CIs for the estimated hazard ratio. 
## The model works on the unconstrained log-hazard scale, so we exp()
## These CIs should contain the true relative hazard (95% of the time)
## True relative hazard is 1-protection
## You can increase numCommunities and run the script again; the relative hazard should converge toward the true value
print(exp(confint(m)["treatmentVaccinate", ]))

