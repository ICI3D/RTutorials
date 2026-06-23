## Redo the whole trial in fluStudy.R as a single function
## So that we can see what would happen if we do the trial many times
library(tidyverse)

numCommunities <- 24
aveElders <- 100
shapeElders <- 5
seedinit <- 431
foiMean <- 0.5
foilsd <- 0.001
protection <- 0.3
numTrials <- 500

set.seed(seedinit)

## A function that just does what we did in the previous script
doTrial <- function(numCommunities, aveElders, shapeElders
                    , foiMean, foilsd, protection
){
  dat <- tibble(
    village = as.character(1:numCommunities)
    , treatment = sample(c(
      rep("Vaccinate", numCommunities/2)
      , rep("Control", numCommunities/2)
    ))
    , numElders = rnbinom(numCommunities, mu=aveElders, size=shapeElders)
    , foi = rlnorm(numCommunities, foiMean, foilsd)
  )
  
  dat <- (dat
          |> mutate(
            foi = ifelse(treatment=="Vaccinate", foi*(1-protection), foi)
            , infected = rbinom(numCommunities, numElders, 1-exp(-foi))
            , propInf = infected/numElders
          )
  )
  
  m <- glm(propInf ~ treatment
           , data=dat, weights=numElders
           , family=binomial(link="cloglog")
  )
  
  ci <- exp(confint(m)["treatmentVaccinate", ])
  return(c(lwr=ci[[1]], upr=ci[[2]]))
}

## Test it
doTrial(numCommunities, aveElders, shapeElders
        , foiMean, foilsd, protection
)

## Now run it many (numTrials) times
trials <- replicate(numTrials, doTrial(numCommunities, aveElders, shapeElders
                                       , foiMean, foilsd, protection
))

## Some annoying R "magic" to make it into a decent data frame
## followed by calculation of:
##  power – is the model confident the ratio < 1
##  coverage – is the true value inside the confidence interval?
trials <- (
  as.data.frame(t(trials))
  |> mutate(
    sig = upr<1
    , cover = (lwr<1-protection) & (upr>1-protection)
  )
)

## Coverage is good, power is 100%. Nice. Our trial is big enough and our test is valid.
print(trials 
      |> summarise(coverage = mean(cover), power=mean(sig))
)

