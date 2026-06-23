## Irrecoverable exception!! 2024 Jun 26 (Wed)

## What happens if we add more realistic variation?
library(tidyverse)
library(shellpipes)

loadEnvironments()

numCommunities <- 24
aveElders <- 100
shapeElders <- 2
seedinit <- 431
foiMean <- 0.5
## foilsd <- 0.001
protection <- 0.3
numTrials <- 20
numTrials <- 200
plen <- 11

## Add a more realistic variation between communities
foilsd <- 0.1

set.seed(seedinit)

prot <- seq(0, protection, length.out=plen)
effTest <- sapply(prot, function(p){
  return(doExp(numTrials, numCommunities, aveElders, shapeElders
               , foiMean, foilsd, protection=p
  ))
})

eff <- (as.tibble(t(as.matrix(effTest)))
        |> mutate(protection=prot)
        |> gather(metric, proportion, coverage:power)
        |> mutate(protection=as.numeric(protection))
)

## Now the coverage is around 85%. This means our significance test is not doing what we think!
print(summary(eff))

print(
  ggplot(eff, aes(protection, proportion, color=metric))
  + geom_line()
)

## Can it be fixed?
## FRY version below; redo everything but change the model statement

library(lme4)

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
  
  ## Here's where I add a random effect for village
  m <- glmer(propInf ~ treatment + (1|village)
             , data=dat, weights=numElders
             , family=binomial(link="cloglog")
  )
  
  ## We have occasional crashes with profile CIs, so we use Wald to automate
  ci <- exp(confint(m, method="Wald")["treatmentVaccinate", ])
  return(c(lwr=ci[[1]], upr=ci[[2]]))
}

prot <- seq(0, protection, length.out=plen)
effTest <- sapply(prot, function(p){
  return(doExp(numTrials, numCommunities, aveElders, shapeElders
               , foiMean, foilsd, protection=p
  ))
})

eff <- (as.tibble(t(as.matrix(effTest)))
        |> mutate(protection=prot)
        |> gather(metric, proportion, coverage:power)
        |> mutate(protection=as.numeric(protection))
)

print(summary(eff))

print(
  ggplot(eff, aes(protection, proportion, color=metric))
  + geom_line()
)


## Using mixed models fixes the coverage. Cool!
