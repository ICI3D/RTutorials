library(tidyverse); theme_set(theme_bw())
library(shellpipes)
startGraphics()

numCommunities <- 24
aveElders <- 100
shapeElders <- 5
seedinit <- 431
foiMean <- 0.5
foilsd <- 0.001
protection <- 0.3
numTrials <- 20
numTrials <- 200
plen <- 11

set.seed(seedinit)

## Now we're going to make a single function that does what the previous script did (many trials)
## First, repeat the doTrial function (we're leaving the older scripts behind, conceptually)
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

## Now make a function that calls it and returns a nice vector
## for us to use with replicate
doExp <- function(numTrials
                  , numCommunities, aveElders, shapeElders
                  , foiMean, foilsd, protection
)
{
  return(
    as.data.frame(t(replicate(numTrials
                              , doTrial(numCommunities, aveElders, shapeElders
                                        , foiMean, foilsd, protection
                              )
    )))
    |> mutate(
      sig = upr<1
      , cover = (lwr<1-protection) & (upr>1-protection)
    )
    |> summarise(coverage = mean(cover), power=mean(sig))
    |> unlist()
  )
}

## Now use sapply to call doExp for a range of protection values
prot <- seq(0, protection, length.out=plen)
effTest <- sapply(prot, function(p){
  return(doExp(numTrials, numCommunities, aveElders, shapeElders
               , foiMean, foilsd, protection=p
  ))
})

## Torture the results of sapply into a tidy data frame
eff <- (as_tibble(t(as.matrix(effTest)))
        |> mutate(protection=prot)
        |> gather(metric, proportion, coverage:power)
        |> mutate(protection=as.numeric(protection))
)

print(summary(eff))

## Make a nice picture
print(
  ggplot(eff, aes(protection, proportion, color=metric))
  + geom_line()
)

saveEnvironment()
