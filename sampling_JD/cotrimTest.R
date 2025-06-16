library(tidyverse)

## Is our novel drug better for treating malaria than cotrim?

## Redo cotrimStudy.R as a function
## Then run many times to do an "experiment" in study design

## Parameters

## Individual-based RCT study: cotrim vs. aidamycin
## Can we improve slide-negativity on Day 5?

## 60% resolve under standard-of-care (cotrim)
## Hypothesize that 80% will resolve under new treatment

## Parameters
base_odds <- 1.5 ## Odds that cotrim will clear
new_odds <- 2 ## Odds ratio for additional protecton due to aidamycin
participants <- 80
seed <- 228

numTrials <- 100

set.seed(seed)

### Calculate protection proportions from odds
oddProb <- function(o){
  return(o/(o+1))
}

## Participants in each arm
## Standard-of-care treatment vs. new treatment
socNum <- floor(participants/2)
newNum <- participants-socNum

doStudy <- function(base_odds, new_odds, socNum, newNum){
  base_cure <- oddProb(base_odds)
  new_cure <- oddProb(base_odds*new_odds)
  print(c(base_cure, new_cure))
  
  setup <- tibble(
    people = as.factor(1:participants)
    , treatment = sample(c(
      rep("Cotrim", socNum)
      , rep("New", newNum)
    ))
  )
  
  result <- (setup
             |> mutate(
               cureProb = ifelse(treatment=="Cotrim", base_cure, new_cure)
               , cured = rbinom(participants, size=1, prob=cureProb)
             )
  )
  
  mod <- glm(cured ~ treatment
             , family=binomial()
             , data = result
  )
  
  ci <- exp(confint(mod)["treatmentNew", ])
  return(c(lwr=ci[[1]], upr=ci[[2]]))
}

## Test the function
doStudy(base_odds, new_odds, socNum, newNum)

## Run the function many (numTrials) times
trials <- replicate(numTrials
                    , doStudy(base_odds, new_odds, socNum, newNum)
)

## Some annoying R "magic" to make trials into a decent data frame
## followed by calculation of:
##  power – is the model confident the ratio > 1
##  coverage – is the true value inside the confidence interval?
trials <- (
  as.data.frame(t(trials))
  |> mutate(
    sig = lwr>1
    , cover = (lwr<new_odds) & (upr>new_odds)
  )
)

## print(trials)

## Coverage is good, but power is very low. 
## Good thing we tried the experiment first!
print(trials 
      |> summarise(coverage = mean(cover), power=mean(sig))
)

