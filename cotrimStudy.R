library(tidyverse)

## Is our novel drug better for treating malaria than cotrim?

## Individual-based RCT study: cotrim vs. aidamycin
## Can we improve slide-negativity on Day 5?

## 60% resolve under standard-of-care (cotrim)
## Hypothesize that 80% will resolve under new treatment

## Parameters
base_odds <- 1.5 ## Odds that cotrim will clear
new_odds <- 2 ## Odds ratio due to aidamycin
participants <- 80
participants <- 800000
seed <- 228

### Calculate protection proportions
oddProb <- function(o){
  return(o/(o+1))
}

## Derived quantities
base_cure <- oddProb(base_odds)
new_cure <- oddProb(base_odds*new_odds)
print(c(base_cure=base_cure, new_cure=new_cure))
## Not exactly what we expected, maybe.
## Odds can be confusing, but they're our friends

## Participants in each arm
socNum <- floor(participants/2)
newNum <- participants-socNum

set.seed(seed)

setup <- tibble(
  people = as.factor(1:participants)
  , treatment = sample(c(
    rep("cotrim", socNum)
    , rep("new", newNum)
  ))
)

result <- (setup
           |> mutate(
             cureProb = ifelse(treatment=="cotrim", base_cure, new_cure)
             , cured = rbinom(participants, size=1, prob=cureProb)
           )
)

## Let's imagine you're a researcher and all you see is these data
## What would you conclude?

mod <- glm(cured ~ treatment
           , family=binomial()
           , data = result
)

print(mod)
summary(mod)


