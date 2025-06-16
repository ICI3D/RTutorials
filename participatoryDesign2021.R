## MMED Sampling design participatory coding 2021
## (C) ICI3D; some rights reserved

## Do existing COVID vaccines protect against the lambda variant?
## WHAT is our outcome variable? Severe disease.
## There are many other possibilities.
#### Deaths 

## How can we approach this question?
## What kind of study should we do?

## Post-vaccination surveillance
## Observational study: who was vaccinated what's the outcome?
## Randomized controlled trial: 
#### Advantage: balances all possible confounders (?)
#### Disadvantage: unethical

## Single-arm trial: simply give the vaccine and observe
#### Disadvantage: No direct comparison
#### Advantage: Allows you to vaccinate more people avoid ethical problems
## No-control trial: compare different vaccines

## Observational study
#### Vaccination status
#### Severe illness
#### Age 
#### co-morbidity
#### occupation

## We are going to assume transmission is exogenous (comes from outside)
## for the purpose of this exercise

library(tidyverse)

N <- 1000
vaccProp <- 0.6
minAge <- 20
maxAge <- 70
baseVacc <- 0.2 ## Should be positive
maxVacc <- 0.8
baseProg <- 0.1 ## Should be positive
maxProg <- 0.4
challengeProb <- 0.6
VE <- 0.5

pop <- tibble(NULL
              , id = as.character(1:N)
              , age = round(runif(N, minAge, maxAge))
              , vaccProb = baseVacc + (maxVacc-baseVacc)*(age-minAge)/(maxAge-minAge)
              , challengeProb = challengeProb
              , progProb = baseProg + (maxProg-baseProg)*(age-minAge)/(maxAge-minAge) 
              , vaccStatus = rbinom(N, 1, vaccProb)
)

pop <- (pop
        |> mutate(NULL
                   , disProb = challengeProb*progProb
                   , disProb = ifelse(vaccStatus==1, (1-VE)*disProb, disProb)
                   , disease = rbinom(N, 1, disProb)
        )
)

dat <- (pop
        |> select(id, age, vaccStatus, disease)
)

## Do a logistic regression
## pull out the estimate, confidence intervals and (maybe) the P value
## we can also use CIs to evaluate "significance"
## wrap all of this in a function
## Test assumptions, analysis plans, etc.
mod <- glm(disease ~ age + vaccStatus, family="binomial", data=dat)
summary(mod)
confint(mod)

print(pop, n=Inf)

print(ggplot(pop)
      + aes(x=age, y=vaccProb)
      + geom_point()
)
