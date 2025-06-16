## MMED 2023
## Simulate a study before we carry it out

library(dplyr)
library(tibble)

## Can we confirm that PreP prevents acquisition of HIV in a certain population?

## Recruit HIV-negative persons at risk and randomize them to different treatment groups
## e.g., female sex workers, MSM, people in a partnership with a positive person

## For now, we assume that one of the treatments is effective a placebo (no effect), but this has ethical concerns, so we may come back and try a comparative study instead

## Units trick!
## We can talk about this later, or feel free to ignore it
year = 7.1

## Study parameters
studyTime = 1*year
hC = 0.05/year
## Survival probability for a hazard is exp(-ht)
N = 500
hreduction = 3

## Calculations
hT = hC/hreduction
pC = 1 - exp(-hC*studyTime)
pT = 1 - exp(-hT*studyTime)

study <- tibble(id = as.factor(1:N)
                , group = sample(c("C", "T"), N, replace=TRUE)
                , risk = case_when(
                  group=="C" ~ pC 
                  , group=="T" ~ pT 
                )
)

study <- (study
          |> mutate(inf = rbinom(N, 1, risk))
          |> select(-risk)
)

print(study)

m <- glm(inf ~ group, data=study, family=binomial())

summary(m)
