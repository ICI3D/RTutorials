## MMED 2023
## Follow up on study (rename these files soon!)

library(dplyr)
library(tibble)

set.seed(20230705)

## Can we confirm that PreP prevents acquisition of HIV in a certain population?

## Recruit HIV-negative persons at risk and randomize them to different treatment groups
## e.g., female sex workers, MSM, people in a partnership with a positive person

## For now, we assume that one of the treatments is effective a placebo (no effect), but this has ethical concerns, so we may come back and try a comparative study instead

## Units trick!
## We can talk about this later, or feel free to ignore it
year = 7.1


doStudy <- function(studyTime, hC, N, hreduction){
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
  
  m <- glm(inf ~ group, data=study, family=binomial())
  return(confint(m, method="Wald")["groupT", ])
  ## Wald confidence are less accurate but more computationally robust
}

## Study parameters
m <- doStudy(studyTime = 1*year
             , hC = 0.05/year
             , N = 500
             , hreduction = 3
)

## Validate: in the null world do we get the right amount of coverage?

numTrials <- 1000
validationStudy <- as.data.frame(t(replicate(numTrials 
                                             , doStudy(studyTime = 1*year
                                                       , hC = 0.05/year
                                                       , N = 500
                                                       , hreduction = 1
                                             )
)))

print(mean(validationStudy[[1]] > 0))
print(mean(validationStudy[[2]] < 0))


numTrials <- 1000
powerStudy <- as.data.frame(t(replicate(numTrials 
                                        , doStudy(studyTime = 1*year
                                                  , hC = 0.05/year
                                                  , N = 500
                                                  , hreduction = 3
                                        )
)))

print(mean(powerStudy[[1]] > 0))
print(mean(powerStudy[[2]] < 0))

