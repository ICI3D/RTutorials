## MMED 2023
## Follow up on study (rename these files soon!)

library(dplyr)
library(tibble)

## Can we confirm that PreP prevents acquisition of HIV in a certain population?

## Recruit HIV-negative persons at risk and randomize them to different treatment groups
## e.g., female sex workers, MSM, people in a partnership with a positive person

## For now, we assume that one of the treatments is effective a placebo (no effect), but this has ethical concerns, so we may come back and try a comparative study instead

## Units trick!
## We can talk about this later, or feel free to ignore it
year = 7.1
numTrials <- 1000
studyTime <- 5
N <- 100

set.seed(20230705)

#' @param studyTime duration of study in years
#' 
#' @param hC hazard rate in control group
#' 
#' @param N number of participants
#' 
#' @param hreduction hazard rate reduction in treatment group
#' 
#' @return confidence interval for treatment effect
doStudy <- function(
    studyTime, hC, N, hreduction
){
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

doResult <- function(
    numTrials, studyTime, hC, N, hreduction
) {
  ## Validate: in the null world do we get the right amount of coverage?
  validationStudy <- as.data.frame(t(replicate(numTrials 
                                               , doStudy(studyTime = studyTime
                                                         , hC = hC
                                                         , N = N
                                                         , hreduction = 1
                                               )
  )))
  
  ## Power: in the alternative world what is chance of success?
  powerStudy <- as.data.frame(t(replicate(numTrials 
                                          , doStudy(studyTime = 1*year
                                                    , hC = hC
                                                    , N = N
                                                    , hreduction = hreduction
                                          )
  )))
  
  return(list(
    CIabove = mean(validationStudy[[1]] > 0),
    CIbelow = mean(validationStudy[[2]] < 0),
    someEffect = mean(powerStudy[[1]] > 0),
    wrongEffect = mean(powerStudy[[2]] < 0)
  ))
  
}

# 
.args <- if (interactive()) {
  c(0.05/year, 3, "test.rds")
} else commandArgs(trailingOnly = TRUE)
control_hazard <- as.numeric(.args[1])
treatreduction <- as.numeric(.args[2])

res <- doResult(numTrials, studyTime, control_hazard, N, treatreduction)

saveRDS(res, tail(.args, 1))
