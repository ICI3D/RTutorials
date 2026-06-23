## Dushoff plus MMED gang, 2026
## We are going to experiment with how to design and evaluate a study in
## R, before spending a lot of time and money on conducting the study in W

## Science goals

## Statistical goals
## 1. If there is no difference (hypothetically), we don't want to find anything
#### The proportion of false positives should not exceed α, 
#### α is my significance level (typically 0.05)
#### This value is more cultural than scientific and sometimes will change
## 2. If there's a difference, we hope to see it clearly
#### We should try to make sure we have enough samples that this is likely
#### If we can control for relevant factors, we will have a better chance (confounders, and so on)
## 3. We would like valid confidence intervals for the effect

## Study plan
## Validation
## Do a study with parameters that we believe, and see whether it works
## Do 1000 studies, and see what range of CIs we get, and what “coverage” we get
## Do 1000 studies many more times, each time varying something about our assumptions or sample size

## Evaluation (compare our model world to a more complicated model world)

######################################################################

## Google intro dplyr
## dplyr is super-cool!
library(dplyr)
