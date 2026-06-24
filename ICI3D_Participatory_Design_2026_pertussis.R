## Dushoff plus MMED gang, 2026
## We are going to experiment with how to design and evaluate a study in
## R, before spending a lot of time and money on conducting the study in W

## Science goals
## Our question is whether it would be good to give pregnant women the existing, adult pertussis vaccine
## Will it protect their children? Short term
#### Static question 
## Will it protect their and other children? Medium term
#### Dynamic question 
## What are some possible confounders?
#### socio-economics
#### Other medical issues (include the info or possibly exclude the moms)
#### Specific environment (clinic or neighborhood)
#### Baby health, age, medical factors (e.g., pre-term)

## Other issues
#### Exclusion criteria could include previous adult vax or disease

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
## We should give the vaccine to some mothers but not others
## Enroll at an ANC – enroll first, then randomize
## Treatment group get the vaccine
## Control group also gets a shot, probably of something good but not relevant: multi-vitamin, pneumococcal vaccine ...

## Validation
## Do a study with parameters that we believe, and see whether it works
## Do 1000 studies, and see what range of CIs we get, and what “coverage” we get
## Do 1000 studies many more times, each time varying something about our assumptions or sample size

## Evaluation (compare our model world to a more complicated model world)

######################################################################

## Internet search for intro dplyr
## dplyr is super-cool!
library(dplyr)

## We almost never want numbers floating around our code
branchSize <- 100
branches <- 2
numClinics <- 2
people <- branches*branchSize
clinics <- paste0("C", 1:numClinics)
treatments <- c(rep("vacc", branchSize), rep("control", branchSize))

## We are enrolling women as they come and using a fixed order
## We could try to get the same number of women per cline
## We could stratify by clinic
## This is something we can test later
pop <- tibble(id = as.character(people)
	, clinic = sample(clinics, people, replace=TRUE)
	, treatment = sample(treatment)
)

## What is our outcome? What is baseline risk? How might treatment modify it?
## Outcome is reported severe pertussis disease
## Based on voluntary presentation
## First year of life?
## What to do about routine immunization?
## Expert says we don't want to exclude or shorten our time period
#### Question: should we control for who gets it in our analysis?
#### Answer: this is a really good topic for this sort of simulation
#### Whatever the answer the power of the experiment will be improved reducing variability – encourage women to bring and vaccinate their babies
#### Secondary outcome variable could be when in life child gets sick

baselineRisk <- 0.1
protection <- 0.7 ## hypothesized
protection <- 0 ## null
## vaccRisk <- ????
## We can calculate using RR or ORs
## In this experiment we're going to use OR first because it matches our statistical model best
## We can then experiment with how well it still works with RR
