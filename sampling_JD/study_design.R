## Dushoff plus MMED gang, 2024
## We are going to experiment with how to design and evaluate a study in
## R, before spending a lot of time and money on conducting the study in W

## What is a good research question?
## These methods absolutely work for dynamical questions
## But not necessarily in 90 minutes 🙂

## What is the relationship between size of population and proportion needed to vaccinate? For which disease? Good, but a little too hard for right now

## What is the optimal time to vaccinate during an outbreak of cholera?
## How effective is the vaccination?

## How does ART treatment regimen affect viral suppression?
## Randomize patients to different ART regimens
## Hypothesis: new regimen will be better at viral suppression

## What are our goals for a successful study?
## Boost the immune system (goal for pharmacy, not study design)
## Ethical: do not randomize people below the standard of care)
## Measure viral load at start and at finish
## Quantify viral suppression for each individual 
## Quantify the difference between old and new regime

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

## Recruit people who are not yet on ART
## Record the previous viral load
numParticipants <- 1000
seed <- 2439
newEff <- 0.5
oldEff <- 0.5

tlist <- factor(c("old", "new"), levels=c("old", "new"))

set.seed(seed)

## Create a study population
## tibble is just like data.frame but more flexible
dat <- tibble(
	id = as.factor(1:numParticipants)
	, treatment = sample(tlist, numParticipants, replace=TRUE)
)

## print(dat)

## Do a study
## Some confounders we could consider in this experiment
#### Age, sex, risk group, community

## Some confounders we might want to consider for evaluation
#### Viral load
#### T-cell counts
#### Treatment outcomes

## Do an experiment!
dat <- (dat
	|> mutate(
		prob = ifelse(treatment=="old", oldEff, newEff)
		, suppressed = as.logical(rbinom(numParticipants, 1, prob))
	)
	|> select(-prob)
)

print(dat)

## We could analyze this with a contingency table
#### This is fine for very simple experiments, but not really flexible
#### Not easy to add confounders

## We could use a binomial-based test
#### Same difficulties as above

## The standard simple thing is logistic regression
#### Typically done using glm()
#### May also not be perfect depending on assumptions about confounders
#### Parameters may be hard to interpret
#### Can test all of this right here

## Analyze our data!

fit <- glm(suppressed ~ treatment, family="binomial", data=dat)

## glm is great because we can easily add confounders
## fit <- glm(dat, suppressed ~ treatment + age + sex)

## We can also generalize easily to other kinds of models:
## For example, we could imagine that community is a random effect
## fit <- glmer(dat, suppressed ~ treatment + age + sex + (1|community))

summary(fit)
confint(fit)
