library(tidyverse)

## Is our novel drug better for treating malaria than cotrim?

## Individual-based RCT study: cotrim vs. aidamycin
## Can we improve slide-negativity on Day 5?

## 60% resolve under standard-of-care (cotrim)
## Hypothesize that 80% will resolve under new treatment

## Parameters
failure_base <- 0.4 ## Proportion who fail to clear under soc
prot <- 0.5 ## Additional proportion protected
participants <- 80
seed <- 0228

## Derived
socNum <- floor(participants/2)
newNum <- participants-socNum

set.seed(seed)

base_cure <- 1 - failure_base
new_cure <- 1 - failure_base*(1-prot)

setup <- tibble(
	people = as.factor(1:participants)
	, treatment = sample(c(
		rep("cotrim", socNum)
		, rep("aida", newNum)
	))
)

result <- (setup
	%>% mutate(
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


