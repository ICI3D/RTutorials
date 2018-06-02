library(tidyverse)

## Is our novel drug better for treating malaria than cotrim?

## Redo study.R as a function

## Parameters
base_odds <- 1.5 ## Proportion who fail to clear under soc
new_odds <- 1 ## Additional proportion protected
participants <- 80
numTrials <- 1000

seed <- 0228
set.seed(seed)

doStudy <- function(base_odds, new_odds, numParticipants){
	## Derived
	socNum <- floor(participants/2)
	newNum <- participants-socNum

	base_cure <- qlogis(log(base_odds))
	new_cure <- qlogis(log(base_odds+new_odds))

	setup <- tibble(
		people = as.factor(1:participants)
		, treatment = sample(c(
			rep("cotrim", socNum)
			, rep("new", newNum)
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

	ci <- confint(mod)["treatmentnew", ]
	return(c(lwr=ci[[1]], upr=ci[[2]]))
}

doStudy(base_odds, new_odds, numParticipants)

