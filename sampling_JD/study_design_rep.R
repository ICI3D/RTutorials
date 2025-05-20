
## See notes in study_design.R

library(dplyr)

seed <- 2439
set.seed(seed)

oneStudy <- function( numParticipants, newEff, oldEff){
	tlist <- factor(c("old", "new"), levels=c("old", "new"))

	## See comments in study_design.R
	dat <- tibble(
		id = as.factor(1:numParticipants)
		, treatment = sample(tlist, numParticipants, replace=TRUE)
	)

	dat <- (dat
		|> mutate(
			prob = ifelse(treatment=="old", oldEff, newEff)
			, suppressed = as.logical(rbinom(numParticipants, 1, prob))
		)
		|> select(-prob)
	)

	fit <- glm(suppressed ~ treatment, family="binomial", data=dat)

	return(c(
		est=coef(fit)[["treatmentnew"]]
		, lwr = confint(fit)[["treatmentnew", 1]]
		, upr = confint(fit)[["treatmentnew", 2]]
	))
}

oneStudy(
	numParticipants=1000
	, newEff=0.5, oldEff=0.5
)


manyStudies <- function(numReplicates, numParticipants, newEff, oldEff){
	wide <- replicate(numReplicates, 
		oneStudy(numParticipants=numParticipants
			, newEff=newEff, oldEff=oldEff
		)
	)
	return(t(wide))
}

# replicate(numReplicates, function(n){return(oneStudy(numReplicates, ...))})

manyStudies(
	numReplicates = 10
	, numParticipants=1000
	, newEff=0.5, oldEff=0.5
)

## Continued on [study_design_rep.R]
