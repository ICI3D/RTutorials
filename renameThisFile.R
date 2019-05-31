## Variability, Sampling Distributions and Simulation

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA

## (C) MMED community, 2019 www.ici3d.org

## We are going to _simulate_ a study and then analyze the data. Why?

library(dplyr)

set.seed(43)

makePop <- function(numChildren, meanHeight
	,sdHeight,meanGrowth
	,sdGrowth,treatGrowth
)
{
	## Put about half or the children into the treatment group at random
	## (the rest are controls)
	baseHeight <- rnorm(numChildren, meanHeight, sdHeight)
	numTreat <- floor(numChildren)/2
	treat <- sample(c(
		rep("vit", numTreat), rep("control", numChildren-numTreat)
	))
	growth <- rnorm(numChildren
		, mean = ifelse(treat=="vit", meanGrowth+treatGrowth, meanGrowth)
		, sd = sdGrowth
	)
	newHeight <- baseHeight + growth
	return(data.frame(baseHeight, treat, newHeight))
}

analyzePop <- function(pop){
	mod <- lm(newHeight ~ baseHeight + treat, data=pop)
	co <- (summary(mod)$coef)
	return(co[["treatvit", 4]])
}

simOutcomes <- function(numSims
	, numChildren = 50, meanHeight = 125
	,sdHeight = 12 ,meanGrowth = 8
	,sdGrowth = 2 ,treatGrowth = 0
)
{
	return(replicate(numSims, {
		analyzePop(makePop(numChildren, meanHeight
			,sdHeight,meanGrowth
			,sdGrowth,treatGrowth
		))
	}))
}

pVals <- simOutcomes(1000, treatGrowth=4)

hist(pVals)

print(mean(pVals<0.05))
