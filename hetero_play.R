library(shellpipes)
loadEnvironments()

par(mfrow=c(1,3)) 

beta.low <- 0.8

## Large pop
het.epidemic(beta.mean = 2, beta.var = .001
	, runs = 100, pop.size = 200
	, end.time = 10, gmma = 1
)

## Baseline
het.epidemic(beta.mean = 2, beta.var = .001
	, runs = 100, pop.size = 50
	, end.time = 10, gmma = 1
)

## High variance
het.epidemic(beta.mean = 2, beta.var = 2
	, runs = 100, pop.size = 50
	, end.time = 10, gmma = 1
)

## Low R
het.epidemic(beta.mean = beta.low, beta.var = .001
	, runs = 100, pop.size = 50
	, end.time = 10, gmma = 1
)

## Low R high variance
het.epidemic(beta.mean = beta.low, beta.var = 2
	, runs = 100, pop.size = 50
	, end.time = 10, gmma = 1
)

