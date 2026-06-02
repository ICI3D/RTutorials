
## This code can be deleted if we merge with introLikelihood
sampleSize <- 100
truePrev <- .3
set.seed(3)
samplePos <- rbinom(1, sampleSize, truePrev)
hypoPrevs <- seq(0,1, length=10000)

zmin <- 0.15
zmax <- 0.45
zoom <- (hypoPrevs>=zmin) & (hypoPrevs <= zmax)
zPrevs <- hypoPrevs[zoom]

######################################################################

## To do a Bayesian analysis we need a prior distribution
## A very cool one is the Jeffreys Prior, where the prior 
## probability at a point is related to the Fisher information of the distribution
## https://en.wikipedia.org/wiki/Jeffreys_prior
## The Jeffreys prior for the binomial distribution is a beta distribution with standard parameters both equal to 1/2.

## It looks like this

plot(hypoPrevs, dbeta(hypoPrevs, 1/2, 1/2)
	, main="A standard prior"
	, xlab="Prevalence"
	, ylab="Density"
	, type="l", log="y"
)

## Notice that the prior is very broad

######################################################################

## In this case, the Jeffreys prior is also a conjugate prior:
## the posterior will also have a beta distribution

## The posterior after our first sample looks like this:

sampleNeg = sampleSize-samplePos

plot(hypoPrevs, dbeta(hypoPrevs, samplePos+1/2, sampleNeg+1/2)
	, main="Posterior distribution"
	, xlab="Prevalence"
	, ylab="Density"
	, type="l", log="y"
)

## We can zoom in again

plot(zPrevs, dbeta(zPrevs, samplePos+1/2, sampleNeg+1/2)
	, main="Posterior distribution"
	, xlab="Prevalence"
	, ylab="Density"
	, type="l", log="y"
)

## The ICI3D-recommended way of choosing Bayesian CIs is quantiles
## This means we find the values that are only exceeded with probability of α/2, where α is our significance level

## Because we have the exact distribution of our posterior, 
## this is easy to calculate in this case
alpha = 0.05
ci <- qbeta(c(alpha/2, 1-alpha/2), samplePos+1/2, sampleNeg+1/2)
print(ci)

## We can show these values with arrows

base <- min(dbeta(zPrevs, samplePos+1/2, sampleNeg+1/2))
height <- dbeta(ci, samplePos+1/2, sampleNeg+1/2)

arrows(ci, base, ci, height)
