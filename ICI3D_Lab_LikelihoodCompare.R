## Likelihood Comparisons
## (C) Steve Bellan, Cari van Schalkwyk and the ICI3D team 2009–2025

## The code is made available under a Creative Commons Attribution 4.0 International License. You
## are free to reuse this code provided that you give appropriate credit, provide a link to the
## license, and indicate if changes were made. You may do so in any reasonable manner, but not in
## any way that suggests the licensor endorses you or your use. Giving appropriate credit includes
## citation of the original repository.

library(bbmle)
library(rjags)

## Imagine we sample 100 people from a population and 28 are HIV positive
sampleSize <- 100
samplePos <- 28                          

## Sample prevalence
## If we ASSUME that the sample is representative of a larger population, what can we conclude about this population?
## Also: How can we probe and question this assumption?
#### Are ANC prevalences representative of the general population? What population might they represent? …

## We can address the statistical question (what can we conclude under this assumptions)
## Under either Frequentist or Bayesian paradigms
## Using either direct methods or by exploring the likelihood surface
## Direct methods are more accurate, and faster
## Exploration-based methods are easier to generalize and apply to new circumstances (like fitting dynamical models)

#### Frequentist approach

## Direct
binom.test(samplePos, sampleSize)$conf.int

## Exploration (find the maximum likelihood)

confint(mle2(
	function(p) -dbinom(samplePos, size = sampleSize, prob = p, log = TRUE)
	, start = list(p = 0.5)
))

#### Bayesian approach

## Direct (based on Jeffreys prior)
alpha = 0.05
sampleNeg = sampleSize-samplePos
qbeta(c(alpha/2, 1-alpha/2), samplePos+1/2, sampleNeg+1/2)

## Exploration (MCMC sampling)

burn <- 1000
iterate <- 5000

data_list <- list(samplePos = samplePos, sampleSize = sampleSize)

model_string <- "
model {
	samplePos ~ dbin(p, sampleSize)
	p ~ dbeta(0.5, 0.5)
}
"

model <- jags.model(textConnection(model_string)
	, data = data_list, n.chains = 3
)

update(model, burn)

samples <- coda.samples(model, variable.names = "p", n.iter = iterate)

quantile(as.vector(as.matrix(samples)), c(0.025, 0.975))

## Note that Bayesian and Frequentist are making different assumptions and therefore estimating fundamentally different quantities (though very similar). By contrast, the exploration methods are _trying_ to estimate the same quantities as the direct methods, but using exploration (and also approximation, in the frequentist case); they are clumsier for simpler problems, but more flexible.
