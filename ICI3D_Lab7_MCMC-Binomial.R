#' Introduction to MCMC 1: Estimating a posterior binomial probability
#' Clinic on the Meaningful Modeling of Epidemiological Data
#' International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
#' African Institute for Mathematical Sciences, Muizenberg, RSA
#' Steve Bellan 2012, 2015
#' Carl Pearson 2025
#'
#' By the end of this tutorial you should be able to:
#'  * Write a likelihood function for binomially distributed data
#'  * Explain the Metropolis-Hasting algorithm
#'  * Explain how proposal distribution affects MCMC convergence
#'  * Assess MCMC convergence with the Gelman-Rubin diagnostic
#'    and trace plots

library(ggplot2)
library(dplyr)
library(tidyr)

#' Part 1: Sample Data
#' Imagine there is a population of individuals with a 30% prevalence of some
#' disease. If you were to draw a 100 person sample, you could estimate the
#' population prevlance from that sample. You may recall using other techniques
#' to make that estimate in this workshop or other courses; here, we are going
#' to use the Bayesian perspective with MCMC to estimate the posterior
#' probability distribution of the prevalence from this "data" and a prior
#' distribution (informative or uninformative) for the prevalence.

true_prevalence <- .3
sample_size <- 100
sample_positive <- rbinom(1, sample_size, true_prevalence)
sample_prevalence <- sample_positive / sample_size

#' @question How do the true_ and sample_prevalence compare? What could
#' you change to make them more likely to be close for any given draw?

#' Part 2: Prior Estimate
#' Now we need to specify our prior probability distribution. This corresponds
#' our belief *before* we conducted the study regarding prevalence in the
#' population. For example, there might be a *global* estimate of the prevalence
#' or a previous local study some time ago. Or if this disease has a high
#' mortality rate, we might have a reasonable guess about how many people
#' experience it. However, we might also have little to no information, and thus
#' choose an _uninformative_ prior, meaning we assume as wide a range of values
#' as plausible.
#' 
#' Our parameter of interest is prevalence, which is bounded between 0 and 1.
#' The `beta` distribution matches these bounds and is thus commonly used for
#' the probability distribution for parameters that are themselves
#' probabilities. We will use it in this example.
#' 
#' One reminder: for numerical stability issues, we generally prefer to use the
#' `log` scale when working with likelihood calculations.

# ignore these for now - come back and change them later
# default_shape1 <- 1
# default_shape2 <- 1

#' @param prevalence a probability vector; the estimate(s) we wish to evaluate
#' @param shape1 a positive numeric; see [stats::dbeta()]
#' @param shape2 a positive numeric; see [stats::dbeta()]
#' 
#' @details
#' `shape1` and `shape2` are related to the mean and variation of the prior by
#' $$
#' mean = shape1 / (shape1 + shape2)
#' variation = shape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
#' $$
#' 
beta_prior <- function(
	prevalence,
	shape1 = default_shape1, ## for 30%, should adjust shape1 and shape2 to get some estimate
	shape2 = default_shape2  ## of the mean other than 50% and some tighter confidence
) {
	return(dbeta(prevalence, shape1 = shape1, shape2 = shape2, log = TRUE))
}

#' @param prevalence a probability vector; the estimate(s) we wish to evaluate
#' @param data a list-like structure with `observed` and `size` elements; by
#'        default, set to our sample from earlier
#' 
#' @details
#' Recall: the likelihood of a particular prevalence is the probability of the
#' observed outcome out of a particular size sample, if that prevalence were
#' true.
likelihood <- function(
		prevalence,
    data = list(size = sample_size, observed = sample_positive)
) {
  return(with(
  	data, dbinom(observed, size = size, prob = prevalence, log = TRUE)
  ))
}

#' We generally need both the prior and the likelihood, so let's define
#' their sum for convenience.
#' 
#' n.b. we use the combined parameters of [likelihood()] and [beta_prior()]
likelihood_and_prior <- function(
	prevalence,
	shape1 = default_shape1, shape2 = default_shape2,
	data = list(size = sample_size, observed = sample_positive)
) {
	return(beta_prior(prevalence, shape1, shape2) + likelihood(prevalence, data))
}

#' @question What did you change shape1 and shape2 to? Why?

#' Part 3:
#' Let's examine some plots for our prior and likelihood functions. In the plots
#' below, we are transforming most of our functions results from the `log` scale
#' with the `|> exp()`
#' 
#' To do so, we'll make a data structure containing all the views we want to see
#' and then plot them.
views <- factor(
	c(
		"Prior", "Likelihood Only", "Likelihood & Prior", "Log(Likelihood & Prior)"
	),
	levels = c(
		"Prior", "Likelihood Only", "Likelihood & Prior", "Log(Likelihood & Prior)"
	),
	ordered = TRUE
)

plot_df <- expand.grid(prevalence = seq(0, 1, by = 0.001), view = views) |>
	mutate(y = case_when(
		view == views[1] ~ exp(beta_prior(prevalence)),
		view == views[2] ~ exp(likelihood(prevalence)),
		view == views[3] ~ exp(likelihood_and_prior(prevalence)),
		view == views[4] ~ likelihood_and_prior(prevalence)
	))

ggplot(plot_df) +
	aes(prevalence, y) +
	geom_line() +
	facet_wrap(~ view, scales = "free_y", nrow = 2) +
	theme_minimal()

#' Part 4: Run Metropolis-Hastings Markov Chain Monte Carlo (MCMC)
#' For the beta-prior, binomial-likelihood, single data point case, one can
#' directly write down the posterior distribution. But real problems are rarely
#' as simple. This exercise demonstrates how to use MCMC techniques on a simple
#' problem so you can see how they work, but they can also solve more
#' complicated problems as you will see in other exercises.

# qlogis and plogis are logit and inverse logit functions, respectively

iterations <- 1000 # how many steps in the chain?
chain <- array(dim = c(iterations + 1, 3)) # pre allocate storage space accordingly
proposer <- function(previous, sd = 0.1) rnorm(1, previous, sd) # proposer for next iteration
logit_initial_prevalence <- runif(1, qlogis(0.01), qlogis(.99))
initial_prevalence <- plogis(logit_initial_prevalence)

chain[1, ] <- c(
	logit_initial_prevalence, initial_prevalence, likelihood_and_prior(initial_prevalence)
)

for (step in seq_len(iterations)) {
	proposal <- proposer(chain[step, 1])
	prop_prev <- plogis(proposal)
	ll_proposal <- likelihood_and_prior(prop_prev)
	metro_hasting_ratio <- exp(ll_proposal - chain[step, 3])
	if (runif(1) < metro_hasting_ratio) { # accept proposal
		chain[step + 1, ] <- c(proposal, prop_prev, ll_proposal)
	} else { # reject proposal
		chain[step + 1, ] <- chain[step, ]
	}
}

chain_df <- as.data.frame(chain) |>
	setNames(c("sample", "prevalence", "likelihood")) |>
	within({ iteration <- seq_along(sample) }) |>
	pivot_longer(cols = -c(iteration))

ggplot(chain_df) + aes(iteration, value) +
	geom_line() +
	facet_grid(name ~ ., scales = "free_y") +
	theme_minimal()

#' Part 5: Questions!
#' @question What is the Gelman-Rubin diagnostic?
#' @hint you might need to look it up!
#' 
#' @question If you wanted to look at the G-R diagnostic, what steps would take?
#' 
#' @question Bonus: if you have time, start taking those steps, by:
#'  - generating multiple independent chains
#'  - plotting those chain trajectories
#'  - plotting the corresponding posterior distributions
#'  - calculating the Gelman-Rubin diagnostic
