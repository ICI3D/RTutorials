## Introduction to MCMC 2: Fitting an SI model to an HIV epidemic with adaptive block MCMC
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
## Steve Bellan 2015
## Carl Pearson 2025
###################################################################### 

library(odin2)
library(monty)
library(posterior)

#' Part 1: Preview Review
#' In lab 7, you did some MCMC manually. Actually following all the way through
#' - doing multiple chains, checking diagnostics - requires substantially more
#' work... if done by hand! As you might imagine

sample_size <- 100
sample_positive <- rbinom(1, sample_size, 0.3)

prev_prior <- monty::monty_dsl({
	true_prevalence ~ Beta(shape1, shape2)
}, fixed = list(shape1 = 20, shape2 = 80))

prev_likelihood <- monty::monty_model_function(
	\(true_prevalence) dbinom(sample_positive, sample_size, true_prevalence, log = TRUE)
)

prev_posterior <- prev_prior + prev_likelihood

result <- monty_sample(
	prev_posterior,
	monty_sampler_adaptive(matrix(1)),
	n_steps = 1000, n_chains = 10
) |> posterior::as_draws_df()

posterior::summarise_draws(result) # `posterior` provides many options for summarizing chains
bayesplot::mcmc_dens_chains(result) # `bayesplot` provides many options for plotting mcmc results

#' Part 2: Apply MCMC to an Infectious Disease Model
#' Now you should consider a much more complicated MCMC problem: comparing an
#' infectious disease. We will use the data and one of the models from the
#' the HIV tutorial.

hivmodel <- odin2::odin({
	
	## parameters
	bb <- parameter(0.9)
	alpha <- parameter(8)
	birthRt <- parameter(.03)
	progRt <- parameter(.4) # parameter((1/10)*4)
	deathRt <- parameter(1/60.0)
	
	## initial conditions
	I0 <- parameter(90)
	S0 <- parameter(100000)
	
	initial(S) <- S0
	initial(I1) <- I0
	initial(I2) <- 0
	initial(I3) <- 0
	initial(I4) <- 0
	
	initial(infections, zero_every = 1) <- 0
	initial(deaths, zero_every = 1) <- 0
	
	## convenience variables
	I <- I1 + I2 + I3 + I4
	N <- I + S
	
	## processes
	infection <- bb * exp(-alpha * I / N) * S * I / N
	birth <- birthRt*N

	deriv(S) <- birth - infection - deathRt * S
	deriv(I1) <- infection - (progRt + deathRt) * I1
	deriv(I2) <- progRt * I1 - (progRt + deathRt) * I2
	deriv(I3) <- progRt * I2 - (progRt + deathRt) * I3
	deriv(I4) <- progRt * I3 - (progRt + deathRt) * I4
	
	deriv(infections) <- infection
	deriv(deaths) <- progRt * I4
	
	cases <- data()
	cases ~ Poisson(infections)
	
})

true_sys <- dust2::dust_system_create(hivmodel())
time <- seq(0, 39, by = 1)
dust2::dust_system_set_state_initial(true_sys)
dust2::dust_system_set_time(true_sys, 0)
y <- dust2::dust_system_simulate(true_sys, time)

reference_infections <- data.frame(
	time = seq(1, 39, by = 1),
	cases = rpois(dim(y)[2]-1, y[6, -1]),
	latent = y[6, -1]
)

library(ggplot2)

p <- ggplot(reference_infections) + aes(x=time) +
		geom_line(aes(y = latent, color = "latent")) +
		geom_point(aes(y = cases, color = "observed")) +
		theme_minimal() + theme(
			legend.position = "inside",
			legend.position.inside = c(0, 1), legend.justification.inside = c(0, 1)
		) +
		scale_x_continuous("Simulated Year") +
		scale_y_log10("Cases", limits = c(100, NA)) +
		scale_color_manual(NULL, values = c(
			latent = "firebrick", observed = "black",
			prior = "purple", posterior = "darkgreen"
		))

print(p)

hiv_likelihood <- dust2::dust_likelihood_monty(
	dust2::dust_unfilter_create(hivmodel(), 0, reference_infections),
	monty_packer(c("alpha", "bb", "progRt"))
)

hiv_prior <- monty::monty_dsl({
	alpha ~ Uniform(.2, 2) 
	bb ~ Uniform(1, 30)
	progRt ~ Uniform(.1, 1)
})

rng <- monty_rng_create()
prior_pars <- lapply(
	1:100, \(x) as.list(hiv_prior$direct_sample(rng)) |> setNames(c("alpha", "bb", "progRt"))
)

prior_sys <- dust2::dust_system_create(
	hivmodel(), pars = prior_pars, n_groups = length(prior_pars)
)
time <- seq(0, 39, by = 1)
dust2::dust_system_set_state_initial(prior_sys)
dust2::dust_system_set_time(prior_sys, 0)
y <- dust2::dust_system_simulate(prior_sys, time)
y[6,,] # state, sample, time

library(data.table)

prior_dt <- rbindlist(apply(y[6,,], 1, \(ts) data.table(time = seq_along(ts), cases = round(ts))), idcol = "sample")

p + geom_line(
	aes(y=cases, color = "prior", group = sample),
	data = prior_dt, alpha = 0.1
)

hiv_posterior <- hiv_likelihood + hiv_prior

results <- monty_sample(
	hiv_posterior, monty_sampler_random_walk(diag(3)*0.02), 1000,
	n_chains = 10
)

results_draws <- posterior::as_draws_df(results)

post_slice <- sample(dim(results_draws)[1], 100)

post_pars <- lapply(
	seq_len(100),
	\(i) results_draws[post_slice[i], c("alpha", "bb", "progRt")] |> as.list()
)

post_sys <- dust2::dust_system_create(
	hivmodel(), pars = post_pars, n_groups = length(post_pars)
)
time <- seq(0, 39, by = 1)
dust2::dust_system_set_state_initial(post_sys)
dust2::dust_system_set_time(post_sys, 0)
y <- dust2::dust_system_simulate(post_sys, time)

library(data.table)

post_dt <- rbindlist(apply(y[6,,], 1, \(ts) data.table(time = seq_along(ts), cases = round(ts))), idcol = "sample")

p + geom_line(
	aes(y=cases, color = "prior", group = sample),
	data = prior_dt, alpha = 0.1
) + geom_line(
	aes(y=cases, color = "posterior", group = sample),
	data = post_dt, alpha = 0.1
)

bayesplot::mcmc_hist_by_chain(results_draws)

## Question: Have your chains converged? How can you tell?

## Challenge: adjust the above code to try this with more
## informative priors - how does that change convergence?