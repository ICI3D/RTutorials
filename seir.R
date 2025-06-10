# seir.R
# JRCP 06.05.09
# Updated for 2012
#
# NOTE: The model and parameters used in this tutorial modified from:
#
# Earn et al. _A Simple Model for Complex Dynamical Transitions in Epidemics._
# 2000; Science 287: 667-670; DOI: 10.1126/science.287.5453.667

beta_calc <- function(
  R_0, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
) {
  R_0 / ((sigma / (mu + sigma)) * (1 / (mu + gamma)))
}

S.star <- function(
  beta, N, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
) {
  N * ((mu + sigma) / beta) * ((mu + gamma) / sigma)
}

E.star <- function(
  beta, N, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
) {
  mu * (N - S.star(beta, N, mu, sigma, gamma)) / (mu + sigma)
}

I.star <- function(
  beta, N, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
) {
  (sigma / (mu + gamma)) * E.star(beta, N, mu, sigma, gamma)
}

R.star <- function(
  beta, N, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
) {
  (gamma / mu) * I.star(beta, N, mu, sigma, gamma)
}

N0 <- 500000

S.star(beta_calc(13), N0) +
  E.star(beta_calc(13), N0) +
  I.star(beta_calc(13), N0) +
  R.star(beta_calc(13), N0)

S.star(beta_calc(18), N0)
E.star(beta_calc(18), N0)
I.star(beta_calc(18), N0)
R.star(beta_calc(18), N0)

E.star(beta_calc(13), N0) + I.star(beta_calc(13), N0)

E.star(beta_calc(18), 300000) + I.star(beta_calc(18), 300000)

E.star(beta_calc(13), 7200) + I.star(beta_calc(13), 7200)
E.star(beta_calc(18), 7200) + I.star(beta_calc(18), 7200)

## NOTE: The rest of the script is used for the BONUS question.
## You are encouraged to continue commenting the script below this
## line but you should first complete benchmark questions 3 and
## 4 to ensure that you have mastered the script up to this point.

library(deSolve)

seir <- function(t, y, params) {
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]

	beta <- params["beta"]
	N <- params["N"]
	mu <- params["mu"]
	sigma <- params["sigma"]
	gamma <- params["gamma"]
	nu <- mu * N

	dSdt <- nu - (beta * S * I / N) - (mu * S)
	dEdt <- (beta * S * I / N) - (mu * E) - (sigma * E)
	dIdt <- (sigma * E) - (mu * I) - (gamma * I)
	dRdt <- (gamma * I) - (mu * R)

	return(list(c(dSdt, dEdt, dIdt, dRdt)))
}

## Note that N0 is defined earlier in the script as 500,000.

param_vals <- c(
  beta = beta_calc(18), N = N0, mu = 0.02 / 365.25, sigma = 1 / 8, gamma = 1 / 5
)

times <- seq(0, 70 * 365, 10)

S0 <- 20000
E0 <- 200
I0 <- 125

## Stop and think about this. What is the level of immunity in
## the population for these initial conditions and a population
## size N0?

init <- c(sus = S0, exp = E0, inf = I0, rec = N0 - S0 - E0 -I0)

tc <- data.frame(lsoda(init, times, seir, param_vals))

plot(x = tc$time/365, y = tc$sus / N0,
	type = "l", xlab = "Time (years)", ylab = "Proportion of population",
	bty = "n", ylim = c(0, max(tc$sus / N0))
)
text(50, 1.1 * S.star(beta_calc(18), N0) / N0, "S(t)")
lines(tc$time / 365, (tc$exp + tc$inf) / N0, col = "green")
text(50, 0.2 * S.star(beta_calc(18), N0) / N0, "E(t)+I(t)", col = "green")

plot(tc$exp / N0, tc$sus / N0, type = "l", xlab = "EXP", ylab = "SUS", bty = "n")
points(E.star(beta_calc(18), N0) / N0, S.star(beta_calc(18), N0) / N0, pch = "*", col = "red")
points(E0 / N0, S0 / N0, col = "blue")

plot(tc$inf / N0, tc$sus / N0, type = "l", xlab = "INF", ylab = "SUS", bty = "n")
points(I.star(beta_calc(18), N0) / N0, S.star(beta_calc(18) , N0) / N0, pch = "*", col = "red")
points(I0 / N0, S0 / N0, col = "blue")

#' @section Benchmark Questions
#'
#' @question Comments?
#' This file contains code related to the SEIR model presented lectures, but the
#' code does not include any commenting. Add comments that describe the code
#' presented in seir.R. Make sure you understand each part of every line of the
#' code. You should be able to find the necessary information in the help files
#' for the `deSolve` library and the `lsoda()` function, but if anything also
#' feel free to ask the faculty or mentors for clarification.
#'
#' @question
#'
#' Recall in today's class on Dynamics of Directly Transmitted Pathogens, we
#' examined for R0 (13, 18), the transmission rate in a population of 7,200 people,
#' and the total number of infected persons (E* + I*), Now:
#' Use the functions E.star() and I.star() to fill in the missing parts of the
#' table on Slide 32-33 from this morning’s lecture:
#' That is, calculate the transmission rate (beta) and the endemic equilibrium value for the number of infected
#' (exposed/latent + infectious) individuals in a population of 300,000 with an
#' R0 of 13 and in a population of 500,000 with an R0 of 18.
#'
#' @question
#' 
#' Write a new function that calculates the number of infected
#' individuals the endemic equilibrium value for the number of infected
#' (exposed/latent + infectious) individuals in a population from the arguments
#' population.size and reproduction.number
#'
#' @tip
#' 
#' answer should look like the following:

ei.star <- function(population.size, R0) {
	ouptut <- `????` # some code here
	return(output)
}

#' @question BONUS
#'
#' Using the seir() function as template, create a new function seir.seasonal()
#' that allows beta to vary with time. Define beta as a sinusoidal wave with a
#' period of 1 year, an amplitude of 0.08*β, and a mean value that would give
#' an R0 value of 18 for the non-seasonal model.
