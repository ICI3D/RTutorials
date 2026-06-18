#' Fitting 1: Fitting a dynamic SI model with Maximum Likelihood Estimation
#' Clinic on the Meaningful Modeling of Epidemiological Data
#' International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
#' African Institute for Mathematical Sciences, Muizenberg, RSA
#' (C) Steve Bellan, 2015, Seth Blumberg 2025, Carl Pearson 2026
#'
#' By the end of this tutorial you should ...
#'  * Understand how to simulate cross-sectional prevalence data around a
#'    simulated epidemic.
#'  * Calculate a binomial likelihood from these prevalence data and a fully
#'    specified epidemic model.
#'  * Use R's `optim` function to do multivariate optimization over transformed
#'    parameter space.
#'  * Understand the difference betweeen SANN and Nelder-Mead algorithms
#'  * Create 95% CIs and contours from the hessian matrix
#'  * Create 95% CIs and contours based on the likelihood ratio
#'
#' n.b. the code below uses `roxygen2` syntax for annotating comments. That
#' includes providing example calls of the functions in @examples blocks.
#' The code in @examples blocks

library(deSolve)
library(ggplot2)

library(boot)
library(ellipse)

#' @title Check if a value is a positive scalar
#'
#' @param x The value to check.
#' @return logical. `TRUE` if `x` is a positive scalar, `FALSE` otherwise.
is_positive_scalar <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0
}

#' @title Check if a value is a proportion
#'
#' @param x The value to check.
#' @return logical. `TRUE` if `x` is a proportion, `FALSE` otherwise.
is_proportion <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0 && x <= 1
}

#' @title Collect Disease Parameters
#'
#' @description
#' Creates a list of disease parameters with their default values for the HIV
#' epidemic model.
#'
#' This function captures its local environment variables using
#' `as.list(environment())` to construct a named list from the arguments while
#' providing convenient defaults. This function also validates those parameters.
#'
#' @param Beta numeric. Transmission rate (infectious contacts per
#'   year-infectious-persons). Default is 0.9.
#' @param alpha numeric. Decline in transmission coefficient with prevalence
#'   (unitless). Default is 8.
#' @param progRt numeric. Progression rate, defined as `n` divided by
#'   the life-with-HIV expectancy, where `n` is the number of boxcars
#'   (1 / year). Default is 4/10.
#' @param birthRt numeric. Birth rate, representing the proportion of people
#'   who give birth per year (new people / current people / year).
#'   Default is 0.03.
#' @param deathRt numeric. Background death rate, defined as 1 divided by
#'   the life expectancy (1 / year). Default is 1/60.
#'
#' @return list. A named list containing the defined disease parameters.
#'
#' @examples
#' disease_params()
#' disease_params(Beta = .2)
disease_params <- function(
  Beta = 0.9,
  alpha = 8,
  progRt = (4 / 10),
  birthRt = .03 / 1,
  deathRt = 1 / 60
) {
  stopifnot(
    "Beta must be a positive scalar" = is_positive_scalar(Beta),
    "progRt must be a positive scalar" = is_positive_scalar(progRt),
    "birthRt must be a positive scalar" = is_positive_scalar(birthRt),
    "deathRt must be a positive scalar" = is_positive_scalar(deathRt)
  )
  return(as.list(environment()))
}


#' @title Initialize State Variables from Prevalence
#'
#' @description
#' Creates a named vector of initial state values (proportions of the
#' population) for the SI ODE model given the initial prevalence.
#'
#' @param initPrev numeric. The initial prevalence at the start of the
#'   simulation, which must be a proportion (between 0 and 1 inclusive).
#'   Default is `exp(-7)`.
#'
#' @return numeric. A named vector of initial proportions for state variables:
#'   `S` (susceptibles), `I1`, `I2`, `I3`, `I4` (infected classes), `CI`
#'   (cumulative incidence), and `CD` (cumulative deaths).
#'
#' @examples
#' initialize_from_prev()
#' initialize_from_prev(0.01)
initialize_from_prev <- function(
  initPrev = exp(-7)
) {
  stopifnot(
    "initPrev must be a proportion between 0 and 1" = is_proportion(initPrev)
  )
  return(c(
    S = 1 - initPrev,
    I1 = initPrev,
    I2 = 0,
    I3 = 0,
    I4 = 0,
    CI = 0,
    CD = 0
  ))
}

#' @title SI ODE Model with Four Infected Boxcars
#'
#' @description
#' Calculates the derivatives for the SI (Susceptible-Infected) epidemic model
#' with 4 boxcar stages of infection. This model is equivalent to the third
#' model in the HIV in Harare tutorial, although some parameters may differ.
#'
#' @param tt numeric. The current time point in the integration.
#' @param yy numeric. Named vector of current state variable values: `S`
#'   (susceptible), `I1`, `I2`, `I3`, `I4` (infected classes), `CI`
#'   (cumulative incidence), and `CD` (cumulative deaths).
#' @param parms list. A list of disease parameters containing:
#'   * `Beta`: transmission rate
#'   * `alpha`: decline in transmission coefficient with prevalence
#'   * `progRt`: progression rate through boxcars
#'   * `birthRt`: birth rate
#'   * `deathRt`: natural death rate
#'
#' @return list. A list containing a single numeric vector of the state
#'   variable derivatives.
#'
#' @examples
#' dHIV_SI4(0, initialize_from_prev(), disease_params())
dHIV_SI4 <- function(tt, yy, parms) {
  with(c(parms, as.list(yy)), {
    ## State variables are: S, I1, I2, I3, I4; tracking states CI, CD
    pop_N <- sum(yy) ## total population
    pop_I <- pop_N - S
    prev_I <- pop_I / pop_N
    transmissionCoef <- Beta * exp(-alpha * prev_I)

    # processes
    infection <- transmissionCoef * prev_I * S
    birth <- birthRt * pop_N
    death <- deathRt * yy[1:5]
    progress <- progRt * yy[2:5]

    deriv <- rep(NA, length(yy))
    ## state variable derivatives (ODE system)
    ## Susceptibles
    deriv[1] <- birth - death[1] - infection
    ## I Boxcars
    ## each lose their progress, gain earlier progress, die of natural causes
    deriv[2:5] <- -progress + c(infection, progress[1:3]) - death[2:5]
    ## Cumulative Incidence
    deriv[6] <- infection
    ## Cumulative Disease Induced Death, i.e progression from final boxcar
    deriv[7] <- progress[4]

    return(list(deriv))
  })
}

#' @title Simulate Epidemic
#'
#' @description
#' Runs the deterministic model simulation using the specified ODE system,
#' initial state values, time sequence, and disease parameters.
#'
#' @param init numeric. Named vector of initial state values. Default is
#'   `initialize_from_prev()`.
#' @param tseq numeric. Sequence of times at which model predictions are
#'   desired. Default is monthly steps from 1976 to 2015.
#' @param modFunction function. The ODE system derivative function (such as
#'   `dHIV_SI4`). Default is `dHIV_SI4`.
#' @param parms list. List of disease parameters. Default is `disease_params()`.
#'
#' @return data.frame. A data frame of simulated outputs containing:
#'   * `time`: time points of predictions
#'   * state variables (`S`, `I1` to `I4`, `CI`, `CD`)
#'   * `I`: total infected population (`I1 + I2 + I3 + I4`)
#'   * `N`: total population (`S + I`)
#'   * `P`: infected prevalence (`I / N`)
#'
#' @examples
#' simEpidemic()
simEpidemic <- function(
  init = initialize_from_prev(),
  tseq = seq(1976, 2015, by = 1 / 12),
  modFunction = dHIV_SI4,
  parms = disease_params()
) {
  # n.b. a `within` block works within a data frame to calculate new values
  return(
    lsoda(init, tseq, modFunction, parms = parms) |>
      as.data.frame() |>
      within({
        I <- I1 + I2 + I3 + I4
        N <- S + I
        P <- I / N
      })
  )
}

#' @title Sample Epidemic
#'
#' @description
#' Simulates drawing cross-sectional samples of individuals from a simulated
#' epidemic at specified time points, testing them, and calculating sample
#' prevalence alongside binomial confidence intervals.
#'
#' @param simDat data.frame. Simulated epidemic data, typically the output
#'   of `simEpidemic()`. Must contain columns `time` and `P`.
#' @param sampleDates numeric. Vector of dates at which to sample the
#'   population. Default is every 3 years from 1980 to 2010.
#' @param numSamp numeric. Vector indicating the number of individuals
#'   sampled at each date. Default is 80 individuals per date.
#'
#' @return data.frame. A data frame with columns:
#'   * `time`: sample dates
#'   * `numPos`: simulated number of positive cases
#'   * `numSamp`: number of individuals sampled
#'   * `sampPrev`: sample prevalence (`numPos / numSamp`)
#'   * `lci`: lower binomial confidence interval bound
#'   * `uci`: upper binomial confidence interval bound
#'
#' @examples
#' sampleEpidemic(simEpidemic())
sampleEpidemic <- function(
  simDat,
  sampleDates = seq(1980, 2010, by = 3),
  numSamp = rep(80, length(sampleDates)),
  seed = 1
) {
  set.seed(1)
  data_subset <- subset(simDat, time %in% sampleDates)
  if (nrow(data_subset) != length(sampleDates)) {
    warning(
      "The following sampleDates are not present in simDat: ",
      toString(setdiff(sampleDates, simDat$time))
    )
    numSamp <- numSamp[which(sampleDates %in% simDat$time)]
  }

  return(
    within(data_subset, {
      numSamp <- numSamp
      numPos <- rbinom(length(numSamp), numSamp, P)
      sampPrev <- numPos / numSamp
      lci <- mapply(
        function(x, n) binom.test(x, n)$conf.int[1],
        x = numPos,
        n = numSamp
      )
      uci <- mapply(
        function(x, n) binom.test(x, n)$conf.int[2],
        x = numPos,
        n = numSamp
      )
    })[, c("time", "numSamp", "numPos", "sampPrev", "lci", "uci")]
  )
}

## Run system of ODEs for "true" parameter values
# Default model parameters are defined in lines 20-26
trueParms <- disease_params()
# Simulated epidemic (underlying process)
simDat <- simEpidemic(parms = trueParms)

# Simulate *observed* data about the epidemic
myDat <- sampleEpidemic(simDat)

# Plot simulated prevalence and sample data using ggplot2
plot_sim_vs_obs <- ggplot() +
  geom_line(
    data = simDat,
    aes(x = time, y = P),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    data = myDat,
    aes(x = time, y = sampPrev),
    color = "red",
    size = 3
  ) +
  geom_errorbar(
    data = myDat,
    aes(x = time, ymin = lci, ymax = uci),
    color = "red",
    width = 0.5,
    linewidth = 1
  ) +
  scale_y_continuous(limits = c(0, 0.4)) +
  labs(x = NULL, y = "prevalence") +
  theme_classic()

print(plot_sim_vs_obs)

## To start, we need to write a likelihood function that gives the
## probability that a given parameter set (Beta, alpha values) would generate
## the observed data. Remember that we are assuming that there is some true
## underlying epidemic curve that is deterministic and the data we observe
## are only noisy because of sampling/observation error (not because the
## underlying curve is also noisy--i.e. process error--which would be
## particularly likely for epidemics in small populations).

## We assume binomial sampling errors. So we can write the -log-likelihood as
## the probability of observing the observed number positive out of each
## sample if the prevalence is the value generated by a model parameterized
## by a given set of parameters:

#' @title Negative Log-Likelihood for HIV SI4 Model
#'
#' @description
#' Calculates the negative log-likelihood of the observed data given a set of
#' disease parameters. This assumes binomial sampling errors at the observed
#' time points.
#'
#' @param parms list. List of disease parameters. Default is `disease_params()`.
#' @param obsDat data.frame. Observed data containing columns `time`, `numPos`,
#'   and `numSamp`. Default is `myDat`.
#' @param t0 numeric. The starting time of the epidemic simulation. Default
#'   is 1976.
#'
#' @return numeric. The negative log-likelihood value.
#'
#' @examples
#' # log likelihood of the true parameters (which we usually never know)
#' nllikelihood(disease_params())
#' # ... vs some random guess
#' nllikelihood(disease_params(Beta = 3, alpha = 1))
nllikelihood <- function(
  parms = disease_params(),
  obsDat = myDat,
  t0 = 1976
) {
  simDat <- simEpidemic(tseq = c(t0, obsDat$time), parms = parms)[-1, ]
  ## What are the rows from our simulation at which we have observed data?
  nlls <- -dbinom(
    obsDat$numPos,
    obsDat$numSamp,
    prob = simDat$P,
    log = TRUE
  )
  return(sum(nlls))
}

## First look up how optim() works. The more you read through the help
## file the easier this will be!!! In particular make sure you understand that
## the first argument of optim must be the initial values of the parameters to
## be fitted (i.e. Beta & alpha) and that any other parameters to be fixed are
## given as additional arguments (in the help file under "...")
## ?optim

#' @title Substitute and Update Parameters
#'
#' @description
#' Combines fitted and fixed parameters into a single list suitable for the
#' epidemic model. Parameters whose names start with `"log_"` are
#' automatically exponentiated back to their untransformed scale.
#'
#' @param fit.params numeric. Named vector of parameters being fitted.
#' @param fixed.params list. List of fixed parameters, typically the output
#'   of `disease_params()`. Default is `disease_params()`.
#'
#' @return list. A list of all parameters (both fixed and fitted) on their
#'   original (unlogged) scale.
#'
#' @examples
#' guess_params <- c(log_Beta = log(5), log_alpha = log(8))
#' subsParms(guess_params)
subsParms <- function(fit.params, fixed.params = disease_params()) {
  within(fixed.params, {
    loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
    unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]
    for (nm in unloggedParms) {
      assign(nm, as.numeric(fit.params[nm]))
    }
    for (nm in loggedParms) {
      assign(gsub('log_', '', nm), exp(as.numeric(fit.params[nm])))
    }
    rm(nm, loggedParms, unloggedParms)
  })
}

## Make likelihood a function of fixed and fitted parameters.
objFXN <- function(
  fit.params, ## parameters to fit
  fixed.params = disease_params(), ## fixed paramters
  obsDat = myDat
) {
  parms <- subsParms(fit.params, fixed.params)
  nllikelihood(parms, obsDat = obsDat) ## then call likelihood
}
objFXN(guess.params, disease_params())

## Select initial values for fitted parameters from which optimization routine
## will start. If you select bad initial values the algorithm can get stuck on a
## bad set of parameters. You can always try the true values as a starting point
## for this problem, although that's rarely possible in real problems.

init.pars <- c(log_alpha = log(30), log_Beta = log(.1))
## We will start with SANN optimization since it is stochastic and therefore
## less likely to get stuck in a local minima. But then finish with Nelder-Mead
## optimization which is much faster.

###  NOTE: for trace >0 you see more progress report, bigger numbers show more
###  update
trace <- 3

## SANN: This is stochastic, be CAREFUL -- sometimes it gets stuck at local minima
## for unreasonble parameters. If you see this happen, run it again!
optim.vals <- optim(
  par = init.pars,
  objFXN,
  fixed.params = disease_params(),
  obsDat = myDat,
  control = list(trace = trace, maxit = 150),
  method = "SANN"
)
exp(optim.vals$par) # note `log_` no longer applies
trueParms[c('alpha', 'Beta')] |> unlist()

## We feed the last parameters of SANN in as the first values of Nelder-Mead
optim.vals <- optim(
  par = optim.vals$par,
  objFXN,
  fixed.params = disease_params(),
  obsDat = myDat,
  control = list(trace = trace, maxit = 800, reltol = 10^-7),
  method = "Nelder-Mead",
  hessian = T
)
optim.vals
MLEfits <- optim.vals$par
trueParms[c('alpha', 'Beta')]
exp(unname(MLEfits))

log_alpha.fit <- MLEfits["log_alpha"]
log_Beta.fit <- MLEfits["log_Beta"]

## Look at the output of optim. Understand what it means. Did the algorithm
## converge? Look at ?optim to understand it.

# Plot MLE fit time series using ggplot2
fitDat <- simEpidemic(parms = subsParms(optim.vals$par, trueParms))

plot_mle_fit <- plot_sim_vs_obs +
  geom_line(
    data = fitDat,
    aes(x = time, y = P),
    color = "blue",
    linewidth = 1
  )

print(plot_mle_fit)

######################################################################
## Contour plots with the hessian
######################################################################
## The Hessian matrix gives you the curvature of the likelihood function at the
## maximum likelihood estimate (MLE) of the fitted parameters. In other words,
## it tells you the second derivative around MLE, which can be used to
## estimate the covariance variance matrix of the estimator. This estimate of
## the covariance variance matrix is known as the Fisher information matrix
## and can be obtained by inverting the Hessian.

## invert the Hessian, to estimate the covar-var matrix of parameter estimates
fisherInfMatrix <- solve(optim.vals$hessian)

# Extract ellipse coordinates
ellipse_coords <- as.data.frame(
  exp(ellipse(fisherInfMatrix, centre = MLEfits, level = 0.95))
)
colnames(ellipse_coords) <- c("alpha", "Beta")

# True parameters data frame
true_df <- data.frame(alpha = trueParms$alpha, Beta = trueParms$Beta)

# MLE parameters data frame
mle_df <- data.frame(
  alpha = exp(log_alpha.fit),
  Beta = exp(log_Beta.fit)
)

# Plot contours using ggplot2
ggplot() +
  geom_point(
    data = true_df,
    aes(x = alpha, y = Beta, color = "truth"),
    size = 4
  ) +
  geom_point(
    data = mle_df,
    aes(x = alpha, y = Beta, color = "MLE"),
    size = 4
  ) +
  geom_path(
    data = ellipse_coords,
    aes(x = alpha, y = Beta, color = "95% Confidence Region"),
    linewidth = 1
  ) +
  scale_x_log10(limits = c(2, 15)) +
  scale_y_log10(limits = c(0.5, 2)) +
  scale_color_manual(
    name = NULL,
    values = c(
      "truth" = "red",
      "MLE" = "black",
      "95% Confidence Region" = "black"
    ),
    breaks = c("truth", "MLE", "95% Confidence Region"),
    guide = guide_legend(
      override.aes = list(
        shape = c(16, 16, NA),
        linetype = c("blank", "blank", "solid")
      )
    )
  ) +
  labs(
    x = expression(alpha),
    y = expression(beta),
    title = "-log(likelihood) contours"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

######################################################################
## Contour plots with likelihood profiles
######################################################################
## With all other parameters fixed to their initial values, lets look at a
## contour likelihood plot over Beta and alpha.  To do this we write wrapper
## functions of log_Beta and log_alpha to feed to outer() and then
## contour(). This is confusing so make sure you understand every function.

## This function simply takes values log_Beta and log_alpha and feeds them into
## objFXN above as a single variable called logpars, you'll see why this is
## useful below.
objXalpha_Beta <- Vectorize(
  function(
    alpha,
    Beta,
    fixed.params = disease_params(),
    browse = F
  ) {
    objFXN(
      fit.params = c(log_alpha = log(alpha), log_Beta = log(Beta)),
      fixed.params = fixed.params
    )
  },
  list("alpha", "Beta")
)


## Now instead of giving a single argument on the log scale we give 2
## on the untransformed scale.
objFXN(c(log_alpha = log(1 / 5), log_Beta = log(25)))
objXalpha_Beta(1 / 5, 25)

## Now we use the R function outer() to evaluate objXalpha_Beta() over a grid
## of {alpha, Beta} combinations. This can take a long time because we have to do
## res^2 evaluations of nllikelihood(), and recall that each time we do this we are
## running lsoda() inside simEpidemic()

## Grid resolution resXres, increasing it makes contours smoother but takes a lot longer
res <- 15

## Now create a sequence of alpha values for the grid
alpha.seq <- exp(seq(log_alpha.fit - 1, log_alpha.fit + 1, l = res))
alpha.seq

## Now create a sequence of Beta values for the grid
Beta.seq <- exp(seq(log_Beta.fit - 1, log_Beta.fit + 1, l = res))
Beta.seq

## The function outer() now evaluates objXalpha_BetaVEC on this grid. ?outer
mat <- outer(alpha.seq, Beta.seq, objXalpha_Beta) # this can take a long time

## Make a contour plot that shows the confidence regions in red.  Likelihood
## Ratio Test confidence regions use the chi squared distribution cutoff with
## degrees of freedom 2 (2 parameters)
ml.val <- optim.vals$value
conf.cutoff <- ml.val + qchisq(.95, 2) / 2

# Convert grid matrix to data frame
grid_df <- expand.grid(alpha = alpha.seq, Beta = Beta.seq)
grid_df$z <- as.vector(mat)

# Plot likelihood contours using ggplot2
ggplot() +
  geom_contour_filled(
    data = grid_df,
    aes(x = alpha, y = Beta, z = z),
    breaks = seq(min(mat), max(mat), length.out = 20)
  ) +
  geom_contour(
    data = grid_df,
    aes(
      x = alpha,
      y = Beta,
      z = z,
      color = "95% contour (profile likelihood)"
    ),
    breaks = conf.cutoff,
    linewidth = 1
  ) +
  geom_path(
    data = ellipse_coords,
    aes(
      x = alpha,
      y = Beta,
      color = "95% contour (Fisher information matrix)"
    ),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(
    data = true_df,
    aes(x = alpha, y = Beta, color = "truth"),
    size = 3
  ) +
  geom_point(
    data = mle_df,
    aes(x = alpha, y = Beta, color = "MLE"),
    size = 3
  ) +
  scale_x_log10(limits = c(3, 15)) +
  scale_y_log10(limits = c(0.5, 2)) +
  scale_fill_viridis_d(name = "Negative Log-Likelihood") +
  scale_color_manual(
    name = NULL,
    values = c(
      "truth" = "red",
      "MLE" = "black",
      "95% contour (profile likelihood)" = "black",
      "95% contour (Fisher information matrix)" = "black"
    ),
    breaks = c(
      "truth",
      "MLE",
      "95% contour (profile likelihood)",
      "95% contour (Fisher information matrix)"
    ),
    guide = guide_legend(
      override.aes = list(
        shape = c(16, 16, NA, NA),
        linetype = c("blank", "blank", "solid", "dashed")
      )
    )
  ) +
  labs(
    x = expression(alpha),
    y = expression(beta),
    title = "-log(likelihood) contours"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

######################################################
# EXERCISES
#
# 1. Evaluate how the MLE estimate and confidence interval changes when
# there is more or less observational noise. (Hint: Change the number of
# people sampled during each surveillance study in the sampleEpidemic
# function.)
#
# 2. Evaluate how the MLE estimate and confidence interval changes when the
# frequency of surveillance changes.
#
# 3. Evaluate how the MLE estimate and confidence changes when the model used
# to conduct the estimation differs from the 'true' model. Specifically,
# see what happens if you only use two boxcars in the model, rather than four.
# (Hint: You will need to make a new function for the model used to
# calculate the likelihood)
