#' Introduction to Inference
#' [MMED](https://www.ici3d.org/MMED)
#' Steve Bellan 2012, 2015
#' Carl Pearson 2025, 2026
#' Claire Perrin Smith 2026
#'
#' By the end of this tutorial you should be able to:
#'  * Write a likelihood function for binomially distributed data
#'  * Explain the Metropolis-Hastings algorithm
#'  * Explain how proposal distribution affects MCMC convergence
#'  * Assess MCMC convergence with the Gelman-Rubin diagnostic
#'    and trace plots
#'
#' -----------------------------------------------------------------------------

################################################################################
# STEP 0: SETUP ################################################################
################################################################################

#' Setup the `???` object you need replace
makeActiveBinding(
  "???",
  \(msg) {
    stop(sprintf("Error: you need to %s!", msg), call. = FALSE)
  },
  parent.frame()
)

#' Load packages
library(dplyr)
library(ggplot2)
library(patchwork)

#' Setup Reference Plotting Colors
plot_colors <- c(
  posterior = "#00b300",
  likelihood = "#cc00ff",
  prior = "#ff9933",
  denom = "#0099ff",
  analytical = "#000000",
  MLE = "dodgerblue",
  observed = "firebrick",
  other_samples = "lightgrey"
)

#' Setup Reference Values
true_prevalence <- 0.3
sample_size <- 100L
observation <- 28L

set.seed(42)

################################################################################
# STEP 1: RECALL THE BINOMIAL ##################################################
################################################################################

dMystery <- function(observed_positive) {
  return(dbinom(
    x = observed_positive,
    size = sample_size,
    prob = true_prevalence
  ))
}

#' First, let's simulate many experiments of sample_size = 100

set.seed(42)
random_samples <- data.frame(
  sample_id = seq_len(1000),
  observed_positive = rbinom(
    n = 1000,
    size = sample_size,
    prob = true_prevalence
  ) # some sampling procedure
)

#' Finally, use this plotting function to look at your results.

distro_plot <- function(FUN, samples) {
  ggplot(samples) +
    aes(x = observed_positive) +
    geom_histogram(
      aes(
        y = after_stat(density),
        fill = after_stat(case_when(
          x == true_prevalence * sample_size ~ "MLE",
          x == observation ~ "observed",
          .default = "other samples"
        ))
      ),
      binwidth = 1L,
      center = 0
    ) +
    stat_function(
      mapping = aes(color = "analytical"),
      fun = FUN,
      geom = "step",
      n = sample_size + 1,
      direction = "mid"
    ) +
    scale_x_continuous(
      "Observed Positive",
      limits = c(0, sample_size),
      breaks = seq(0, sample_size, 10),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous("Density") +
    scale_color_manual(
      NULL,
      values = plot_colors,
      aesthetics = c("color", "fill")
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 24) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification.inside = c(1, 1),
      plot.margin = margin(r = 20)
    )
}

ggsave(
  "testing/binomial_distro.png",
  distro_plot(dMystery, random_samples),
  height = 5,
  width = 10,
  dpi = 1200,
  bg = "transparent"
)

# 2. Recall Likelihood

# Recall the correct likelihood function

lMystery <- function(latent_probability, log = FALSE) {
  return(dbinom(
    x = observation,
    size = sample_size,
    prob = latent_probability,
    log = log
  ))
}

# Using this plotting function, inspect your likelihood function

likelihood_plot <- function(
  FUN,
  type = "likelihood",
  x_label = "Actual Prevalence",
  y_label = NULL,
  title = NULL,
  show_legend = FALSE
) {
  p <- ggplot() +
    geom_function(
      mapping = aes(color = type),
      fun = FUN,
      linewidth = 1,
      n = 1001
    ) +
    scale_x_continuous(
      x_label,
      limits = c(0, 1),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(y_label) +
    scale_color_manual(
      NULL,
      values = plot_colors
    ) +
    coord_cartesian(clip = "off") +
    labs(title = title) +
    theme_minimal(base_size = 24) +
    theme(
      plot.margin = margin(r = 20)
    )

  if (show_legend) {
    p <- p +
      theme(
        legend.position = "inside",
        legend.position.inside = c(1, 1),
        legend.justification.inside = c(1, 1)
      )
  } else {
    p <- p + theme(legend.position = "none")
  }
  return(p)
}

# draw and save likelhood function

likelihood_curve_plot <- likelihood_plot(lMystery)

ggsave(
  "testing/likelihood.png",
  likelihood_curve_plot,
  height = 5,
  width = 10,
  dpi = 1200,
  bg = "transparent"
)

# 3. Combined Likelihood plot, prior plot, and posterior plot

# introduce a prior which takes no position on the prevalence

priorMystery <- function(latent_probability) {
  return(dunif(latent_probability, min = 0, max = 1))
}

posterior_fun <- function(priorMystery, lMystery) {
  function(latent_probability) {
    return(lMystery(latent_probability) * priorMystery(latent_probability))
  }
}

# use a plotting function to display the prior, likelihood, and posterior

compare_plot <- function(prior_fun, likelihood_fun) {
  p1 <- likelihood_plot(
    prior_fun,
    type = "prior",
    x_label = NULL,
    show_legend = FALSE
  )

  p2 <- likelihood_plot(
    likelihood_fun,
    type = "likelihood",
    x_label = NULL,
    show_legend = FALSE
  )

  p3 <- likelihood_plot(
    posterior_fun(prior_fun, likelihood_fun),
    type = "posterior",
    x_label = "Prevalence",
    show_legend = FALSE
  )

  return(p1 / p2 / p3)
}

combined_plot <- compare_plot(priorMystery, lMystery)

ggsave(
  "testing/combined_plots.png",
  combined_plot,
  height = 5,
  width = 10,
  dpi = 1200,
  bg = "transparent"
)

# 4. MCMC in practice

# write a proposal function, which moves symmetrically
# about the current value

proposalMystery <- function(initial_prevalence) {}

#
