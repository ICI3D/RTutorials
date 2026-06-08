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

makeActiveBinding("???", \() stop("Replace Me!", call. = FALSE), parent.frame())

library(ggplot2)
set.seed(42)

#' Breakout 1: Recall the Binomial

true_prevalence <- 0.3

#' Recall the correct distribution function

dMystery <- function(observed_positive, observation_size) {
  return(`???`)
}

#' Next, simulate many experiments of sample_size = 100

random_samples <- data.frame(
  sample_id = seq_len(1000),
  observed_positive = `???` # some sampling procedure
)

#' Finally, use this plotting function to look at your results.

distro_plot <- function(FUN, samples) {
  ggplot(samples) +
    aes(x = observed_positive) +
    geom_histogram(
      aes(y = after_stat(density), fill = "sample"),
      binwidth = 1L,
      center = 0
    ) +
    geom_function(
      mapping = aes(color = "analytical"),
      fun = FUN,
      args = list(observed_size = 100)
    ) +
    scale_x_continuous(
      "Observed Positive",
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    scale_y_continuous("Density") +
    scale_color_discrete("Feature", aesthetics = c("color", "fill")) +
    theme_minimal() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(1, 1),
      legend.justification.inside = c(1, 1)
    )
}

distro_plot(dMystery, random_samples)
