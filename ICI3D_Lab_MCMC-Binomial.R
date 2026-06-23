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
library(tidyr)
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

#' define some reusable scales for plotting
scale_x_observed <- function(
  sample_size,
  name = "Observed Positive",
  breaks = seq(0, sample_size, by = 10),
  expand = expansion(mult = 0),
  limits = c(0, sample_size),
  ...
) {
  args <- as.list(environment())
  args$sample_size <- NULL
  do.call(scale_x_continuous, args)
}

scale_x_prevalence <- function(
  name = "Prevalence",
  limits = c(0, 1),
  expand = expansion(mult = 0),
  ...
) {
  do.call(scale_x_continuous, as.list(environment()))
}

scale_y_density <- function(name = "Density", ...) {
  do.call(scale_y_continuous, as.list(environment()))
}

scale_color_views <- function(
  name = NULL,
  values = plot_colors,
  aesthetics = c("color", "fill"),
  ...
) {
  do.call(scale_color_manual, as.list(environment()))
}

scale_linetype_views <- function(
  name = NULL,
  values = c(
    "Likelihood" = "solid",
    "Binomial Test CI" = "dotted",
    "Likelihood Ratio CI" = "dashed"
  ),
  ...
) {
  do.call(scale_linetype_manual, as.list(environment()))
}

theme_legend <- function(
  position = c(1, 1),
  justification = c(1, 1),
  ...
) {
  theme(
    legend.position = "inside",
    legend.position.inside = position,
    legend.justification.inside = justification,
    ...
  )
}

#' @title Likelihood Ratio Tester
#'
#' @param par a comparison parameter value
#' @param llFun the log-likelihood function, f(par)
#' @param llmle the log-likelihood of the MLE
#'
#'
likelihood_ratio <- function(
  par,
  llFun,
  llmle
) {
  2 * (llmle - llFun(par))
}

#' @title Solve Binomial Confidence Interval Limits
#'
#' @description Finds the confidence interval bounds on either side of the MLE for a binomial proportion
#' where the likelihood ratio statistic equals the target critical value from the chi-squared distribution.
#'
#' @param observed_positive integer; number of observed positive outcomes (successes)
#' @param sample_size integer; total number of trials
#' @param target_ci numeric; the target critical value from qchisq (e.g. qchisq(0.95, df = 1))
#'
#' @return A named numeric vector with the 'lower' and 'upper' confidence limits.
#' @examples
#' lr_binomial_ci(observed_positive = 23, sample_size = 100, target_ci = 0.95)
lr_binomial_ci <- function(observed_positive, sample_size, target_ci) {
  p_mle <- observed_positive / sample_size
  target_value <- qchisq(target_ci, 1)
  llmle <- dbinom(
    observed_positive,
    size = sample_size,
    prob = p_mle,
    log = TRUE
  )

  # Objective function to minimize: we want the absolute likelihood ratio to match target_ci
  obj_fun <- function(p) {
    (likelihood_ratio(
      par = p,
      llFun = \(p) {
        dbinom(observed_positive, size = sample_size, prob = p, log = TRUE)
      },
      llmle = llmle
    ) -
      target_value)^2
  }

  # Solve on the left side of the MLE (if p_mle > 0)
  if (p_mle == 0) {
    lower_bound <- 0
  } else {
    lower_bound <- optimize(obj_fun, interval = c(0, p_mle))$minimum
  }

  # Solve on the right side of the MLE (if p_mle < 1)
  if (p_mle == 1) {
    upper_bound <- 1
  } else {
    upper_bound <- optimize(obj_fun, interval = c(p_mle, 1))$minimum
  }

  return(c(lower_bound, upper_bound))
}

#' @examples
#' binomial_ci(observed_positive = 23, sample_size = 100, target_ci = 0.95)
binomial_ci <- function(observed_positive, sample_size, target_ci) {
  as.numeric(
    binom.test(
      observed_positive,
      sample_size,
      conf.level = target_ci
    )$conf.int
  )
}

################################################################################
# STEP 1: RECALL THE BINOMIAL ##################################################
################################################################################

#' First, let's simulate many experiments of sample_size = 100

set.seed(42)
random_samples <- data.frame(
  sample_id = seq_len(1000),
  observed_positive = `???`("simulate binomial random samples")
)

#' What is the probability of observing `observed_positive` in a sample of size
#' `sample_size`, given a true prevalence of `true_prevalence`?
dMystery <- function(
  observed_positive,
  sample_size,
  true_prevalence,
  log = FALSE
) {
  `???`("What is the probability distribution for observing this event?")
}


#' @title A Distribution Plotting Function
#'
#' @param dSomething a distribution function, f(observed_position, sample_size, true_prevalence)
#' @param samples A data frame with `observed_positive` column
#' @param true_prevalence a proportion, number in [0, 1]; the true prevalence
#' @param sample_size The sample size
#' @param observation A particular value observed positive
#'
#' @return A ggplot object showing the distribution of the observed positives
distro_plot <- function(
  dSomething,
  samples,
  true_prevalence,
  sample_size,
  observation
) {
  ggplot(samples) +
    aes(x = observed_positive) +
    geom_histogram(
      aes(
        y = after_stat(density),
        fill = after_stat(case_when(
          x == round(true_prevalence * sample_size) ~ "MLE",
          x == observation ~ "observed",
          .default = "other samples"
        ))
      ),
      binwidth = 1L,
      center = 0
    ) +
    stat_function(
      mapping = aes(color = "analytical"),
      fun = \(x) dSomething(x, sample_size, true_prevalence),
      geom = "step",
      n = sample_size + 1,
      direction = "mid"
    ) +
    scale_x_observed(sample_size = sample_size) +
    scale_y_density() +
    scale_color_views() +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 24) +
    theme_legend(plot.margin = margin(r = 20))
}

ggsave(
  "testing/binomial_distro.png",
  distro_plot(
    dMystery,
    random_samples,
    true_prevalence,
    sample_size,
    observation
  ),
  height = 4,
  width = 12,
  dpi = 1200,
  bg = "transparent"
)

#' #############################################################################
#' # STEP 2: RECALL LIKELIHOOD #################################################
#' #############################################################################

#' What is the likelihood of `true_prevalence` given `observed_positive` in a
#' sample of size `sample_size`?
lMystery <- function(
  true_prevalence,
  observed_positive = observation,
  sample_size = 100L,
  log = FALSE
) {
  `???`("calculate the likelihood of the prevalence")
}

#' @title A Likelihood Plotting Function
#'
#' @param lSomething a likelihood function
#' @param type a character string; the name of the likelihood function
#' @param observed_positive integer; the number of observed positive cases
#' @param sample_size integer; the sample size
#' @param x_label a character string; the label for the x-axis
#' @param y_label a character string; the label for the y-axis
#' @param title a character string; the title of the plot
#' @param show_legend a logical; whether to show the legend
#' @param lr_ci numeric vector of length 2; optionally, likelihood ratio CI limits
#' @param binom_ci numeric vector of length 2; optionally, binom.test CI limits
#' @param what a character string; alias for type
#'
#' @return A ggplot object showing the likelihood function
likelihood_plot <- function(
  lSomething,
  type = "likelihood",
  lr_ci = NULL,
  binom_ci = NULL,
  what = type
) {
  p <- ggplot() +
    geom_function(
      mapping = aes(color = what, linetype = "Likelihood"),
      fun = lSomething,
      n = 1001
    ) +
    scale_x_prevalence() +
    scale_y_density() +
    scale_color_views() +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 24)

  # Optionally add likelihood ratio CI annotations
  if (!is.null(lr_ci)) {
    y_lr <- mean(lSomething(lr_ci))
    p <- p +
      geom_segment(aes(
        x = lr_ci[1],
        xend = lr_ci[2],
        y = y_lr,
        yend = y_lr,
        linetype = "Likelihood Ratio CI"
      ))
  }

  # Optionally add binom.test CI annotations
  if (!is.null(binom_ci)) {
    y_bt <- mean(lSomething(binom_ci))
    p <- p +
      geom_segment(aes(
        x = binom_ci[1],
        xend = binom_ci[2],
        y = y_bt,
        yend = y_bt,
        linetype = "Binomial Test CI"
      ))
  }

  if (!is.null(binom_ci) || !is.null(lr_ci)) {
    p <- p + scale_linetype_views()
  }

  return(p)
}

# draw and save likelhood function

# Calculate CI limits for plotting/annotation
lr_limits <- lr_binomial_ci(
  observed_positive = observation,
  sample_size = sample_size,
  target_ci = 0.95
)

binom_limits <- binomial_ci(
  observed_positive = observation,
  sample_size = sample_size,
  target_ci = 0.95
)

likelihood_curve_plot <- likelihood_plot(
  lMystery,
  lr_ci = lr_limits,
  binom_ci = binom_limits
)

ggsave(
  "testing/likelihood.png",
  likelihood_curve_plot + theme_legend(plot.margin = margin(r = 20)),
  height = 4,
  width = 12,
  dpi = 1200,
  bg = "transparent"
)

#' #############################################################################
#' # STEP 3: COMBINED LIKELIHOOD, PRIOR, AND POSTERIOR PLOTS #####################
#' #############################################################################

#' Introduce a prior which takes no position on the prevalence
priorMystery <- function(latent_probability) {
  `???`("calculate the prior probability")
}

#' Define a function to calculate the posterior probability
posterior_fun <- function(
  priorMystery,
  lMystery,
  observed_positive = observation,
  sample_size = 100L
) {
  function(latent_probability) {
    return(
      lMystery(latent_probability, observed_positive, sample_size) *
        priorMystery(latent_probability)
    )
  }
}

# use a plotting function to display the prior, likelihood, and posterior

compare_plot <- function(
  orig_plot,
  prior_fun,
  likelihood_fun,
  observed_positive = observation,
  sample_size = 100L
) {
  (orig_plot /
    likelihood_plot(
      prior_fun,
      type = "prior"
    ) /
    likelihood_plot(
      posterior_fun(prior_fun, likelihood_fun, observed_positive, sample_size),
      type = "posterior"
    )) +
    plot_layout(guides = "collect")
}

combined_plot <- compare_plot(
  likelihood_plot(lMystery),
  priorMystery,
  lMystery
) &
  theme(legend.position = "none", plot.margin = margin(r = 20))

ggsave(
  "testing/combined_plots.png",
  combined_plot,
  height = 8,
  width = 12,
  dpi = 1200,
  bg = "transparent"
)

################################################################################
# STEP 4: MCMC in practice #####################################################
################################################################################

# write a proposal function, which moves symmetrically
# about the current value

proposalMystery <- function(initial_prevalence, proposal_width = 0.1) {
  `???`("propose a new prevalence value")
}

# now let's iteratively use that proposal

# Initialize MCMC variables
set.seed(42)
max_steps <- 500
current_prev <- 0.75 # reasonable starting point
post_fun <- posterior_fun(priorMystery, lMystery, observation, sample_size)

# Safe posterior function that returns 0 if out of bounds to avoid warnings
safe_posterior <- function(p) {
  if (p < 0 || p > 1) {
    return(0)
  }
  return(post_fun(p))
}

make_mcmc_plot <- function(history_df, max_steps = 500, title = "", ci = NULL) {
  # Define bins
  bin_width <- 0.01
  bin_breaks <- seq(0, 1, by = bin_width)
  bin_centers <- seq(bin_width / 2, 1 - bin_width / 2, by = bin_width)

  # Bin history
  history_df$bin_center <- (floor(history_df$prevalence * 100) + 0.5) / 100

  # Calculate counts and pad empty bins
  bin_counts <- history_df |>
    group_by(bin_center) |>
    summarise(count = n(), .groups = 'drop') |>
    complete(bin_center = bin_centers, fill = list(count = 0))

  is_multichain <- "chain" %in% names(history_df)

  # Determine highlight color status for the final step
  last_idx <- nrow(history_df)
  if (is_multichain) {
    bin_counts$status <- "neutral"
  } else {
    current_prev <- history_df$prevalence[last_idx]
    current_bin <- (floor(current_prev * 100) + 0.5) / 100
    accepted_step <- history_df$accepted[last_idx]

    bin_counts <- bin_counts |>
      mutate(
        status = case_when(
          bin_center == current_bin & accepted_step ~ "accepted",
          bin_center == current_bin & !accepted_step ~ "rejected",
          TRUE ~ "neutral"
        )
      )
  }

  # Plot the posterior histogram
  p_step <- ggplot(bin_counts) +
    aes(x = bin_center, y = count / last_idx, fill = status) +
    geom_col(width = bin_width) +
    scale_fill_manual(
      values = c(
        "accepted" = "#2ECC71", # green
        "rejected" = "#E74C3C", # red
        "neutral" = "lightgrey"
      ),
      guide = "none"
    ) +
    scale_x_continuous(
      name = "Prevalence",
      limits = c(0, 1),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(name = "Density") +
    theme_minimal(base_size = 24) +
    theme(plot.margin = margin(r = 20))

  # Trace plot
  if (is_multichain) {
    p_trace <- ggplot(history_df) +
      aes(x = step, y = prevalence, color = factor(chain)) +
      geom_line(linewidth = 0.8) +
      scale_x_continuous(
        name = "Step",
        limits = c(0, max_steps),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        name = "Prevalence",
        limits = c(0, 1)
      ) +
      theme_minimal(base_size = 24) +
      theme(
        plot.margin = margin(r = 20),
        legend.position = "none"
      )
  } else {
    p_trace <- ggplot(history_df) +
      aes(x = step, y = prevalence) +
      geom_line(color = "#354B5E", linewidth = 0.8) +
      geom_point(aes(color = accepted), size = 2.5) +
      scale_color_manual(
        values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"),
        guide = "none"
      ) +
      scale_x_continuous(
        name = "Step",
        limits = c(0, max_steps),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        name = "Prevalence",
        limits = c(0, 1)
      ) +
      theme_minimal(base_size = 24) +
      theme(plot.margin = margin(r = 20))
  }

  # Optionally add median and CI annotations
  if (!is.null(ci)) {
    lowerq <- 1 - (1 - ci) / 2
    upperq <- (1 - ci) / 2
    qs <- quantile(
      history_df$prevalence,
      c(lowerq, 0.5, upperq),
      na.rm = TRUE
    )

    p_step <- p_step +
      geom_vline(
        xintercept = c(qs[1], qs[3]),
        linetype = "dashed",
        color = "darkblue",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = qs[2],
        linetype = "solid",
        color = "darkblue",
        linewidth = 1.2
      ) +
      geom_point(
        data = data.frame(x = qs[2], y = 0),
        aes(x = x, y = y),
        color = "darkblue",
        size = 5,
        shape = 18,
        inherit.aes = FALSE
      )

    p_trace <- p_trace +
      geom_hline(
        yintercept = c(qs[1], qs[3]),
        linetype = "dashed",
        color = "darkblue",
        linewidth = 1
      ) +
      geom_hline(
        yintercept = qs[2],
        linetype = "solid",
        color = "darkblue",
        linewidth = 1.2
      ) +
      geom_point(
        data = data.frame(x = max_steps, y = qs[2]),
        aes(x = x, y = y),
        color = "darkblue",
        size = 5,
        shape = 18,
        inherit.aes = FALSE
      )
  }

  # Return stacked plot (vertical combination)
  return((p_step / p_trace) + plot_annotation(title = title))
}

run_mcmc_sampling <- function(
  start_prev = 0.75,
  max_steps = 500,
  proposal_width = 0.1,
  post_fun = post_fun,
  mode = "single"
) {
  current_prev <- start_prev
  current_post <- safe_posterior(current_prev)

  # Accumulate history
  history_df <- list(data.frame(
    step = 0,
    prevalence = current_prev,
    accepted = TRUE,
    bin_center = (floor(current_prev * 100) + 0.5) / 100
  ))

  step_counter <- 1

  while (step_counter < max_steps) {
    # 1. Propose new value
    proposal_prev <- proposalMystery(
      current_prev,
      proposal_width = proposal_width
    )

    # 2. Calculate safe posterior densities
    proposal_post <- safe_posterior(proposal_prev)

    # 3. Calculate Metropolis-Hastings acceptance ratio
    if (current_post == 0) {
      metro_hastings_ratio <- 1
    } else {
      metro_hastings_ratio <- proposal_post / current_post
    }

    # 4. Accept or reject
    accepted_step <- FALSE
    if (runif(1) < metro_hastings_ratio) {
      current_prev <- proposal_prev
      current_post <- proposal_post
      accepted_step <- TRUE
    }

    # Increment step and record
    step_counter <- step_counter + 1
    history_df[[length(history_df) + 1]] <- data.frame(
      step = step_counter,
      prevalence = current_prev,
      accepted = accepted_step,
      bin_center = (floor(current_prev * 100) + 0.5) / 100
    )

    # Prompt the user for decision
    if (interactive() && mode != "continue") {
      p_combined <- make_mcmc_plot(
        do.call(rbind, history_df),
        max_steps = max_steps,
        title = paste("Interactive MCMC: Step", step_counter, "of", max_steps)
      )
      print(p_combined)
      ans <- readline(
        "Press [Enter] for next step, 'c' to run remaining to 500, or 'q' to quit: "
      )
      ans <- tolower(trimws(ans))
      if (ans == "q") {
        break
      } else if (ans == "c") {
        mode <- "continue"
      }
    }
  }

  return(do.call(rbind, history_df))
}

# Apply MCMC with the default/standard proposal width (0.1)
chain_standard <- run_mcmc_sampling(
  start_prev = 0.75,
  max_steps = max_steps,
  proposal_width = 0.1,
  post_fun = post_fun,
  mode = "single"
)

# Apply MCMC with a narrower proposal width (0.02)
chain_narrow <- run_mcmc_sampling(
  start_prev = 0.75,
  max_steps = max_steps,
  proposal_width = 0.02,
  post_fun = post_fun,
  mode = "continue"
)

# Apply MCMC with a wider proposal width (0.4)
chain_wide <- run_mcmc_sampling(
  start_prev = 0.75,
  max_steps = max_steps,
  proposal_width = 0.4,
  post_fun = post_fun,
  mode = "continue"
)

# Save widescreen plots for slides (16:9 ratio)
ggsave(
  "testing/mcmc_standard.png",
  make_mcmc_plot(
    chain_standard,
    max_steps = max_steps,
    title = "Standard Proposal (width = 0.1)",
    ci = 0.95
  ),
  width = 12,
  height = 6.75,
  dpi = 1200,
  bg = "transparent"
)

ggsave(
  "testing/mcmc_narrow.png",
  make_mcmc_plot(
    chain_narrow,
    max_steps = max_steps,
    title = "Narrow Proposal (width = 0.02)",
    ci = 0.95
  ),
  width = 12,
  height = 6.75,
  dpi = 1200,
  bg = "transparent"
)

ggsave(
  "testing/mcmc_wide.png",
  make_mcmc_plot(
    chain_wide,
    max_steps = max_steps,
    title = "Wide Proposal (width = 0.4)",
    ci = 0.95
  ),
  width = 12,
  height = 6.75,
  dpi = 1200,
  bg = "transparent"
)

# Run 4 chains starting from different points with standard proposal width (0.1)
start_points <- c(0.1, 0.4, 0.7, 0.9)
chains_list <- list()
for (i in seq_along(start_points)) {
  chain_data <- run_mcmc_sampling(
    start_prev = start_points[i],
    max_steps = max_steps,
    proposal_width = 0.1,
    post_fun = post_fun,
    mode = "continue"
  )
  chain_data$chain <- paste("Chain", i)
  chains_list[[i]] <- chain_data
}
chains_combined <- do.call(rbind, chains_list)

ggsave(
  "testing/mcmc_multichain.png",
  make_mcmc_plot(
    chains_combined,
    max_steps = max_steps,
    title = "4 Chains from Different Starting Points (width = 0.1)",
    ci = 0.95
  ),
  width = 12,
  height = 6.75,
  dpi = 1200,
  bg = "transparent"
)
