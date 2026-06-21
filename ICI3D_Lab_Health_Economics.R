
## Lab: Health economics and dynamical modelling 

#####################################################################
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org
##
## Attribution: Mmamapudi Kubjane (2026)
##             Adapted from ODE models lab: Juliet R.C. Pulliam; Cari van Schalkwyk
## 
## ICI3D_Lab_ODEmodels.R
##
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)
##
#####################################################################
##
##    In this lab, we will build on the ODE Lab (SIR model):
##    To include a vaccination intervention
##    The vaccine has 100% efficacy — those who receive it are fully protected
##    Define and simulate alternative intervention scenarios
##    Estimate the epidemiological impact of vaccination on disease incidence (I)
##    Attach costs to key health outcomes and interventions
##    Calculate and interpret the Incremental Cost-Effectiveness Ratio (ICER)
##  
#####################################################################

# Install packages if haven't got them
# install.packages("tidyverse")
# install.packages("deSolve")
# install.packages("reshape2")
 
# Load libraries 
library(tidyverse)   
library(deSolve)
library(reshape2)

# ------------------------------------------------------------------------------
# Specify model scenarios 
# ------------------------------------------------------------------------------
ScenarioList <- c(
  "Baseline (no vaccine)",
  "Intervention (vaccine)"
)

# ------------------------------------------------------------------------------
# Specify cost parameters 
# ------------------------------------------------------------------------------
cost_vaccine   <- 10    # USD, vaccine course per person
cost_treatment <- 200   # USD, treatment course per case
#   Note: For simplicity test/diagnosis cost is bundled into cost_treatment;
#   only individuals who are diagnosed and treated incur this cost.
#   Treatment uptake: proportion of incident cases that reach treatment.
#   Accounts for lost to follow-up, non-diagnosis, refusal, incomplete treatment.
#   treatment_uptake = 1 - lost individuals 
#   
treatment_uptake <- 0.85   # 85% treated, 15% lost individuals 

# ------------------------------------------------------------------------------
# Specify intervention (vaccine) parameters 
# ------------------------------------------------------------------------------
vaccine_coverage <- 0.70   # Proportion of susceptible offered vaccine
vaccine_efficacy <- 1.00   # 100% protection in those vaccinated
# Individuals not vaccinated remain in S, fully susceptible.

# ------------------------------------------------------------------------------
# SIR Differential equations as in ODE lab (re-review if have forgotten)
# ------------------------------------------------------------------------------
sir_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N  <- S + I + R
    dS <- -beta * S * (I / N)           
    dI <-  beta * S * (I / N) - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

# ------------------------------------------------------------------------------
# Epidemiological parameters (apply to both scenarios)
# ------------------------------------------------------------------------------
parameters_values <- c(
  beta  = 0.4,   # Infectious contact rate (per person per day)
  gamma = 0.5    # Recovery rate (per day)
)

# ------------------------------------------------------------------------------
#  Set initial population
# ------------------------------------------------------------------------------
N_total <- 1000
I0      <- 10
S0_base <- N_total - I0

time_values <- seq(0, 120, by = 0.25)   # 120-day epidemic horizon

# ------------------------------------------------------------------------------
# Run Baseline: No vaccine
# ------------------------------------------------------------------------------
sir_baseline <- as.data.frame(ode(
  y     = c(S = S0_base, I = I0, R = 0),
  times = time_values,
  func  = sir_equations,
  parms = parameters_values
))
sir_baseline$Scenario <- "Baseline (no vaccine)"

# ------------------------------------------------------------------------------
# Run Intervention: Vaccine applied before epidemic
# ------------------------------------------------------------------------------
n_offered   <- round(vaccine_coverage * S0_base)
n_protected <- round(n_offered * vaccine_efficacy)
S0_vax      <- S0_base - n_protected
R0_vax      <- n_protected


sir_vaccine <- as.data.frame(ode(
  y     = c(S = S0_vax, I = I0, R = R0_vax),
  times = time_values,
  func  = sir_equations,
  parms = parameters_values
))
sir_vaccine$Scenario <- "Intervention (vaccine)"

# ------------------------------------------------------------------------------
# Compute incidence (new cases per time step) for each scenario
# ------------------------------------------------------------------------------
add_incidence <- function(df) {
  inc <- c(0, -diff(df$S))
  inc[inc < 0] <- 0   # suppress ODE numerical noise
  df$new_cases <- inc
  df
}

# view simulations over time
sir_baseline <- add_incidence(sir_baseline)
view (sir_baseline)

sir_vaccine  <- add_incidence(sir_vaccine)
view (sir_vaccine)

# ------------------------------------------------------------------------------
# Sum total impact in both scenarios 
# ------------------------------------------------------------------------------

TotalImpact <- data.frame(
  Scenario = c("Baseline (no vaccine)", "Intervention (vaccine)"),
  TotalCases = c(
    sum(sir_baseline$new_cases, na.rm = TRUE),
    sum(sir_vaccine$new_cases, na.rm = TRUE)
  )
)

# ------------------------------------------------------------------------------
# Sum total cost in both scenarios
# ------------------------------------------------------------------------------

treated_baseline <- sum(sir_baseline$new_cases, na.rm = TRUE) * treatment_uptake
treated_vaccine  <- sum(sir_vaccine$new_cases, na.rm = TRUE) * treatment_uptake

TotalCost <- data.frame(
  Scenario = c("Baseline (no vaccine)", "Intervention (vaccine)"),
  VaccineCost = c(
    0,
    n_offered * cost_vaccine
  ),
  TreatmentCost = c(
    treated_baseline * cost_treatment,
    treated_vaccine  * cost_treatment
  )
) %>%
  mutate(
    TotalCost = VaccineCost + TreatmentCost
  )

# ------------------------------------------------------------------------------
# Calculate Incremental Cost-Effectiveness
# ------------------------------------------------------------------------------

baseline_cases <- TotalImpact$TotalCases[
  TotalImpact$Scenario == "Baseline (no vaccine)"
]

baseline_cost <- TotalCost$TotalCost[
  TotalCost$Scenario == "Baseline (no vaccine)"
]

Analysis <- TotalImpact %>%
  filter(Scenario != "Baseline (no vaccine)") %>%
  left_join(
    TotalCost %>% select(Scenario, TotalCost),
    by = "Scenario"
  ) %>%
  mutate(
    BaselineCases = baseline_cases,
    BaselineCost  = baseline_cost,
    CasesAverted  = BaselineCases - TotalCases,
    IncCost       = TotalCost - BaselineCost,
    IncCost_p     = 100 * IncCost / BaselineCost,
    IncImpact_p   = 100 * CasesAverted / BaselineCases,
    ICER          = IncCost / CasesAverted
  )

# View all the calculations performed
View(Analysis)

# ==============================================================================
# Discussion Questions
# ==============================================================================
#
# Work through these questions by modifying the parameter values above and
# re-running the relevant sections of the script.
#
# ------------------------------------------------------------------------------
# Q: What if vaccine coverage is lower?
# ------------------------------------------------------------------------------
#   Coverage reflects the proportion of susceptibles who are offered and accept
#   the vaccine. Low coverage may result from supply constraints, hesitancy, or
#   access barriers.
#
#   a) Change vaccine_coverage to XXX and re-run the model.
#      How does this affect total cases and the ICER?
#
#   b) Is there a coverage level below which the vaccine is no longer
#      cost-effective under a $500 per case averted threshold?
#      Explore a range of values (e.g. 0.10, 0.20, 0.30, ...) to find it.
#
#   c) Compare the effect of halving coverage vs. halving efficacy.
#      Which has a bigger impact on cases averted and on the ICER? Why might
#      that be?
# ------------------------------------------------------------------------------
