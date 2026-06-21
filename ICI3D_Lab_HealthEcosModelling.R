
#####################################################################

## Lab: Introduction to health economics in dynamical modelling 

#####################################################################
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org
##
## Attribution: Mmamapudi Kubjane (2026)
##             
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)
##
#####################################################################

##		The goal of this to lab is to complete a simple health 
##		economics modelling exercise in R.
##
##		In this lab, you will: 
##    - Define and simulate alternative vaccination intervention scenarios
##    - Estimate the epidemiological impact of vaccination on disease incidence
##    - Attach costs to key health outcomes and interventions
##    - Calculate and interpret the Incremental Cost-Effectiveness Ratio (ICER)
##
##    In this lab, we will build on 'Lab: ODE models in R'
##      (https://github.com/ICI3D/RTutorials/blob/master/ICI3D_Lab_ODEmodels.R). 
##    We will extend the SIR model to include a vaccination intervention.
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
#   
treatment_uptake <- 0.85   # 85% treated, 15% lost individuals 

# ------------------------------------------------------------------------------
# Specify intervention (vaccine) parameters 
# ------------------------------------------------------------------------------
vaccine_coverage <- 0.70   # Proportion of susceptible offered vaccine
vaccine_efficacy <- 1.00   # 100% protection in those vaccinated
# Individuals not vaccinated remain in S, fully susceptible.

# ------------------------------------------------------------------------------
# SIR Differential equations as in Lab 1 (re-review if have forgotten)
# ------------------------------------------------------------------------------
sir_equations <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
  	N  <- S + I + R
  	dS <- -beta * S * (I / N)           
    dI <-  beta * S * (I / N) - gamma * I
    dR <-  gamma * I
    cumcases <- beta * S * (I / N)    
    return(list(c(dS, dI, dR, cumcases)))
  })
}

# ------------------------------------------------------------------------------
# Epidemiological parameters (apply to both scenarios)
# ------------------------------------------------------------------------------
parameters_values <- c(
  beta  = 0.7,   # Infectious contact rate (per person per day)
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
  y     = c(S = S0_base, I = I0, R = 0, cumcases = 0),
  times = time_values,
  func  = sir_equations,
  parms = parameters_values
))
sir_baseline$Scenario <- "Baseline (no vaccine)"

head(sir_baseline,10)
# what information is this data frame showing?

# ------------------------------------------------------------------------------
# Run Intervention: Vaccine applied before epidemic
# ------------------------------------------------------------------------------

# For the intervention scenario:
#		- a proportion of S individuals are vaccinated at time 0
#		- there is no ongoing vaccination
#		- to indicate that the (successfully) vaccinated people are protected
#			from infection), we move them into state R at time 0. 
#		- Alternatively, we could have created a separate component for them (e.g. V)
#	The number of people from S who are moved into R due to the intervention
#		depends on vaccine coverage and vaccine efficacy.

n_offered   <- round(vaccine_coverage * S0_base)
n_protected <- round(n_offered * vaccine_efficacy)
S0_vax      <- S0_base - n_protected
R0_vax      <- n_protected


sir_vaccine <- as.data.frame(ode(
  y     = c(S = S0_vax, I = I0, R = R0_vax, cumcases = 0),
  times = time_values,
  func  = sir_equations,
  parms = parameters_values
))
sir_vaccine$Scenario <- "Intervention (vaccine)"

head(sir_vaccine,10)
# what information is this data frame showing?

# ------------------------------------------------------------------------------
# Compare cumulative cases over time for each scenario
# ------------------------------------------------------------------------------

CasesByScenario <- bind_rows(sir_baseline %>% select(time, Scenario, cumcases)
														 , sir_vaccine %>% select(time, Scenario, cumcases))

ggplot(CasesByScenario, 
			 mapping = aes(x = time, y = cumcases, color = Scenario)) +
	labs(x = 'Time (days)', y  = 'Cumulative cases') +
	geom_line(size = 2) +
	theme_minimal() +
	theme(text = element_text(size = 20))  


# ------------------------------------------------------------------------------
# Sum total impact in both scenarios 
# ------------------------------------------------------------------------------

TotalImpact <- data.frame(
  Scenario = c("Baseline (no vaccine)", "Intervention (vaccine)"),
  TotalCases = c(
    max(sir_baseline$cumcases),
    max(sir_vaccine$cumcases))
)

TotalImpact
# what information is this data frame showing?

# ------------------------------------------------------------------------------
# Sum total cost in both scenarios
# ------------------------------------------------------------------------------

treated_baseline <- max(sir_baseline$cumcases) * treatment_uptake
treated_vaccine  <- max(sir_vaccine$cumcases) * treatment_uptake

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

TotalCost
# what information is this data frame showing?

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
Analysis
# what information is this data frame showing?

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
#   a) Change vaccine_coverage to 20% and 50% in turn, and re-run the model.
#      How does this affect total cases and the ICER?
#
#   b) Compare the effect of halving coverage vs. halving efficacy.
#      Which has a bigger impact on cases averted and on the ICER? Why might
#      that be?
#
# ------------------------------------------------------------------------------
