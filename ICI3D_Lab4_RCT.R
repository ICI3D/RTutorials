
## Study design: Randomised Controlled Trials 
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program

## Travis Porco
## Reshma Kassanjee 2023 & 2025
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

# The setting for this tutorial:
#   Suppose you are building a SARS-CoV-2 model, and are 
#   trying to decide on your inputs and assumptions related to 
#   transmission rates. 
#   As part of this, you may want to understand the affect of 
#   antiviral treatment on changes in viral load in the
#   infected person, as infectiousness may be higher for 
#   people with higher levels of the virus.
#   Here we are considering a study on the outcome: decrease  
#   in log viral load over 7 days from accessing care.
#   The exposure of interest is receiving antiviral 
#   treatment (versus not).
#   A possible confounder is whether a person has comorbidities 
#   (other conditions such as diabetes which is known 
#   to be associated with poorer COVID-19 outcomes)


# In this tutorial, you will: 
#   Simulate data with randomisation of treatment, and estimate the 'affect'
#   Simulate data without randomisation of treatment, and estimate the 'affect'
#   Repeat the above multiple times to understand the affect of 
#     randomisation on estimation.

library(tidyverse)


#### Warm up 1: Randomisation

## How can we randomly assign subjects to the treatment and control groups?  

# There are different ways of doing this random selection, but for one, 
#   we can use the 'sample' command to sample subjects for one of the groups.

# Here, we divide 20 subjects into a treatment group and a control group.
#   We select without replacement half of the numbers from
#   1 to 20 (representing the people) to create the treatment group.

ids <- 1:20
treatment.group <- sample(x = ids,
                          size = 10,
                          replace = FALSE)
treatment.group

# Repeat this exercise a few times.

# Another way to randomise is to shuffle a vector of ten 0s (representing, 
#   say, the controls) and ten 1s (the treated)  

assignments <- sample(x = c(rep(1,10),rep(0,10)),
                      size = 20,
                      replace = FALSE)
assignments

# The assignment of the first person is found by looking at assignments[1],
#   of the second person, by looking at assignments[2], and so on.  
#   We can print the list of treated subjects like this:

which(assignments==1)


#### Warm up 2: Sample values from a normal distribution

# To simulate a given number of normal random variates, use the 'rnorm' command.  
#   Here, we simulate 100 normal variates with a mean of 1 and a standard
#   deviation of 0.3, and plot the data in a histogram

v1 <- rnorm(100, mean = 1, sd = 0.3)

(ggplot(data = data.frame(outcomevar = v1), mapping = aes(x=outcomevar))
  + labs(x = 'Outcome', y = 'Frequency'
         , title = 'Histogram of generated outcomes')
  + geom_histogram(bins = 20, fill = 'red', color = 'black')
  + theme_minimal()
  + theme(text = element_text(size = 18))
)



#### Warm up 3: Simulate a dataset

# The outcome is a continuous measure (decrease in log viral load),
#   and the study aims to compare the average outcome between the 
#   two study groups/arms (antiviral treatment versus not - i.e. 
#   versus the standard control treatment).

# We will now generate a simulated (or fake) dataset (so something
#   like what you may have obtained in a real-world study),
#   and save it as a data frame. 

# We will make a data frame which contains the following columns:
#   study.id (1,2,..), treatment (0/1), and outcome (a continuous number),
#   and where each row provides the data for one subject.

# The average or mean outcome for the treatment group can be different to that
#   in the control group --  this difference is the 'treatment effect',
#   which we want to estimate.

# To begin, we will assume the treatment effect is zero. 

n.subjects        <- 500    # number of subjects in study (even number for this tutorial)
control.mean      <- 2      # mean outcome for the control group
treatment.effect  <- 0     # difference in mean for the treatment group (vs control)
outcome.sd        <- 0.5    # spread (standard deviation) of the outcome in each arm

dset            <- data.frame(study.id = 1:n.subjects)
dset$treatment  <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)),
                           size = n.subjects,
                           replace = FALSE)
dset.mean       <- control.mean + treatment.effect*dset$treatment
dset$outcome    <- rnorm(n.subjects,
                         mean = dset.mean,
                         sd = outcome.sd)

# Let us look at the first (up to) 20 rows of the dataset:

head(dset, 20)

# Take some time to ensure you understand each step above. 

# Let us plot histograms of the outcomes, by arm:

(ggplot(data = dset, mapping = aes(x=outcome))
  + labs(x = 'Acuity (logMAR)', y = 'Frequency'
         , title = 'Acuity by treatment group')
  + geom_histogram(bins = 20, fill = 'red', color = 'black')
  + facet_grid(treatment~.)
  + theme_bw()
  + theme(text = element_text(size = 18))
)

# We can also calculate the mean outcome in each arm

dset |> group_by(treatment) |> summarise(mean = mean(outcome))

# and the difference in means (estimated treatment effect):  

means <- dset |> group_by(treatment) |> summarise(mean = mean(outcome))
with(means, mean[treatment == 1] - mean[treatment == 0]) 

# Now change the treatment effect to be non-zero. For example, 
#   suppose that the average outcome in the treatment group is 0.7
#   units larger than in the control group - generate dset and the plot. 
# Each time you run the code above, do you create the same dataset?


# Now that we have warmed up, let's dive in. 
# Let us quickly clear up our environment before moving on. 

rm(list=ls())


#### A function to generate data

# Our goal is to explore effect estimation under two different scenarios - 
#   (I) One for a study where treatment is not randomly assigned 
#   (II) One for a study where treatment is randomly assigned. 

# Each person in our dataset either receives antiviral treatment or not, 
#   and can have a comordity or not.
# The presence of comorbities is a confounder, because a doctor 
#   may decide on the treatment according to this, and 
#   comorbidities can also directly impact our outcome
#   (e.g. perhaps people with other illness have a slower drop in 
#   in viral load)

# Since we will want to generate a dataset multiple times below,
#   we write a function to make the data. 
#   We have also specified default values for all inputs to the 
#   function.

# Carefully work through the function below and make sure you understand each line. 
# Also think about how the inputs need to be set for scenarios (I) and (II).  

# Suppose now that randomisation was not applied. 
#   Instead doctors could choose treatment assignments, and they based their decisions
#   on how compromised baseline acuity was.
#   Here we extend our data generation function to take this into account:
#   people who were 'severe' were provided the treatment with 
#   probability treatment.prob.severe (default of 0.8), and people not 'severe' 
#   received the treatment with probability treatment.prob.notsevere 
#   (default of 0.4)

# Ask mentors and faculty if you are confused or would like to confirm you understanding
# Or talk it through with your neighbour.

gen.data <- function(n.subjects = 100 # number of subjects
                     , ref.mean = 2 # mean outcome in control group without comorbidites
                     , treatment.effect = 1.5 # effect of treatment on outcome
                     , comorb.effect = -1 # effect of comorbidity on outcome
                     , prev.comorb = 0.2 # prevalence of comorbidities
                     , prob.treat.nocomorb = 0.5 # probability of treatment in people without comorbidites
                     , prob.treat.comorb = 0.5 # probability of treatment in people with comorbidites
                     , outcome.sd = 0.75 # standard deviation of outcome
){
  dset                <- data.frame(study.id = 1:n.subjects)
  dset$comorbidity    <- rbinom(n.subjects, 1, prev.comorb)
  prob.treatment      <- prob.treat.comorb*dset$comorbidity + prob.treat.nocomorb*(1-dset$comorbidity)
  dset$treatment      <- rbinom(n.subjects, 1, prob.treatment)
  mean.vector         <- ref.mean + treatment.effect*dset$treatment + comorb.effect*dset$comorbidity
  dset$outcome        <- rnorm(n.subjects, mean = mean.vector, sd = outcome.sd)
  dset
}


# Let us try out the function

gen.data() # when you do not specify an input, the default value

# Choose a couple of inputs to change (one at a time), and try to check whether 
# the dataset changes in the expected way - using some of what you have learnt
# about exploring data 

# For example, compare 
d1 <- gen.data(treatment.effect = 0) 
d2 <- gen.data(treatment.effect = 1)
d3 <- gen.data(treatment.effect = 2)

# using 
d1 |> group_by(treatment) |> summarise(mean = mean(outcome))
d2 |> group_by(treatment) |> summarise(mean = mean(outcome))
d3 |> group_by(treatment) |> summarise(mean = mean(outcome))



#### Understanding how randomisation balances confounders
  
# Let's return to our two study designs:
#   (I) A study where treatment is not randomly assigned 
#   (II) A study where treatment is randomly assigned. 
  
# In (I), the doctor can decide who gets treatment and may decide to 
#   more frequently provide patients with comorbidities antivirals. 
#   Which inputs above would change?
  
# An example of data that may be generated in such a scenario is:    
  
d1.norandom <- gen.data(prob.treat.nocomorb = 0.5 
                          , prob.treat.comorb = 0.8)

d1.norandom |> group_by(comorbidity) |> summarise(mean = mean(treatment))
d1.norandom |> group_by(treatment) |> summarise(mean = mean(comorbidity))

# We can see that the confounder is not balanced between the two treatment arms. 

# In (II) treatment is randomly assigned, and therefore cannot 
#   be related to whether the person has a comorbidity. 

d1.random <- gen.data(prob.treat.nocomorb = 0.70 
                      , prob.treat.comorb = 0.70)

d1.random |> group_by(comorbidity) |> summarise(mean = mean(treatment))
d1.random |> group_by(treatment) |> summarise(mean = mean(comorbidity))

# We can see that the confounder is balanced between the two treatment arms
#   by study design.

# It is sometimes difficult to see the above because of noise in our dataset.
# Try to increase the study size and rerun the lines above to convince yourself
#   of the imbalance / balance of comorbity by treatment arm. 



#### Estimating the difference

# We are going to need a way to analyse the data. Below is a function for doing 
#   this. Don't worry about the details (if you have time at the end you can 
#   investigate it more) - just know if you provide it a dataset with columns
#   'treatment' (0 for control and 1 for treatment) and 'outcome' (numerical)
#   it will provide you an estimate of the difference in the average outcome for 
#   your treatment and control arms, with a 95% confidence interval. 
#   It obtains this estimate by comparing the recorded outcomes for the two groups.


analyse.data <- function(data.in){
  # using central limit theorem
  mean.diff <- mean(data.in$outcome[data.in$treatment == 1]) - mean(data.in$outcome[data.in$treatment == 0])
  est.sd <- sqrt(var(data.in$outcome[data.in$treatment == 1])/length(data.in$outcome[data.in$treatment == 1]) +
                   var(data.in$outcome[data.in$treatment == 0])/length(data.in$outcome[data.in$treatment == 0]))
  est.point <- mean.diff
  est.ci.lower  <- mean.diff-1.96*est.sd
  est.ci.upper  <- mean.diff+1.96*est.sd
  return(round(c(est = est.point, lower = est.ci.lower, upper = est.ci.upper),3))
}

# For example, here we generate two large datasets, and analyse them:

data.temp <- gen.data(n.subjects = 1000, treatment.effect = 0.2)
analyse.data(data.temp)

data.temp <- gen.data(n.subjects = 1000, treatment.effect = 0.2)
analyse.data(data.temp)

# Very importantly - what the is the TRUE values that we are trying to estimate?
# It is the 'treatment.effect'. 
# You specified this value to generate the data, so we know the correct 'answer' to 
# our estimation - this is what makes simulation so useful!



#### Multiple studies - without and with randomisation 

## Let us repeat this process of generating and analysing the data multiple times, 
#   and explore our results.
#  We will 'repeat' our study multiple times using a 'for' loop, and gatHering our
#  estimates in a data frame. We will do 500 simulations. 

# We will set the treatment.effect to 1 - this is the number we are trying to estimate.
# We will mostly keep the other inputs as the default values.

in.treatment.effect <- 1

# In scenario (I), doctors get to decide on treatment. 
# Let's suppose they provide antivirals to 50% of those without comorbidities and 80% with. 

n.sims <-  500

df.ests1 <- data.frame(study.number = 1:n.sims, est = NA, lower = NA, upper = NA)
for (ii in 1:n.sims) {
  data.ii <- gen.data(treatment.effect = in.treatment.effect
                      , prob.treat.nocomorb = 0.5 
                      , prob.treat.comorb = 0.8)
  ests.ii <- analyse.data(data.ii)
  df.ests1$est[ii]   <- ests.ii['est']
  df.ests1$lower[ii] <- ests.ii['lower']
  df.ests1$upper[ii] <- ests.ii['upper'] 
}

# Let us plot the estimates and CIs for the studies, and add a 
#   horizontal line showing the true treatment effect.

ggplot(data = df.ests1, aes(x = study.number, y = est)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "grey") +
  geom_point(color = "red", size = 1.5) +
  geom_hline(yintercept = in.treatment.effect, colour = "blue") +
  labs(
    title = "Estimated treatment effect over multiple studies\n(without randomisation)",
    x = "Study number",
    y = "Estimated treatment effect (and 95% CI)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14)
  )

# Are we estimating the treatment effect well?
# Why?
# what else do we learn from the plot?

# Let us calculate the bias, i.e., the average estimate minus the true treatment effect

mean(df.ests$est) - in.treatment.effect


# In scenario (II), doctors do not get to decide on treatment - we set it randomly. 
# Let's suppose we provide antivirals to 70% of people, randomly. 

df.ests2 <- data.frame(study.number = 1:n.sims, est = NA, lower = NA, upper = NA)
for (ii in 1:n.sims) {
  data.ii <- gen.data(treatment.effect = in.treatment.effect
                      , prob.treat.nocomorb = 0.7 
                      , prob.treat.comorb = 0.7)
  ests.ii <- analyse.data(data.ii)
  df.ests2$est[ii]   <- ests.ii['est']
  df.ests2$lower[ii] <- ests.ii['lower']
  df.ests2$upper[ii] <- ests.ii['upper'] 
}

# Let us plot as before

ggplot(data = df.ests2, aes(x = study.number, y = est)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "grey") +
  geom_point(color = "red", size = 1.5) +
  geom_hline(yintercept = in.treatment.effect, colour = "blue") +
  labs(
    title = "Estimated treatment effect over multiple studies\n(with randomisation)",
    x = "Study number",
    y = "Estimated treatment effect (and 95% CI)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 14)
  )

# Let's also calculate the bias again

? ## FIXME
  
# Again, are we estimating the value well? Why? What else do we learn from the plot?

# Once you understand and reflect on the above - try play around. For example
# To look at the results in a different way you could order the studies in the plot
# by the size of estimates, or draw histograms of your estimates for the two scenarios. 
# You can try different study sizes, different prevalence values for comorbities, and 
# more or less extreme confounder imbalances between the two arms (by changing inputs). 

  