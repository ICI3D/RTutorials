
## Originally developed by Travis Porco
## Last updated by Reshma Kassanjee, June 2023
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)


# The setting for this tutorial exercise is loosely inspired by the RCT
#   described by Prajna et al 2010 (https://europepmc.org/article/PMC/3774126).
#   In this clinical trial of patients presenting to the clinic with corneal 
#   ulcers, the primary outcome was spectacle-corrected 
#   visual acuity (BSCVA) at 3 months, which is a continuous measure. 
#   Study participants were randomized to 
#   (1) receive either topical natamycin or topical voriconazole, and
#   (2) either have repeated scraping of the epithelium or not.

# Try to work through Part One during the tutorial time
#   Part Two is 'extra' and optional, to do in your own time  



########## PART ONE ##########


# To begin, let us focus on a simpler version of this trial: 
#   study subjects were randomized to receive either natamycin
#   (we will call these subjects the control group) or voriconazole 
#   (forming the treatment group)




#### RANDOMISATION ####


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




#### GENERATING A DATASET ####

# We will now generate a simulated (or fake) dataset, and save it as a data frame. 

# Recall that the outcome is a continuous measure, and that the study aims to
#   compare the average outcome (i.e., acuity) between the two study groups/arms.


## Warm up exercise: Normal random variables

# To simulate a given number of normal random variates, use the 'rnorm' command.  
#   Here, we simulate 100 normal variates with a mean of 1 and a standard
#   deviation of 0.3, and plot the data in a histogram

v1 <- rnorm(100, mean = 1, sd = 0.3)

par(mfrow = c(1,1),
    oma = c(0,0,0,0)) # This sets the number of subplots, and margins. 
# Do not worry about these sorts of details for now.
hist(v1,
     xlab = 'Acuity (logMAR)',
     ylab = 'Frequency',
     main = 'Histogram of generated outcomes',
     col = 'red')



## Make a dataset

# We will make a data frame which contains the following columns:
#   study.id (1,2,..), treatment (0/1), and outcome (a continuous number),
#   and where each row provides the data for one subject.

# The average or mean outcome for the treatment group can be different to that
#   in the control group --  this difference is the 'treatment effect',
#   which we want to estimate.

# To begin, we will assume the treatment effect is zero. 

n.subjects        <- 500    # number of subjects in study (even number)
control.mean      <- 0.5    # mean acuity for the control group
treatment.effect  <- 0      # difference in mean for the treatment group (vs control)
outcome.sd        <- 0.3    # spread (standard deviation) of the outcome in each arm

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

# Let us plot histograms of the outcomes, by arm:

par(mfrow = c(2,1),
    oma = c(0,0,0,0))
hist(dset$outcome[dset$treatment==0],
     xlab = 'Acuity (logMAR)',
     ylab = 'Frequency',
     main = 'Control group',
     col = 'blue',
     xlim = c(min(dset$outcome)-outcome.sd/10, max(dset$outcome)+outcome.sd/10),
     breaks = seq(from = min(dset$outcome)-outcome.sd/10
                  , to = max(dset$outcome)+outcome.sd/10
                  , length.out = 20))
hist(dset$outcome[dset$treatment==1],
     xlab = 'Acuity (logMAR)',
     ylab = 'Frequency',
     main = 'Treatment group',
     col = 'red',
     xlim = c(min(dset$outcome)-outcome.sd/10, max(dset$outcome)+outcome.sd/10),
     breaks = seq(from = min(dset$outcome)-outcome.sd/10
                  , to = max(dset$outcome)+outcome.sd/10
                  , length.out = 20))

# We can also calculate the mean outcome in each arm

tapply(dset$outcome, dset[,'treatment'], mean)

# and the difference in means (estimated treatment effect):  

mean(dset$outcome[dset$treatment == 1]) - mean(dset$outcome[dset$treatment == 0])

# Now change the treatment effect to be non-zero. For example, 
#   suppose that the average outcome in the treatment group is 0.7
#   units larger than in the control group.
#   i.e., set treatment.effect = 0.7 and regenerate and plot dset.


# Let us quickly clear up our environment before moving on. 

rm(list=ls())


## A function to generate data 

# Since we will want to generate a dataset multiple times below,
#   we have written a function to make the data. 
#   We have also specified default values for the inputs. 

gen.data <- function(n.subjects = 100, 
                     control.mean = 0.5, treatment.effect = 0.7, outcome.sd = 0.3) {
  dset            <- data.frame(study.id = 1:n.subjects)
  dset$treatment  <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)), size = n.subjects, replace = FALSE)
  mean.vector     <- control.mean + treatment.effect*dset$treatment
  dset$outcome    <- rnorm(n.subjects, mean=mean.vector, sd=outcome.sd)
  dset
}

# Try out the function

gen.data() # when you do not specify any of the inputs, the default value is used for that input

gen.data(control.mean = 1, treatment.effect = 0, outcome.sd = 0.3) 
gen.data(control.mean = 1, treatment.effect = 1, outcome.sd = 0.1)

# Replace ?? with your chosen inputs
gen.data(n.subjects = ??, control.mean = ??, treatment.effect = ??, outcome.sd = ??) 




#### UNDERSTANDING HOW RANDOMISATION BALANCES CONFOUNDERS ####

# Suppose that how compromised acuity is at 0 months also affects the outcome. 
#   The baseline acuity is classified as either severely compromised / 'severe' (1) 
#   or not (0).
# Let us generate our dataset again, but this time subjects also have a 
#   (binary) baseline value (called x.severe, either 0 or 1). 
#   If a subject starts as 'severe' (1), their outcome is shifted by some
#     amount (called severe.effect below, with a default values of -0.3).
#   There is a probability severe.prob (default of 0.5) of starting in the
#   'severe' class. 

# Because treatment is randomised, the proportion of subjects with 'severe'
#   should be similar in each arm. By chance, they may be quite different for any 
#   given dataset, but overall, on average, the arms should be well balanced.

gen.data.severe <- function(n.subjects = 100,
                            control.mean = 0.5, treatment.effect = 0.7, outcome.sd = 0.3,
                            severe.prob = 0.5, severe.effect = -0.3) {
  dset            <- data.frame(study.id = 1:n.subjects)
  dset$x.severe   <- rbinom(n.subjects, 1, severe.prob)
  dset$treatment  <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)), size = n.subjects, replace = FALSE)
  mean.vector     <- control.mean + treatment.effect*dset$treatment + severe.effect*dset$x.severe
  dset$outcome    <- rnorm(n.subjects, mean = mean.vector, sd = outcome.sd)
  dset
}

# Let us generate a dataset using our default inputs, 
#   and calculate the proportion severe in each arm, 
#   and the mean outcome for each group of subjects.

# Generate data:
data.temp <- gen.data.severe() 

# Proportion with severe infection in each arm:
mean(data.temp$x.severe[data.temp$treatment==0])
mean(data.temp$x.severe[data.temp$treatment==1])

# Mean outcome by whether 'severe' and treatment:
tapply(data.temp$outcome, data.temp[,c('x.severe','treatment')], mean)

# Highlight the four lines of code above that generate and explore data.temp, 
#   and run them a few times, and reflect on the outputs.




#### BIAS IN ESTIMATION?? ####

# Let us now analyse the data. For each dataset that we simulate, we will estimate 
#   the treatment effect, with a 95% confidence interval (CI).

# *NB*. You do not need to worry about how we analyse the data,
#   please rather spend this time understanding the other concepts. 
#   You can just apply the function we provide below to a dataset.

# This is the function we will use to analyse the data:

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

# Importantly, we assume that we do not record the variable x.severe (baseline severity)
#   and thus do not include it in the analysis, even though it affects our outcome. 

# For example, here we generate two datasets, and analyse them:

data.temp <- gen.data.severe(treatment.effect = 0.2)
analyse.data(data.temp)

data.temp <- gen.data.severe(treatment.effect = 0.2)
analyse.data(data.temp)



## Let us repeat this process of generating and analysing the data multiple times, 
#   and explore our results:

# First we specify our inputs

n.sims              <-  500    # Number of times to repeat the study
treatment.effect.in <-  0.8    # Input treatment effect

# Then we generate and analyse the datasets, saving the results in df.ests

df.ests <- data.frame(study.number = 1:n.sims, est = NA, lower = NA, upper = NA)
for (ii in 1:n.sims) {
  data.ii <- gen.data.severe(treatment.effect = treatment.effect.in)
  ests.ii <- analyse.data(data.ii)
  df.ests$est[ii]   <- ests.ii['est']
  df.ests$lower[ii] <- ests.ii['lower']
  df.ests$upper[ii] <- ests.ii['upper'] 
}

# Let us plot the estimates and CIs for the studies, and add a 
#   horizontal line showing the true treatment effect.

par(mfrow = c(1,1)
    , oma = c(0,0,0,0))
y.min.temp <- min(df.ests$lower)-0.1
y.max.temp <- max(df.ests$upper)+0.1
with(df.ests, 
     {plot(study.number
           , est
           , xlab = 'Study number'
           , ylab = 'Estimated treatment effect (and 95% CI)'
           , main = 'Estimated treatment effect over multiple studies \n (with randomisation)'
           , ylim = c(y.min.temp, y.max.temp)
           , type = 'n'); 
       arrows(x0 = study.number, y0 = lower, y1 = upper, code = 0, col = 'darkgrey');
       points(study.number, est, pch = 20, col = 'red');
       abline(h=treatment.effect.in, col = 'blue', lwd = 2)
     })

# Let us calculate the bias, i.e., the average estimate minus the true treatment effect

mean(df.ests$est) - treatment.effect.in


# Think about what all of these outputs show us.




#### BIAS WITHOUT RANDOMISATION ####

# Suppose now that randomisation was not applied. 
#   Instead doctors could choose treatment assignments, and they based their decisions
#   on how compromised baseline acuity was.
#   Here we extend our data generation function to take this into account:
#   people who were 'severe' were provided the treatment with 
#   probability treatment.prob.severe (default of 0.8), and people not 'severe' 
#   received the treatment with probability treatment.prob.notsevere 
#   (default of 0.4)

gen.data.conf <- function(n.subjects = 100
                          , control.mean = 0.5, treatment.effect = 0.7, outcome.sd = 0.3
                          , severe.prob = 0.5, severe.effect = -0.3
                          , treatment.prob.severe = 0.8, treatment.prob.notsevere = 0.4) {
  dset            <- data.frame(study.id = 1:n.subjects)
  dset$x.severe   <- rbinom(n.subjects, 1, severe.prob)
  prob.treatment  <- treatment.prob.severe*dset$x.severe + treatment.prob.notsevere*(1-dset$x.severe)
  dset$treatment  <- rbinom(n.subjects, 1, prob.treatment)
  mean.vector     <- control.mean + treatment.effect*dset$treatment + severe.effect*dset$x.severe
  dset$outcome    <- rnorm(n.subjects, mean = mean.vector, sd = outcome.sd)
  dset
}

## Let us repeat the process of generating and analysing the data multiple times, 
#   as above, and compare our results.

# Let us use the same inputs as above

n.sims              <- 500   
treatment.effect.in <- 0.8   

# And generate and analyse the data, saving the estimates in
#   df.ests.conf (conf is for confounding)

df.ests.conf <- data.frame(study.number = 1:n.sims, est = NA, lower = NA, upper = NA)
for (ii in 1:n.sims) {
  data.ii <- gen.data.conf(treatment.effect = treatment.effect.in)
  ests.ii <- analyse.data(data.ii)
  df.ests.conf$est[ii]   <- ests.ii['est']
  df.ests.conf$lower[ii] <- ests.ii['lower']
  df.ests.conf$upper[ii] <- ests.ii['upper'] 
}

# Let us explore the results as before.

par(mfrow = c(1,1)
    , oma = c(0,0,0,0))
y.min.temp <- min(df.ests.conf$lower)-0.1
y.max.temp <- max(df.ests.conf$upper)+0.1
with(df.ests.conf, 
     {plot(study.number
           , est
           , xlab = 'Study number'
           , ylab = 'Estimated treatment effect (and 95% CI)'
           , main = 'Estimated treatment effect over multiple studies \n (without randomisation)'
           , ylim = c(y.min.temp, y.max.temp)
           , type = 'n'); 
       arrows(x0 = study.number, y0 = lower, y1 = upper, code = 0, col = 'darkgrey');
       points(study.number, est, col = 'red', pch = 20);
       abline(h=treatment.effect.in, col = 'blue', lwd = 2)
     })

# And calculate the bias again

mean(df.ests.conf$est) - treatment.effect.in


# Think about what all of these outputs show us.

# Remember, in each of the two sets of simulations, both severity and treatment
#   affect the outcome. In both, we assume that we cannot measure severity and
#   cannot include it in our analysis.  Our goal is to understand the effect 
#   of treatment on the outcome.

# What is the interpretation of the estimates in the first simulation 
#   (with randomisation)  and in the second (without randomisation)?


# If you are done, you can play around with different values for some of the inputs.









########## PART TWO ##########

# Here you will get to explore some clinical trial data. These data (from the
#   Mycotic Ulcer Therapeutic Exploratory Trial) can only be used for this lab.
#   To protect confidentiality, we have added a tiny random value to some of 
#   the scar values from the actual trial.

# The (secondary) outcome you will focus on here is the scar size at 3 weeks.

# To load the data, use:

load('MuTxT.Rdata')

# The variables are:
#   patid           -- patient ID
#   drug            -- drug assignment: 0 is natamycin, 1 is voriconazole 
#   scrape          -- scraping: 0 is no, 1 is yes
#   age             -- age
#   sex             -- sex
#   if_perf         -- 1 if a perforation happened
#   scar_baseline   -- scar size at baseline
#   scar_3W        -- scar size at 3 weeks

# Try to estimate the treatment effect on scar size at 3 weeks. 
#   Consider controlling for baseline scar size.
#   Is it necessary to control for a covariate? Why might one do so?

# An example of an analysis is

summary(lm(scar_3W ~ drug + scrape, data=mutxt))

