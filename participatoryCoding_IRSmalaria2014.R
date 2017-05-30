##################################################
## Introduction to Sampling & Variability
## Steve Bellan
## Meaningful Modeling of Epidemiological Data 2014
## African Institute of Mathematical Sciences, Muizenberg, South Africa

## Pick a research question:
## 
## Does mosquito density decrease after Peter applies Indoor Residual Spraying (IRS) of DDT to houses?


## Simulate data based on what we think represents what's really going on. We think IRS reduces
## mosquito density. But by how much? And how much variability is there between each house's
## mosquito density even without spraying? We can simulate data given that we specify these valus
## and a sample size.

n <- 200 ## Simulate n houses in the study

## Next set up a treatment vector with half of houses marked as receiving IRS and the other half
## being control (i.e. non-sprayed)
trt <- rep(c('control','IRS'), each = n/2)
## then specify the 'true' simulated average mosquito density at control & IRS houses
mosq.av <- c(20, 15)
mosq.sd <- c(4, 4) ## also specify standard deviation

## Use the mean & standard deviation to simulate values of mosquito density for each house. Make
## sure you understand this code by learning about ?rep and by seeing what each of the function's
## arguments yield (i.e. rep(mosq.av, each = n/2), rep(mosq.sd, each = n/2))
mosq <- rnorm(n, mean = rep(mosq.av, each = n/2), sd = rep(mosq.sd, each = n/2))
mosq ## simulated mosquito densities
dat <- data.frame(trt, mosq) ## set this up in  data frame
head(dat,10) ## look at the first 10 rows

## Let's look at our data first!!
par(mfrow=c(2,1)) ## set up plot panels with 2 rows 1 column
 
## what's the highest mosquito density in any house? we can use this for our X-axis limit
xmax <- max(mosq)

## Histogram of control houses' mosquito density
hist(dat$mosq[dat$trt=='control'], main = 'control', col='black', xlim=c(0,xmax))
## Histogram of IRS houses' mosquito density
hist(dat$mosq[dat$trt=='IRS'], main = 'IRS', col='black', xlim=c(0,xmax))

## What's the observed difference in average mosquito density between treatment groups?
obs.diff.trt <- mean(dat$mosq[dat$trt=='control'])-mean(dat$mosq[dat$trt=='IRS'])

## But is this effect bigger than expected by chance alone? Or alternatively, what's the probability
## of finding an effect this or more extreme by chance alone IF there was no diffrence between
## treatments (i.e. our null hypothesis)?
 
## This null hypothesis can be restated as saying that there is no difference between treatment
## groups and therefore any difference we see is do to random chance. This means that if we randomly
## re-labeled each house as 'control' or 'IRS' and then re-calculated the difference between groups,
## and then repeat this many times, we can get a null distribution of effect sizes IF there was no
## difference between groups. We can then see how rare it is to see an effect as extreme as what we
## ACTUALLY saw by chance alone.

plot.as.we.go <- T ## change to false to speed this code up, but you won't see plots
layout(matrix(c(1,2,3,3),2,2)) ## set up 2 plots on the left column & 1 on the right (see ?layout)
par(mar = c(6,6,4,2), 'ps' = 20) ## set up some margins and text pointsize (ps)
perms <- 100 ## randomly reassort the data perms times
perm.diff.trt.vector <- rep(NA, perms) ## empty vector to store difference betwen groups in
for(ii in 1:perms) {
  dat.perm <- dat
  dat.perm$trt <- sample(dat.perm$trt, n)
  dat.perm$trt
  ## What is the difference between groups in this permuted data set?
  perm.diff.trt <- mean(dat.perm$mosq[dat.perm$trt=='control'])-mean(dat.perm$mosq[dat.perm$trt=='IRS'])
  perm.diff.trt.vector[ii] <- perm.diff.trt ## store this in the ii-th spot of our vector.
  if(plot.as.we.go) {
    xmax <- max(mosq) ## to have same xmax in both histograms
    ## control histogram for ii-th permutation
    hist(dat.perm$mosq[dat.perm$trt=='control'], xlab = 'mosquito density', 
         main = 'control', col='gray', xlim=c(0,xmax), ylim = c(0,n/1.5))
    ## IRS histogram for ii-th permutation
    hist(dat.perm$mosq[dat.perm$trt=='IRS'], xlab = 'mosquito density', 
         main = 'IRS', col='gray', xlim=c(0,xmax), ylim = c(0,n/1.5))
    ## Histogram of ALL PERMUTATIONS DONE SO FAR, showing the difference between treatment groups by permutation
    hist(perm.diff.trt.vector, xlab = 'difference in mosquito density (control - IRS) from permuted data',
         breaks = -7:7, main = 'difference between groups in permuted data sets',
         col = 'light blue', ylim = c(0,perms/2))
    abline(v=obs.diff.trt, lwd = 2, col = 'red') ## observed difference between groups as a red line
    Sys.sleep(0.3) ## slow R down so it's easier to see. COMMENT THIS LINE OUT IF YOU WANT IT TO GO FAST
  }
}
if(!plot.as.we.go) { ## If not plotting as we go, just plot at the end.
  dev.off()
  ## Histogram of ALL PERMUTATIONS DONE SO FAR, showing the difference between treatment groups by permutation
  hist(perm.diff.trt.vector, xlab = 'difference in mosquito density (control - IRS) from permuted data',
       breaks = -7:7, main = 'difference between groups in permuted data sets',
       col = 'light blue', ylim = c(0,perms/2))
  abline(v=obs.diff.trt, lwd = 2, col = 'red') ## observed difference between groups as a red line
}


## What's the P value here? The P value is the probability of getting an effect as extreme as
## obs.diff.trt or greater by chance alone if there was no real difference between groups. So what
## proportion of permutations in which there was no real difference between groups (because we
## permuted) was as extreme or more extreme than our data?

## We can calculate this using the empirical cumulative distribution function: ?ecdf
## Read the help file for this and try to understand how this works. For example, if we wanted to know what proportion of numbers in the vector
xx <- c(3,7,2,9,-4,8,10,-30,12)
## were less than
yy <- 8
## we could run
ecdf(xx)(yy)
## or
mean(xx<=yy)

## So ecdf(xx)(yy) tells us the proportion of xx than are less than or equal to yy.

## Now we assign the p value to be ecdf(xx)(yy) if the observed difference is less than zero
## (because it correctly gives the lower tail), or to be 1-ecdf(xx)(yy) if it's greater than zero.
pval <- ifelse(obs.diff.trt < 0,
               ecdf(perm.diff.trt.vector)(obs.diff.trt),
               1 - ecdf(perm.diff.trt.vector)(obs.diff.trt))
pval

## If our pval is 0, that means we didn't run enough permutations to have any treatment effects from
## permuted data ever be more extreme than our data. Maybe try more permutations? (But make sure to
## turn plotting off above!)

## Or maybe our effect is so big that its got such a small P Value we're unlikely to ever get an
## effect size from permuted data that is that extreme!


####################################################################################################
####################################################################################################
####################################################################################################
## Do the WHOLE above procedure 'sims' times to see how often our P Value is < .05. How often will
## we reject the null hypothesis and detect an effect with an effect of this size?
##
## We're not going to plot here because it will take too long to do this whole thing
####################################################################################################
sims <- 100
perms <- 1000 ## How many permutations to do to calculate each P Value
pval.vector <- rep(NA, sims) ## set up vector to store P Value from each simulation
n <- 50 ## how many houses in each study?
trt <- rep(c('control','IRS'), each = n/2)
mosq.av <- c(20, 15)
mosq.sd <- c(5, 5)
for(jj in 1:sims) { ## for each study
  print(paste('working on simulation',jj)) ## update us on progress
  ## Simulate data from this study
  mosq <- rnorm(n, mean = rep(mosq.av, each = n/2), sd = rep(mosq.sd, each = n/2))
  ## mosq <- rpois(n, lambda = rep(mosq.av, each = n/2)) ## Poisson option
  dat <- data.frame(trt, mosq) ## set up data frame
  ## What's the real effect from this study?
  obs.diff.trt <- mean(dat$mosq[dat$trt=='control'])-mean(dat$mosq[dat$trt=='IRS'])
  ## set up vector to store differences between treatments in permuted data
  perms.diff.trt <- rep(NA, perms) 
  for(ii in 1:perms) { ## for each permutation
    dat.perm <- dat ## copy dat to dat.perm
    dat.perm$trt <- sample(dat.perm$trt, n) ## permute treatment variable
    ## Calculate difference betwen treatments in this permutation
    cont.mean <- mean(dat.perm$mosq[dat.perm$trt=='control'])
    irs.mean <- mean(dat.perm$mosq[dat.perm$trt=='IRS'])
    perm.diff.trt.vector[ii] <- cont.mean - irs.mean
  }
  ## Calculate P value using empirical cumulative distribution function
  pval.vector[jj] <- ifelse(obs.diff.trt < 0,
                            ecdf(perm.diff.trt.vector)(obs.diff.trt),
                            1 - ecdf(perm.diff.trt.vector)(obs.diff.trt))
}

## So we did (or simulated) this study 'sims' times, and each time we did it, we used a permutation
## test to calculate a P value for the probability of getting a difference as or more extreme than
## our observed difference between groups. Here are the P Values for this:
print(pval.vector)

## How often did we detect the effect (i.e. reject the null hypothesis of no effect) assuming that
## we say the effect is statistically significant for P < .05?
power <- mean(pval.vector < .05)
power

## Power is the probability of detecting an effect, which can be determined by simulating a study
## numerous times and seeing how often an effect is detected given sample size, effect size, and
## random variation in the data.

####################################################################################################
####################################################################################################
####################################################################################################
## Simulate a CONFOUNDER
####################################################################################################

## Do matches cause lung cancer?
## Could cigarette smoking confound this relationship?

## We found 200 30 year olds, and asked them if they smoke and whether they had matches on them. We
## then came back in 30 more years (when they're 60) and found out how many of them got lung cancer
## (this is like a cohort study).

## We are primarily interested in whether matches cause lung cancer. Let's imagine that we found
n <- 200
## n 30 year olds, half of whom didn't smoked, and half of whom did smoke
cig <- rep(0:1, each = n/2)

## Let's then say that the probability nonsmokers had matches on them was 20%, and 60% for smokers
matc.prob <- c(.2, .8)
## and randomly sample 0's & 1's to see whether they have matches or not
matc <- rbinom(n, 1, p = rep(matc.prob, each = n/2))

## Let's then assume that 10% of nonsmokers & 40% of smokers get cancer from ages 30-60yrs.
canc.prob <- c(.1,.4)
## and randomly simulate cancer in these people based on their smoking status at age 30.
canc <- rbinom(n, 1, p = rep(canc.prob, each = n/2))

## Assemble this into a data frame.
dat <- data.frame(matc, cig, canc)
head(dat,20)

## What was the cumulative incidence of cancer in our cohort?
mean(dat$canc)

## What about in match-users?
p.canc.matc <- mean(dat$canc[dat$matc==1])
p.canc.matc

## What about in non-match-users?
p.canc.nmatc <- mean(dat$canc[dat$matc==0])
p.canc.nmatc

## What's the cumulative incidence ratio of these?
ciratio <- p.canc.matc/p.canc.nmatc
ciratio

## So match users appear to have higher rates of cancer.

## Let's calculate the odds (p/(1-p)) since that's what we will do in logistic regression.
o.canc.matc <- p.canc.matc / (1-p.canc.matc)
o.canc.nmatc <- p.canc.nmatc / (1-p.canc.nmatc)

o.canc.matc ## odds of cancer amongst match users
o.canc.nmatc ## odds of cancer amongst non-match users

oratio <- o.canc.matc / o.canc.nmatc ## odds ratio
oratio

## Let's do a logistic regression model of cancer vs matches. ?glm
mod <- glm(canc ~ matc, family = binomial('logit'), data = dat)
summary(mod)
## From this output, we are ONLY interested in the line for matc for now, because it tells us the
## log-odds ratio estimate, its standard error, & P Value.

## Let's look at the log(odds ratio):
coefficients(mod)['matc']

or <- exp(coefficients(mod)['matc']) ## odds ratio
or.cis <- exp(confint(mod)['matc',]) ## & odds ratio confidence intervals
or
or.cis

## So our P Value < .05 and our odds ratio (OR) confidence ratio does not
## overlap 1 for matches, meaning that our effect estimate is significantly greater than 1.


## What happens if we control for smoking in this analyses via a multivariate regression.
mod2 <- glm(canc ~ matc + cig, family = binomial, data = dat)
summary(mod2)

## No longer significant!
exp(coefficients(mod2)[c('matc','cig')])
exp(confint(mod2)[c('matc','cig'),])
