#####################################################################

## Lab: Introduction to Likelihood

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: Steve Bellan (2009)
##              Cari van Schalkwyk
##              Jonathan Dushoff

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

## The GOAL of this lab is FIXME:
## By the end of this lab, you should be able to:
##
##  * FIXME:
##  * FIXME:
##  * FIXME:
##
## NOTE: The comments will guide you through the lab but you
## should make sure you understand what the code is doing.  Some
## code may have question marks and cause errors. 
## You should fill these in; often you will find suggestions in the comments

## Before you start, it is a good idea to clear your workspace.
## In rstudio, do this with Session/Restart
## There is also a hotkey, which you should memorize and use frequently
## In plain R, just quit `q()` and then start again

######################################################################

## Section 1: FIXME:

## Imagine we are randomly sampling from a population of people in a town
## of size 1,000,000 and we sample 100 people from that population.
sampleSize <- 100
## We assume that the true HIV prevalence in the whole population 
## is 30%, i.e. 300,000 are HIV positive.
truePrev <- .3

set.seed(3)
samplePos <- rbinom(1, sampleSize, truePrev)

## You can use the above code to sample from this distribution, but
## we'll set it at 28 just so everyone is working on the same example
## for now.
samplePos <- 28                          
samplePrev <- samplePos/sampleSize
samplePrev

## Create a color vector for every possible number of HIV+ people we
## could have found when sampling 100 people (0:100 means 101
## possibilities). Then make the value we chose have a different
## color.
color.vec <- rep("black",sampleSize+1)
color.vec[samplePos+1] <-	"purple"

### Create a histogram of the binomial distribution function with the
### truePrevalence, highlighting the value we sampled.
barplot.obj <- barplot(dbinom(0:sampleSize,size=sampleSize, prob=truePrev),
                       xlab="number HIV+", names.arg=c(0:sampleSize),
                       ylab="probability", col=color.vec, border = NA, space =0)

## A politician is claiming that HIV prevalence is 20%
## Given that we found samplePos
samplePos
## positives, would we accept or reject the hypothesis that the truePrevalence is .2?
potential.prev <- .2

## Calculate the probability of getting every single possible value
## from the binomial distribtion with this hypothesized prevalence.
prob.dist <- dbinom(0:sampleSize, size=sampleSize, prob=potential.prev)
ymax <- .2
color.vec <- rep("grey",sampleSize)

## Plot the binomial distribution for N=100, and p=.2
barplot(prob.dist,
        xlab = "number HIV+",
        names.arg = c(0:sampleSize),
        ylab = "probability",
        col = color.vec ,
        border = NA,
        main = paste("hypothetical prevalence:",potential.prev*100,"%"),
        ylim = c(0,ymax),
        space = 0)

##What is the probability of observing 28/100, under this hypothesised prevalence?
dbinom(samplePos, sampleSize, potential.prev)

arrows(x0 = samplePos , y0=ymax, y1=dbinom(samplePos, sampleSize, potential.prev))

## Now do a classic test:

binom.test(samplePos, sampleSize, potential.prev, alternative = "two.sided")

## Task 1: Explain what we learn from the binom.test
## Focus on the confidence intervals; don't worry about the P value (or about the "guess" (potential.prev)

## Task 2: Repeat the above for two or three different examples 
## with different hypothesized prevalence and/or samplePos
## Explain what you see.

###########################################################################
## Now let's use the Maximum Likelihood approach to construct confidence intervals.
########################################################################### 

## Create a vector of potential prevalences spanning 0-1 with 10000 values
## These are all potential "null hypotheses".
potential.prev.vector <- seq(0,1, length=10000)

## The likelihood of a prevalence is the probability of observing the data given the
## hypothesized prevalence:
## L(prevalence | data) = p(data | prevalence)
likelihoods <- dbinom(samplePos, sampleSize, potential.prev.vector)

plot(potential.prev.vector, likelihoods, col = "purple", type = "l", lwd = 2,
     xlab = "potential HIV prevalences", ylab = "likelihood",
     main = "p(our data given prevalence) = LIKELIHOOD",
     bty = "n")

###################################################################### 
## Let's look at the negative log-likelihood which is what people
## traditionally use because it's usually easier to calculate (though
## for this example R can do all the work computationally quite
## easily).
plot(potential.prev.vector, -log(likelihoods), type = "l", col = "purple", col.main = "white",
     bty = "n", lwd = 3, xlab = "potential prevalences (our models)",
     ylab = "-log(likelihood)",  main = "we usually minimize the -log(likelihood)")

## We can use the Likelihood Ratio Test to calculate confidence intervals. 
## If the logL of a hypothesised prevalence is not within the chi-squared
## alpha=.05 cutoff of the minimum logL, then we do not reject it.
chisq.crit <- qchisq(.95, df = 1)
min.l <- min(-log(likelihoods))
abline(h = min.l + chisq.crit/2, lwd = 3, lty = 2)

######################################################################
## It's hard to see this so let's zoom in on last plot for values of
## the potential prevalence between .15 and .5.

xlim <- c(.15, .5)

zoom.plot.index <- potential.prev.vector>.15 & potential.prev.vector<.5
plot(potential.prev.vector[zoom.plot.index], -log(likelihoods[zoom.plot.index]),
     type = "l", col = "purple", col.main = "white", xlim = xlim,
     bty = "n", lwd = 3, xlab = "potential prevalences (our models)",
     ylab = "-log(likelihood)",  main = "we usually minimize the -log(likelihood)")
chisq.crit <- qchisq(.95, df = 1)
chisq.crit
min.l <- min(-log(likelihoods))
ci.likelihood <- range(potential.prev.vector[-log(likelihoods) < min.l + chisq.crit/2])
abline(h = min.l + chisq.crit/2, lwd = 3, lty = 2)
## add title to the plot
ci.l <- signif(ci.likelihood[1],3)
ci.u <- signif(ci.likelihood[2],3)
mtext(paste("95% CI includes HIV prevalences of ", ci.l*100, "% to ", ci.u*100, "%", sep=""), side = 3, line =0)
arrows(ci.l, min.l + 4, ci.l, min.l, length = .2)
arrows(ci.u, min.l + 4, ci.u, min.l, length = .2)
text(ci.l, min.l + 4, ci.l,cex = 1,pos = 3)
text(ci.u, min.l + 4, ci.u,cex = 1,pos = 3)

## Compare confidence intervals calculated using binom.test() and using the Likelihood
## Ratio Test.
binom.test(samplePos, sampleSize, samplePos/sampleSize, alternative = "two.sided")

ci.likelihood

## Are the results similar?

## Task 3: We do another prevalence survey, sampling 200 people from the same population with a true HIV prevalence of 30%
## We believe that the two samples are independent of each other 
## This time we get 64 positives
samplePos2 <- 64
sampleSize2 <- 200

## The likelihood of a prevalence is the probability of observing the new data given the
## hypothesized prevalence:
likelihoods2 <- dbinom(samplePos2, sampleSize2, potential.prev.vector)

## The total likelihood of a prevalence is the probability of observing both these data points
## given the hypothesized prevalences 
totlikelihood <- likelihoods*likelihoods2
## Note: it's normally better practice to do all computations on the 
## log likelihood scale, so we could have equivalently taken the logs and then
## written totalLogLike <- logLikes + logLikes2
## But here we wanted to stay with total for a while to show a comparison curve

## Let's see what the total likelihood curve looks like:
plot(potential.prev.vector, totlikelihood, col = "red", type = "l", lwd = 2,
     xlab = "potential HIV prevalences", ylab = "likelihood",
     main = "p(our data given prevalence) = LIKELIHOOD",
     bty = "n")

## What are two differences that you notice about this curve? 

## The MLE of prevalence given both sets of data is: 
(samplePos+samplePos2)/(sampleSize+sampleSize2)

## We can derive a confidence interval for this estimate, in a similar way to the above, considering the total likelihood:
plot(potential.prev.vector[zoom.plot.index], -log(totlikelihood[zoom.plot.index]),
     type = "l", col = "red", col.main = "white", xlim = xlim, ylim=c(0,10),
     bty = "n", lwd = 3, xlab = "potential prevalences (our models)",
     ylab = "-log(likelihood)")
min.l <- min(-log(totlikelihood))
ci.likelihood <- range(potential.prev.vector[-log(totlikelihood) < min.l + chisq.crit/2])
abline(h = min.l + chisq.crit/2, lwd = 3, lty = 2)
## add title to the plot
ci.l <- signif(ci.likelihood[1],3)
ci.u <- signif(ci.likelihood[2],3)
mtext(paste("95% CI includes HIV prevalences of ", ci.l*100, "% to ", ci.u*100, "%", sep=""), side = 3, line =0)
arrows(ci.l, min.l + 4, ci.l, min.l, length = .2)
arrows(ci.u, min.l + 4, ci.u, min.l, length = .2)
text(ci.l, min.l + 4, ci.l,cex = 1,pos = 3)
text(ci.u, min.l + 4, ci.u,cex = 1,pos = 3)

## What do you notice about this confidence interval compared to the one sample confidence interval?

## Let us again compare confidence intervals calculated using binom.test() and using the Likelihood
## Ratio Test.
binom.test(samplePos+samplePos2, sampleSize+sampleSize2, (samplePos+samplePos2)/(sampleSize+sampleSize2), alternative = "two.sided")

ci.likelihood

## Are the results similar?

## Task 4: Consider the following questions:

## For this example, do you think it was worth the trouble constructing Likelihood based confidence intervals (CIs)?
## Under what circumstances might you use classical tests for CIs?
## Under what circumstances would likelihood-based tests be better for CIs?
