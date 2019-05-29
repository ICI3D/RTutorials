## Introduction to Likelihood
## Meaningful Modeling of Epidemiolgic Data, 2012
## (C) Steve Bellan, 2009-2015

## The code is made available under a Creative Commons Attribution 4.0 International License. You
## are free to reuse this code provided that you give appropriate credit, provide a link to the
## license, and indicate if changes were made. You may do so in any reasonable manner, but not in
## any way that suggests the licensor endorses you or your use. Giving appropriate credit includes
## citation of the original repository.

## Set margins for plotting.
par(mar=c(6,4,4,2))

## Say we are randomly sampling from a population of people in a town
## of size 1,000,000 and we sample 100 people from that population.
size <- 100
## Say that the true HIV prevalence in that population of size 1,000,000
## is 30%, i.e. 300,000 are HIV positive.
true.prev <- .3

## Sample from this distribution once 
## samp.pos <- rbinom(1,100,.3)

## You can use the above code to sample from this distribution, but
## we'll set it at 28 just so everyone is working on the same example
## for now.
samp.pos <- 28                          
samp.prev <- samp.pos/size
samp.pos
samp.prev

## Create a color vector for every possible number of HIV+ people we
## could have found when sampling 100 people (0:100 means 101
## possibilities). Then make the value we chose have a different
## color.
color.vec <- rep("black",size+1)
color.vec[samp.pos+1] <-	"purple"


### Create a histogram of the binomial distribution function with the
### true prevalence, highlighting the value we sampled.
barplot.obj <- barplot(dbinom(0:size,size=size, prob=true.prev),
                       xlab="number HIV+", names.arg=c(0:size),
                       ylab="probability", col=color.vec, border = NA, space =0)

## Given that we found samp.pos
samp.pos
## positives, would we accept or reject the hypothesis that the true prevalence is .2?
potential.prev <- .2

## To do this we use a two-tailed test. Meaning we are asking "If the
## true prevalence was .2, what is the probability of finding 28 HIV+
## people or another value as extreme when sampling 100 people?

## Now calculate the total probability of finding a value in the
## extreme lower tail plus the extreme upper tail.
if(samp.pos < potential.prev*size)
  {
    p.val.one.tail <- pbinom(samp.pos, size, potential.prev, lower.tail = TRUE)
  }else{
    p.val.one.tail <- pbinom(samp.pos - 1, size, potential.prev, lower.tail = FALSE)
  }
## Now multiply times 2, because we could have gotten reults that
## extreme in the other direction too. Make sure you understand what
## this means.
p.val <- p.val.one.tail*2



## Calculate the probability of getting every single possible value
## from the binomial distribtion with this hypothesized prevalence.
prob.dist <- dbinom(0:size, size=size, prob=potential.prev)
ymax <- .2
color.vec <- rep("grey",size)

## Create a logical vector for which values are more extreme
if(samp.pos < potential.prev*size)
  {
    more.extreme <- 0:100 <= samp.pos
  }else{
    more.extreme <- 0:100 >= samp.pos
  }

## Now create a color vector that's grey for all possible sampleable
## values (0-101) but red for the more extreme values.
color.vec <- rep("grey",size+1)
color.vec[more.extreme] <- "red"

## Plot the binomial distribution for N=100, and p=.2, and show the values more extreme than 28 in red
barplot(prob.dist,
        xlab = "number HIV+",
        names.arg = c(0:size),
        ylab = "probability",
        col = color.vec ,
        border = NA,
        main = paste("hypothetical prevalence:",potential.prev*100,"%"),
        ylim = c(0,ymax),
        space = 0)

## Remember this doubles the area in the red tail!
text(80, .75*ymax, paste("p-value =", signif(p.val,3)))

## Task 1: Edit the above code for the theoretical null hypotheses
## that the prevalences are .15,.25,.3,.35,.4, or .5

## Task 2: Repeat the above but change samp.pos to be 38 (i.e. pretend
## 38/100 tested HIV+)

######################################################################
## Now we're going to plot the above results on a single plot by
## showing the p-value for each null hypothesis.
######################################################################

## Create a vector of potential prevalences spanning 0-1 with 1000 values
## These are all potential "null hypotheses".
potential.prev.vector <- seq(0,1, length=10000)
## Create an empty vector to fill for the p.value at each
## corresponding prevalence value.
p.val <- rep(NA, length(potential.prev.vector))

## Now loop thrugh all the hypothesized prevalences, calculate the
## p-value, and store in our p.val vector.
for(jj in 1:length(potential.prev.vector))
  {
    ## Now calculate the total probability of finding a value in the
    ## extreme lower tail plus the extreme upper tail.
    if(samp.pos < potential.prev.vector[jj]*size)
      {
        p.val.one.tail <- pbinom(samp.pos, size, potential.prev.vector[jj], lower.tail = TRUE)
      }else{
        p.val.one.tail <- pbinom(samp.pos - 1, size, potential.prev.vector[jj], lower.tail = FALSE)
      }
    ## Now multiply times 2, because we could have gotten reults that
    ## extreme in the other direction too. Make sure you understand what
    ## this means.
    p.val[jj] <- p.val.one.tail*2
  }

## Which of these null hypothesized prevalences are not rejected at
## the alpha = .05 cutoff?
non.rejected.nulls <- potential.prev.vector[p.val > .05]

ci <- range(non.rejected.nulls)

## Compare this to the CI given by the R function binom.test.
ci
binom.test(28,100,.2, alternative = "two.sided")

## Plot the p-values vs the null hypotheses.
plot(potential.prev.vector, p.val, xlab = "hypothetical prevalence (null hypothesis)", ylab = "p-value",
     lwd = 2, bty = "n", type = "l", cex.lab = 1, col = "red")
## Draw a line at the alpha = .05 cutoff.
segments(0, .05, 1, .05, lty = 2, lwd = 3)

## Now plot the CI bounds
ci.l <- round(ci[1],3)
ci.u <- round(ci[2],3)
arrows(ci.l, .4, ci.l, -.03, length = .2)
arrows(ci.u, .4, ci.u, -.03, length = .2)
text(ci.l,.4, ci.l,cex = 1,pos = 3)
text(ci.u,.4, ci.u,cex = 1,pos = 3)

mtext(paste("95% CI includes HIV prevalences of ", ci.l*100, "% to ", ci.u*100, "%", sep=""), side = 3, line =0)

## Task 3: Write down in words what this plot is showing.

######################################################################
## Now let's do the same analysis but using Maximum Likelihood approach.
###################################################################### 


## The likelihood of a prevalence is the probability of observing the data given the
## hypothesized prevalence:
## L(prevalence | data) = p(data | prevalence)
likelihoods <- dbinom(samp.pos, size, potential.prev.vector)

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

## Now recall that we can use the Chi Squared Likelihood Ratio
## Test. If the logL of a given prevalence is within the chi-squared
## alpha=.05 cutoff of the minimum logL, then we do not reject it.
chisq.crit <- qchisq(.95, df = 1)
chisq.crit
min.l <- min(-log(likelihoods))
abline(h = min.l + chisq.crit/2, lwd = 3, lty = 2)

######################################################################
## It's hard to see this so let's zoom in on last plot for values of
## the potential prevalence between .15 and .5.

## Let's also make a 2 panel plot so we can compare this with the
## P-Value approach from above.
par(mfrow=c(2,1), mar = c(6,4,4,4))
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

## Redraw the P-Value plot from above

## Plot the p-values vs the null hypotheses.
plot(potential.prev.vector, p.val, xlab = "hypothetical prevalence (null hypothesis)", ylab = "p-value",
     xlim = xlim, lwd = 2, bty = "n", type = "l", cex.lab = 1, col = "red")
## Draw a line at the alpha = .05 cutoff.
segments(0, .05, 1, .05, lty = 2, lwd = 3)

## Now plot the CI bounds
ci.l <- round(ci[1],3)
ci.u <- round(ci[2],3)
arrows(ci.l, .4, ci.l, -.03, length = .2)
arrows(ci.u, .4, ci.u, -.03, length = .2)
text(ci.l,.4, ci.l,cex = 1,pos = 3)
text(ci.u,.4, ci.u,cex = 1,pos = 3)

mtext(paste("95% CI includes HIV prevalences of ", ci.l*100, "% to ", ci.u*100, "%", sep=""), side = 3, line =0)

## Compare confidence intervals constructed from cumulative
## probability distribution (i.e. p-values) and from the Likelihood
## Ratio Test.
ci
ci.likelihood

## Task 4: Explain in your own words how the Likelihood construction of p-values
