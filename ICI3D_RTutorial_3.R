## Tutorial 3: Probability Distributions and Control Structures
## Clinic on Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data Program
## African Institute for Mathematical Sciences, Muizenberg, Cape Town, RSA
##
## David M. Goehring 2004
## Juliet R.C. Pulliam 2008, 2009, 2019
## Steve Bellan 2010, 2012
##
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

###################################################################### 

## By the end of this tutorial you should...

## * Feel comfortable working with probability distributions 
## * Know how to use conditional structures
## * Be warned about the dangers of loop structures in R
## * Have tried your hands at synthesizing all the new material

######################################################################
## SECTION A. Probability Distributions in R
###################################################################### 

#################### 
## Probability distributions
#################### 
## So far the calculations we have been performing have been doable on
## a modern calculator (or on your basic phone calculator app), 
## so my assertion that R will make things easier
## for you has not seen any confirmation in experience.

## In terms of solving probability problems, R does not provide many
## tools that extend too far beyond your intuition. If you want to
## know the probability of rolling two sixes on two dice you do not
## need R to tell you that 1/6 times 1/6 is 1/36. The two exceptions
## might be the functions factorial() and choose(), which only appear
## on a minority of modern calculators and which can solve a problem
## for you once you know to reach for them. These functions are
## sufficiently self-explanatory with their help files to render a
## treatment of them here unnecessary. A third exception is
## simulation, which you will try your hand at in the Benchmark
## Questions, and which is an important component of mathematical modelling.

## Enter probability distributions. R is able to do in a few
## milliseconds what took hours of hunting in tables as recently as 30 years
## ago. If you understand what a probability distribution is, R will
## provide four different utilities for (nearly) any such distribution, as
## follows:

## * Generating individual density values or a plot of the density function
## * Generating random numbers drawn from the distribution
## * Providing cumulative probability for points along the distribution
## * Providing points along the distribution corresponding to a cumulative probability

## Each of these is an important part of the toolkit for working with
## probability distributions.

## Making the functions meet your needs 

## The different distributions have different corresponding functions
## in R, but aside from the particulars of their arguments, they 
## consist of four functions of the same general form.

## We will use the binomial distribution (which is a
## discrete distribution) as an example for each of our four methods,
## and you should be able to extend that experience to any
## distribution you like.

## As we have seen hints of before, R can generate random numbers from
## any distribution. For discrete distributions, creating a histogram
## of a large number of these values can give a quick picture of the
## distribution. To generate random numbers from the binomial
## distribution, use rbinom():

hist(rbinom(10000, 10, 1/3), freq=FALSE, breaks=-1:10)

## The arguments of rbinom() tell it how many numbers you want to
## generate, how many trials are used to generate the distribution,
## and what the probability of a success per trial is,
## respectively. The extra arguments of hist() tell it that we want it
## to give proportions rather than counts and then how to divide up
## the data into bars.

## This is an empirical estimation of the distribution. The R function
## that calculates the probability density function of the binomial
## distribution is dbinom(), and that will give the precise
## theoretical values we may need. What is the probability of having 7
## successes on 10 trials with a probability of success of 1/3?

dbinom(7, 10, 1/3)

## Which is the equivalent of about 1.6% of the time. By extension, we
## can generate the complete distribution with vector input to
## dbinom(), as in

dbinom(0:10, 10, 1/3)

## A useful application of rbinom() and dbinom() is to plot a curve of
## the density function over the histogram of observed values. For a
## discrete distribution such as this, the “curve” will be slightly
## misleading, but we will know what it means. Generating fewer random
## numbers this time,

hist(rbinom(500,10,1/3), freq=FALSE, breaks=-1:10)

curve(dbinom(ceiling(x), 10, 1/3), add=TRUE, col="red")

## The argument ceiling(x) resolves the fact that the x-values on the
## plot are continuous, not discrete. The correction is not needed for
## continuous distributions.

## Alternatively, you may use a paired histogram for comparing
## simulations to predicted densities for discrete distributions. The
## visualization used here is less elegant for discrete distributions
## but is more easily extended to continuous distributions.

## Using dbinom() to calculate cumulative probabilities for discrete
## distributions would be tedious, and we cannot easily calculate the
## cumulative probabilities using the equivalent density values from a
## continuous distribution (because it requires integration). The
## solution in R is to use a function, such as pbinom() for the
## binomial distribution, that calculates the cumulative density
## function (c.d.f.), that is, the function F (which here takes
## integer values).

# F(x) = P(X <= x)

## As a quick example, what is the probability of getting 2 or less
## with our chosen binomial distribution?

pbinom(2, 10, 1/3)

## The last function we need in our probability-distribution toolkit
## is qbinom(), the R function that calculates the inverse cumulative
## density function, F^-1 of the binomial distribution The c. d. f. and
## the inverse c. d. f. are related by

# p = F(x)
# x = F^-1(p)

## As a quick example, where is the 80% threshold in the cumulative
## probability distribution?

qbinom(.8, 10, 1/3)

## The c. d. f. and inverse c. d. f. are particularly useful when
## performing statistical tests.

## I do not know why the R language chose such counter-intuitive
## naming practices for its probability distribution functions. But
## let’s have some mnemonics:

# Function                           Prefix    Mnemonic 
#
# random numbers from dist.          r         random
# density of distribution            d         density
# cumulative probability             p         probability (is output)
# inverse cumulative probability     q         qumulative (with an “inversed “c”)

## Feel free to remember them in any way you like, of course.

#################### 
## Probability distributions galore
####################
## You may not be familiar with all of these distributions, but this
## list should prove helpful for future reference. Keep in mind that
## some of these distributions are continuous and some are discrete –
## this will affect the allowed inputs of the functions.

# Beta pbeta() qbeta() dbeta() rbeta() 
# Binomial pbinom() qbinom() dbinom() rbinom() 
# Cauchy pcauchy() qcauchy() dcauchy() rcauchy()
# Chi-square pchisq() qchisq() dchisq() rchisq()
# Exponential pexp() qexp() dexp() rexp()
# F pf() qf() df() rf()
# Gamma pgamma() qgamma() dgamma() rgamma()
# Geometric pgeom() qgeom() dgeom() rgeom()
# Hypergeometric phyper() qhyper() dhyper() rhyper()
# Logistic plogis() qlogis() dlogis() rlogis() 
# Log Normal plnorm() qlnorm() dlnorm() rlnorm()
# Negative Binomial pnbinom() qnbinom() dnbinom() rnbinom()
# Normal pnorm() qnorm() dnorm() rnorm()
# Poisson ppois() qpois() dpois() rpois()
# Student's t pt() qt() dt() rt()
# Uniform punif() qunif() dunif() runif()

###################################################################### 
## SECTION B. Control structures
###################################################################### 
## Now that you have gotten your proverbial feet wet by composing your
## own functions, you are ready to learn how to make your functions
## (or your scripts) more powerful by using control structures. The
## two main varieties of control structures are conditional structures
## and loop structures, but more important than what they are called
## is what they do...

## The semicolon, ";", allows you to put two or more commands
## together on a single line. The commands will be executed
## sequentially, just as if they had been written on separate lines.
## The best use of the semicolon is probably to combine lists of
## assignments into a single line, such as,

x <- 8; y <- 4; z <- 3
c(x, y, z) 

## Using the semicolon in this way will condense your code
## and can make it look cleaner if applied with moderation.
## In most situations, though, clarity is
## more important than compactness, so in general it is best not to
## string long lists of commands together on a single line using
## semicolons. Remember that the amount of whitespace (such as spaces,
## indentation, and empty lines) in your code will not affect its
## efficiency and that adding whitespace (and comments!) will often
## make your code easier to follow.

## Conditional structures

## “If you are under the age of 6, you cannot ride this ride.”

## “If you are available Friday night, please give me your number.”

## Needless to say, we are all familiar with conditional structures in
## casual speech, and conditional structures in R are not much
## different.

## The command ''if'' initiates a conditional structure. (Like function,
## it looks like a function, but is not a function.) What comes in
## parentheses after if is an expression of the condition that must be
## met for the remaining code to be executed. That is, it is a logical
## expression (meaning it should evaluate to TRUE or FALSE) which if
## TRUE tells R to execute the next line (or set) of code.

## A silly, but fully functional, example:

age <- 19

if(age >= 6){
	can.ride <- TRUE 
}

can.ride

## Here we obviously know the result before we start, but in a
## (just-slightly-less silly) function, we would not:

ride.test <- function(age)
{
	can.ride <- FALSE
	if(age >= 6){
		can.ride <- TRUE
	}
	return(can.ride)
}

ride.test(5)

ride.test(65)

## Note that the curly brackets, {}, play the same role as for
## conditional statements as they do for the function construct: they
## allow multi-line procedures. They can also make your code easier to
## follow, even for single-line procedures.

## As an aside, the above function can (and perhaps should) be written
## more simply as

ride.test <- function(age){
	return(age >= 6)
}

## The conditional structure becomes important when you want different
## functions or operations to be performed depending on some
## condition. This is something that you will often want.

## As an example, consider a function that measures central
## tendency. While you may want to avoid the influence of extremely
## large values by using the median, you may want to forgo it on very small
## samples, replacing the median with the mean. Let us say if we have fewer
## than 10 measurements in our sample we will resort to the mean:

central.tend <- function(x){                                     
	my.answer <- median(x)
	if(length(x) < 10){my.answer <- mean(x)}
	return(my.answer)
}

central.tend(c(1,1,3))

## This last example may have raised a red flag about efficiency – I
## determined the median of the data regardless of whether this value
## will ultimately be used in my output. Here, the cost is
## exceptionally low, because that calculation will only be irrelevant
## if there are fewer than 10 items in the vector, in which case it
## took trivially long to execute. But, you can see where there might
## be a problem.

## One solution is to create separate if statements for each
## condition, such as:

# if(x < 0) #followed by what should happen if x < 0

# if(x = 0) #followed by what should happen if x = 0

# if((x > 0) & (x < 1)) # followed by what should happen if x is between 0 and 1, exclusive

# if(x >= 1) # followed by what should happen if x >= 1

## Note that defining conditions which are not mutually exclusive may
## make your code hard to follow and therefore render your results
## unpredictable without detailed examination.

## Another solution, particularly useful for binary (either-or) cases,
## is to use the command else, which follows the if procedure and
## gives what to do if your conditional expression evaluates to
## FALSE. If you are careful about your use of brackets, you can nest
## if and else commands to create more complicated conditional
## structures.

## Here are some additional options for writing the central tendancy 
## function above. Using if and else, as described above:

central.tend2 <- function(x){                                     
	if(length(x) >= 10){
		my.answer <- median(x)
	}else{
		my.answer <- mean(x)
	}
	return(my.answer)
}

# and using the ifelse() control structure, which is similar but more compact:

central.tend3 <- function(x){                                     
	my.answer <- ifelse(length(x) >= 10, median(x), mean(x))
	return(my.answer)
}

## There are two potential big errors that may occur in implementing a
## conditional structure. One occurs if the conditional expression
## evaluates to NA or NaN – try this out yourself:

if(Inf - Inf > 0){ x <- 5 } ## BADCODE

## To make sure that your if statements don’t break like this, you will
## sometimes want to use the is.na() function, which returns TRUE if the
## value being evaluated is NA or NaN.

## The second big mistake that you can make is assuming that R will act
## on the elements of a conditional expression that is a vector. Try this:

if(c(T, F, F)){ 1 + 1 } ## BADCODE

## As the warning that is generated indicates, R will ignore all but
## the first element of the vector. On reflection, this makes sense
## because R wouldn’t know what to do with a vector of logical values
## – the if statement has no inputs or outputs, just a conditional
## procedure. In other words, you shouldn’t expect the if statement to
## behave like a function because it isn’t one.

#################### 
## Loop structures
#################### 
## R employs a number of common loop structures familiar to those of
## you with some programming experience. However, those of you without
## programming experience may actually have an edge here.

## The reason is that, in general, in mathematical programming it is
## much more efficient to perform what traditionally has been done
## with programming loops using vector calculations. You have already
## seen one of the basic methods used to replace loop structures, the
## apply() function.

######################################################################
######################################################################

## This concludes Tutorial III. The benchmark questions should help
## solidify your understanding of the material.

## The probability-distribution section of this tutorial was aided by
## information available at
## http://www.stat.umn.edu/geyer/5102/examp/rlook.html.

## If you are unfamiliar with or rusty on your understanding of the
## binomial distribtion, you may also want to work through the introductory
## Binomial Distribution tutorial, available here:
## https://github.com/ICI3D/RTutorials/blob/master/binomialDistribution.R?raw=true


###################################################################### 
## SECTION C. Benchmark Questions
###################################################################### 

#################### 
## 1. The standard medication used to treat some disease is known to
## produce a severe side effect in 20% of the patients treated. A
## pharmaceutical company develops a new drug to treat the disease and
## tests it on 9 patients. There are no side effects observed in these
## patients. 

## a. What is the probability of treating 9 patients without side
## effects using the standard medication?

## b. Generate a plot of the probability of observing no side effects
## in patients receiving treatment, as a function of the number of
## patients in the study, assuming the new drug causes the severe side
## effect with the same probability as the old.

## c. Add a horizontal red line at p = 0.025, visually depicting the
## number of patients minimally required for a study to show that the
## new medication produces side effects in fewer patients than the
## standard treatment (traditional statistical significance).

#################### 

#################### 
## 2. Re-do the suite of calculations and plots found in the Tutorial
## using the Poisson distribution (with lambda = 3) rather than the
## binomial distribution. (Hint: Make sure the histogram has an
## appropriate range with breaks.)
####################
