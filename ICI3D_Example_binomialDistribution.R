## Fun with one of my favorite distributions
## Clinic on Dynamical Approaches to Infectious Disease Data
## International Clinics on Infectious Disease Dynamics and Data Program
## University of Florida, Gainesville, FL, USA
##
## Jim Scott, 2012
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)


################################################################################
##
## By the end of this tutorial you should:
##
## * understand the terms: sample space, discrete, random variable, probability
##   distribution
## * know the characterisitics of a binomial process
## * know the binomial formula and how to use it
## * be able to utilize the functions dbinom, pbinom, and rbinom
## * be able to plot a binomial distribution in R
## * visualize how the parameters of the binomial distribution affect it.
## * realize that this text editor has no spell check capability and forgive me
##   for any typos I may have made!


# The binomial probability model is a discrete probability model that is
# commonly used in many applications, including modeling disease transmission.
# But, what IS it, exactly?

# The "discrete" part means that it produces random variables that have a finite
# number of possible values (e.g. integers).  The "probability model" part means
# that it provides us with two important pieces of infomation: the possible
# outcomes that may occur and the probabilities with which each outcome occurs.

# There are a number of common discrete probability models that exist, but the
# binomial model is used to model the number of successes that occur in fixed
# number of independent trials.  It's a good model for a process that: 1) has a
# fixed number of independent trials 2) has outcomes that can be categorized
# into either "success" or "failure" 3) has constant probability for "success"
# (i.e. the probability of a "success" doesn't change.

# Coin flipping is a common example of a binomial process.  Suppose I flip a
# coin 10 times. One flip represents one trial.  The number of trials is fixed
# (n=10).  There are only two outcomes.  The probability of a success (e.g.
# getting "tails") remains constant. The number of tails in 10 trials is a
# binomially distributed random variable.

# What's a random variable?  Well, before we talk about that, lets define one
# other term - sample space.  The sample space of a random phenomenon is the set
# of all possible outcomes that could occur.  Note that an outcome need not be
# numeric.  For example, consider tossing a coin one time.  The sample space is
# {heads, tails}.  If you were to
# toss the coin 2 times the sample space would consist of:
# S = {heads heads, heads tails, tails heads, tails tails}
# and we would say the size of the sample space is 4.

# A random variable maps each outcome in the sample space to a numeric value.
# For example, if I defined the random variable X as the number of tails that
# occurs in two flips of a coin, I would observe the following:
#
# outcome          X
# heads heads      0
# heads tails      1
# tails heads      1
# tails tails      2
#
# I could describe the distribution of X in the following manner:
#
# outcome      probability
# X = 0          0.25
# X = 1          0.50
# X = 2          0.25

# Also, notice that the random variable X is binomial because we have:  1) fixed
# number of independent trials (n=2), 2) either a "success" (tails) or failure
# occurs (not tails), and 3) the probability of a "success" (getting tails)
# remains constant. Here the probability of a success in any one trial is p=0.5

# If we suspect that a random variable is binomial, we can use the binomial
# formula to calculate the probability that X equals some specific value.  For
# example, if we didn't know that the probabiliy of getting 0 tails on 2 flips
# is 0.25, we could have used the following formula to obtain it:
#
# Binomial formula:
# P(X = x) = N!/[(x!)(N-x)!] * p^x * (1-p)^(N-x)
#
# You might be familiar with seeing N!/[(x!)(N-x)!] written as "N choose x"
#
# In the above equation:
# x = number of successes
# N = number of trials
# p = the probability of a success

# In our example, N = 2 and p = 0.5.  To calculate the probability that X = 0
# use:
# P(X = 0) = 2!/[(0!)(2-0)!] * 0.5^0 * (1-0.5)^(2-0)
#          = 1               * 1     * 0.25         = 0.25    (recall that 0! = 1)

# This has all been a preamble toward getting you prepared to use the binomial
# distribution in R.  To obtain the calculation above in R, you need only
# execute the following command:

dbinom(0,2,0.5)

# The syntax is pbinom(x, N, p)

# That one isn't too hard to calculate in your head, but suppose you wanted to
# know the probability of getting exactly 20 tails if you flipped a coin 50
# times.  Instead of calculating it with the formula (or a calculator), just use
# R.  Here, N=50, p is still 0.5. Run:

dbinom(20, 50, 0.5)

# If you'd like to determine the entire distribution when N=50 and p=0.5 you
# could type:

dbinom(0:50, 50, 0.5)

# Nice, but not that visually stimulating.  Let's plot it!


barplot(dbinom(0:50, size=50, prob=1/2),names.arg=0:50,ylab="Probability",
        main="Binomial Distribution, N=50, p=1/2",xlab="Number of Tails",
        col="light blue",ylim=c(0,.12))

# The binomial distribution has 2 parameters, N, the number of fixed trials, and
# p, the probability of a success.  The distribution is completely determined by
# these values, just like a normal distribution is completely determined by its
# paramters, the mean and the standard deviation.  To see how the distriution
# changes as p changes, try highlighting and running the following code (here
# we're holding N constant at 50):

binom <- function(p) {
  barplot(dbinom(0:50, size=50, prob=p),names.arg=0:50,ylab="Probability",
          main=paste0("Binomial Distribution, N=50, p=",p),xlab="Number of Tails",
          col="light blue", ylim=c(0,0.3))
  Sys.sleep(0.1)
}
ignore <- sapply((1:19)/20, binom)

# If you didn't see all the plots, try clicking the back arrow a few times in
# the plot window to see all the plots that R produced.


# To get an idea of how the binomial distribution can change when N varies try
# running the following code. Here p is set to 1/2, after you've run the code
# once, try changing p to a different value.

binom <- function(N) {
  barplot(dbinom(0:N, size=N, prob=1/2),names.arg=0:N,ylab="Probability",
          main=paste0("Binomial Distribution, N=",N,", p=1/2"),xlab="Number of Tails",
          col="light blue", ylim=c(0,0.5))
  Sys.sleep(0.1)
}
ignore <- sapply(1:20, binom)

# Again, if you didn't see all the plots, try clicking the back arrow a few
# times in the plot window to see all the plots that R produced.

# The binomial formula provides you with just the area of one bar.  To determine
# the area that is less than or equal to a specific value, for example, the
# probability that we get 20 or fewer tails in 50 flips, you could type:

sum(dbinom(0:20, 50, 0.5))

# or, equivalently

pbinom(20,50,0.5)

# The difference between these two commands is that dbinom sums up the areas of
# each bar you specify whereas pbinom sums up the bars that are less than or
# equal to the value of the first number in the parentheses

# To create a visual of this you could execute the following:

ints=0:50

barplot(dbinom(ints, size=50, prob=1/2),names.arg=0:50,ylab="Probability",
        main="Binomial Distribution, N=50, p=1/2",xlab="Number of Tails",
        col=ifelse(ints<=20,"blue","light blue"))

# The area of the dark blue bars is:

pbinom(20,50,0.5)

# You can also use R to simulate binomially distributed random variables.  For
# example, suppose N = 50 and p=1/2.  You'd expect to get 25 successes, but due
# to random chance you won't always get 25.  Sometimes you'll get a few more,
# sometimes a few less.  To simulate the outcome of 50 coin tosses, just type:

rbinom(1,50,1/2)

# If you'd like to similate the results of 1000 people flipping a coin 50 times
# type:

mysims <- rbinom(1000,50,1/2)
mysims

# the output isn't so useful in that form...try plotting it instead:

hist(mysims, freq=FALSE, breaks=seq(-0.5,50.5,1),ylim=c(0,0.2),col="grey",
     ylab="Number of occurences",xlab="Number of tails in 50 flips")

# Your distribution should look similar to the following, theoretical binomial
# distribution:

barplot(dbinom(0:50, size=50, prob=1/2),names.arg=0:50,ylab="Probability",
        main="Binomial Distribution, N=50, p=1/2",xlab="Number of Tails"
        ,ylim=c(0,0.2),col="light blue")

# click the back arrow in the Plots window to compare and contrast.  You should
# also
# notice that this particular binomial distribution is 'bell shaped' like the
# normal
# distribution.  As a rule of thumb, whenever N*p > 10 and N*(1-p) > 10, you can
# use a normal distribution to approximate the binomial distribution.
# Use mean = N*p and variance = N*p*(1-p).  In this case, mean = 50*0.5 = 25 and
# variance = 50*0.5*0.5 = 12.5.  The SD is sqrt(12.5) or 3.54.  To compare the
# approximated normal distribution to your simulated binomial data, you could
# type:

hist(mysims, freq=FALSE, breaks=seq(-0.5,50.5,1),ylim=c(0,0.2),col="grey",
     ylab="Number of occurences",xlab="Number of tails in 50 flips")
curve(dnorm(x,mean=25,sd=3.54), add=TRUE, col="blue",lwd=3)

# That concludes the tutorial!

# Refs:
# Introduction to Probability with R, Baclawski, Chapman & Hall, 2008
# Introduction to the Practice of Statistics ed 7, Moore, McCabe, & Craig, W.H.
# Freeman, 2012

# To test your skills try the following problems.
#
#
#
# Roulette - on a roulette wheel, there are 18 red numbers, 18 black numbers,
# and 2 green numbers.  The numbers 1 to 36 appear on the wheel along with 0 and # 00.
# The probability that the roulette ball lands on any one number is the same
# (i.e. 1/38).  Suppose you always bet on red, and you play 20 times.
#
#
# a) Determine the probability of each outcome (eg, winning 0 times, 1 time,
# etc)
# b) Determine the probability of winning at least 10 times
# C) Produce a plot of the probability distribution of the number of 'wins'
# d) Simulate the number of wins when you play roulette 20 times for 1000
# replications
# e) Make a histogram of your results from part d)
# f) In how many of your replications did you win at least 10 times?
#
# Answers below
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# a)
dbinom(0:20,20,18/38)

# b)
sum(dbinom(10:20, 20,18/38))
# or
1 - pbinom(9,20,18/38)

# C)
barplot(dbinom(0:20, size=20, prob=18/38),names.arg=0:20,ylab="Probability",
        main="Binomial Distribution, N=20, p=18/38",xlab="Number of Tails"
       ,ylim=c(0,0.2))

# d)
data1<-rbinom(1000,20,18/38)
data1

# e)
hist(data1)

# f)
length(data1[data1>9])
#or
sum(ifelse(data1>9,1,0))
#or
sum(1*(data1>9))
#or just sort and count them up
sort(data1)
