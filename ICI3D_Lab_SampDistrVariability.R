## Simulations, Probability, and Sampling Distributions
## Meaningful Modeling of Epidemiologic Data, 2010
## AIMS, Muizenberg
## (C) Jim Scott, 2010

## Goal:  utilize simulation to gain a basic understanding 
## of probabilites, distributions, and variability
## 

## NOTE: The comments will guide you through the tutorial but you
## should make sure you understand what the code is doing. 

######################################################################
## Section 1: Binomial distribution
######################################################################

######################################################################
## You flip a fair coin 200 times.  What is the probability that the 
## number of heads you get is between 99 and 111 (i.e. 100-110)?
## Edit the following code to find the answer.

## The following code performs the simulation - 1 iteration = 
## flipping a coin 200 times. 10000 iterations are performed.
## Here, Heads is equivalent to getting 1 and tails is 
## equivalent to getting 0.  Just copy and paste.
  
i <- 1                     
sims<-rep(0,10000)
while( i <= 10000 ){
  flips<-sample(0:1,200,replace=TRUE)
  sims[i]<- sum(flips)
  i <- i + 1
}

## Number of iterations in which number of heads is greater than 
## or equal to ??? and less than or equal to !!! (edit the code
## and replace the ??? and the !!! with approriate values)

counts <- ifelse(sims>=??? & sims<=!!!,1,0)
sum(counts)

## Simulated probability

sum(counts)/10000

## Compare your result to the exact binomial probability 
## You'll need to enter the approriate range of values
## The '200' indicates the number of flips performed and the 
## 0.5 is the probability of a 'success' (i.e. getting a 'heads')
## You can type ?dbinom to get more information on the function
## dbinom

sum(dbinom(??:??, 200, 0.5))

## you can also use the normal distribution as an a approximation
## to the binomial distribution:  mean = n*p  sd = sqrt((n*p*(1-p)) 
## where n is the number of flips and p is the probability of getting 
## heads.  Compute the mean and sd and edit the following code:

pnorm(110.5,*mean*,*SD*) - pnorm(99.5,*mean*,*SD*)

######################################################################
## Section 2: Permutation Test
######################################################################

######################################################################
##  An experiment is conducted in order to determine if vitamin C
##  is protective against the common cold.  The following data were
##  observed:
##                        Cold    No Cold
##           
##		 Vitamin C     114      186
##
##        No Vitamin C     156      144 
##
##  
##  Calculate the observed relative risk: 
##	P(Cold | Vitamin C) \ P(Cold | No Vitamin C)
##   

RR <- (??/??)/(??/??)
RR

##  You should find that the relative risk is less than one.  This 
##  suggests that Vitamin C is a protective exposure (incidence is 
##  smaller in the exposed group compared to the unexposed group).
##
##  The goal of this permuation test is to determine the probability 
##  that we would observe a value at least as extreme as the one that 
##  was actually observed, ASSUMING that there is truly no relationship 
##  between the row variable and the column variable (here, disease
##  and exposure). If the probability is "large", then our result is 
##  consistent with our original assumption of no relationship.  If the 
##  probability is small, our result suggests that maybe we should 
##  reconsider our initial assumption.  In other words, maybe disease
##  and exposure are really related to each other.
##
##
##  First, calculate the margins of the above 2 x 2 table.  
##  Total number of participants

pop      <- (114+186+156+144)

##  Total number that got a cold

dis	 <- ???

##  Total number without a cold

nodis    <- pop-dis

##  Total number exposed to Vitatmin C

expo	 <- ???

##  Total number that were not exposed to vitamin C

noexp    <- pop - expo

##  To display the margins type the following:

dis
nodis
expo
noexp

##  Now, the idea is to generate a distribution of relative risks under the 
##  assumption that there truly is no association between exposure and 
##  disease.  First let's walk through one iteration.  We'll set up a vector 
##  of 0's and 1's (0 means no disease and 1 means diseased)
##  and randomly select 300 (the number of exposed  individuals).   

exp1     <-sample(c(rep(1,dis),rep(0,nodis)),expo)

## To see what happened type:

exp1

## This vector represents the diseased and non-diseased people 
## in the exposed group.  To see the total number of diseased people
## that were exposed type:

sum(exp1)

##  We have enough information to compute a simulated RR under our 
##  assumption of 'no association between disease and exposure.  The
##  following code does the job - before you enter it, make a prediction - 
##  what value do you expect it to be?
 
rr_1time = (sum(exp1)/expo)/((dis - sum(exp1))/noexp)

##  Display the relative risk

rr_1time


##  Now, we'll repeat the above process 100000 times to generate an 
##  entire distribution.  Make sure you run the entire next segment
##  of code.  (Don't forget the 'closed bracket' at the end!)

n<-100000
i <- 1
RRs<-rep(0,n)
a_cell<-rep(0,n)
while( i <= n ){
  exp_dis     <-sum(sample(c(rep(1,dis),rep(0,nodis)),expo))
  exp_nodis   <- expo - exp_dis
  noexp_dis   <- dis - exp_dis
  noexp_nodis <- noexp - noexp_dis

  RR<-(exp_dis/(exp_dis + exp_nodis))/(noexp_dis/(noexp_dis + noexp_nodis))

  RRs[i]<-RR
  a_cell[i]<-exp_dis
  i<-i+1
}

##  Lets take a look at the distribution that was generated.  What
##  do you think it will look like?  Will it be discrete or continuous?
##  What shape do you expect it to have?  

hist(RRs, breaks=20)

##  What do you think the average value of the distribution is?
##  Make a prediction, then calculate the mean.

mean(RRs)

##  To calculate the probability of getting an RR as small or smaller 
##  than 0.73077 (the observed RR you should have found) we need only
##  to count up the number of simulated RR's below 0.73077.  Run the
##  following lines of code to get it.

results<- ifelse(RRs <=0.73077,1,0)
sum(results)/n

##  What do you think?  Was our initial assumption valid, or is our
##  result consistent with the idea that Vitamin C helps to prevent
##  colds?