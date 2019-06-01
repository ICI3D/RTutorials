## Variability, Sampling Distributions and Simulation

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA

## (C) MMED community, 2019 www.ici3d.org

## We are going to _simulate_ a study and then analyze the data. Why?

## How much variability do we expect if we do the same experiment over and over?

## Does our fitting model work?
	## Did we make a mistake?
	## Does the dynamical model work?

## Linking dynamical models to data is hard!
	## For complicated models, we really want to test whether our machine works

## Is our approach robust?
	## What if our distributional assumptions aren't quite right?
	## What if our dynamical assumptions aren't quite right?

######################################################################

## Do HIV+ women produce fewer offspring?

## First cut: a cross-sectional study
## This measures only associations and we're worried about confounding
## Let's recruit women between the age of 20 and 30
	## Do we need to research the assumption that prevalence of sex is high in this group?
	## Should we ask people and exclude those who report 0 sexual partners?
	## NEEDS MORE DISCUSSION

set.seed(601)

makePop <- function(sampleSize, prev, fertProb, orPos)
{

	## Calculate fertility of pos women based on OR
	fertOdds <- fertProb/(1-fertProb)
	posFertOdds <- fertOdds*orPos
	posFertProb <- posFertOdds/(1+posFertOdds)

	hivStatus <- sample(
		c("Pos", "Neg"), size=sampleSize
		, replace=TRUE, prob=c(prev, 1-prev)
	)
	fertility <- rbinom(sampleSize
		, prob=ifelse(hivStatus=="Neg", fertProb, posFertProb)
		, size=1
	)

	return(data.frame(hivStatus, fertility))
}

## Now we have some data, and we pretend for a while we don't know where we got it
## Do a logistic regression using glm in R

## glm family binomial does a logistic regression (thinks on log odds scale) by default
## More realistic version would have covariates
# m <- glm(fertility ~ hivStatus + age + educ + cricketSupporter)

analyzePop <- function(pop){
	m <- glm(fertility ~ hivStatus, family="binomial", data=pop)
	co <- summary(m)$coef
	return(co[["hivStatusPos", ncol(co)]])
}

print(analyzePop(makePop(
	sampleSize = 5e3 , prev = 0.3 , fertProb = 0.2 , orPos = 0.8
)))

repSim <- function(numSims, sampleSize = 5e3
	, prev = 0.3 , fertProb = 0.2 , orPos = 0.8
)
{
	replicate(numSims, {
		pop <- makePop(sampleSize, prev, fertProb, orPos)
		return(analyzePop(pop))
	})
}

## Does this test actually work?

## Step 1: Do we get nominal coverage when we assume the null is true?
	## 1-α (typically 95%) of the time, if nothing is happening we shouldn't think we saw anything
	## 95% of the time the CI should contain 0

## We can switch between these for development or serious checking
numSims <- 1e4
numSims <- 1e1

pvec <- repSim(numSims=numSims, orPos=1)
mean(pvec < 0.05)
hist(pvec)

## Step 2: Do we get nominal coverage when we assume our hypothesis is true?
	## 95% of the time CI should contain the value we used for simulation

### Code below here added after lecture
### We really want to return CIs and P values from the 

newAnalyzePop <- function(pop){
	m <- glm(fertility ~ hivStatus, family="binomial", data=pop)
	co <- confint(m)["hivStatusPos", ]
	return(co)
	return(co[["hivStatusPos", ncol(co)]])
}

print(ciPop(makePop(
	sampleSize = 5e3 , prev = 0.3 , fertProb = 0.2 , orPos = 0.8
)))

## Step 3: How often will we see something clear assuming the hypothesis is true?
