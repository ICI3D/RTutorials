
## The most common way to report false positive rate is
## as 1 - test “specificity”

## Since we are assuming a positive test,
## we construct a function that figures out the probability that it is a true positive
## the logic is: we know it's positive, so we want to calculate the relative probability of true and false positives
popPredict <- function(sens, spec, prev){
	true <- sens*prev
	false <- (1-spec)*(1-prev)
	return(true/(true+false))
}

## Test the function on the-already solved problem
print(popPredict(1, 0.95, 0.01))
## Nice

## What happens if we increase prevalence? 
print(popPredict(1, 0.95, 0.10))
## The probability of true positive gets higher
## Talk to JD about odds lens on this problem if you are curious

## Go back to the original prevalence
## What happens if we increase specificity?
print(popPredict(1, 0.975, 0.01))
## Again, the probability of true positive gets higher.
## This makes sense; you're less likely to be a false positive if fewer virus-negative people test positive

## What happens is we _decrease_ sensitivity (starting from previous values)?
print(popPredict(0.95, 0.975, 0.01))
## The probability our positive is true goes down (as we would expect) -- but the change is very small!
## What's going on here? Does it means very high sensitivity doesn't matter, or is there something we're missing?

