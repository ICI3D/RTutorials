library(shellpipes)
startGraphics()

## The most common way to report false positive rate is
## as 1 - test “specificity”
popPredict <- function(sens, spec, prev){
	true <- sens*prev
	false <- (1-spec)*(1-prev)
	return(true/(true+false))
}
## Test function on already solved problem
print(popPredict(1, 0.95, 0.01))
## Test on new prevalence
print(popPredict(1, 0.95, 0.10))

print(popPredict(1, 0.975, 0.01))

print(popPredict(0.95, 0.975, 0.01))
