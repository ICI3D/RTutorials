library(shellpipes)
startGraphics()
library(ggplot2); theme_set(theme_bw(18))

## a visual comparison
print(ci.likelihood)
bt <- binom.test(samplePos, sampleSize, samplePos/sampleSize, alternative = "two.sided")
cicomp <- data.frame(test=c("exact", "mle")
	, est = c(bt$estimate, bt$estimate)
	, lower = c(bt$conf.int[[1]], ci.likelihood[[1]])
	, upper = c(bt$conf.int[[2]], ci.likelihood[[2]])
)
###
print(ggplot(cicomp)
	+ aes(x = est, y = test)
	+ geom_linerange(aes(xmin = lower, xmax = upper))
	+ geom_point(size = 3)
	+ labs(x = "Probability", y = NULL)
)

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

## New spec
print(popPredict(1, 0.975, 0.01))

## New sens
print(popPredict(0.95, 0.975, 0.01))
