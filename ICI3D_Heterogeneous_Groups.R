library(tibble) ## Use slightly better data frames (in function code)

######################################################################

## Pick values from a gamma that would achieve k_l in the limit
makeQuantiles <- function(n, k_l){
	probs <- ((1:n)-1/2)/n
	return(qgamma(
		p=probs, shape=1/k_l
	))
}

## Calculate kappa for a set of quantiles (implicitly, equal-sized groups)
qKappa <- function(q, goal=0){
	return(sum(q^2)*length(q)/sum(q)^2 - 1)
}

## Compare quantiles to a goal value of kappa
compKappa <- function(n, k_l, goal=0){
	return(qKappa(makeQuantiles(n, k_l)) - goal)
}

## Make groups with a desired achieved kappa and mean
## NOTE: Would love to have a more principled upper bound here
makeGroups <- function(n, m, kappa){
	if (kappa==0) return(rep(m, n))
	mult <- 10
	kmax <- 100
	up <- mult*kappa*kmax/(mult*kappa+kmax)
	if(compKappa(n, up) < kappa ){
		stop("Cannot generate enough variance in makeGroups; try more groups (or lower kappa)")
	}
	k_l <- uniroot(compKappa, lower=kappa, upper=up, n=n, goal=kappa)$root
	raw <- makeQuantiles(n, k_l)
	return(raw*m/mean(raw))
}

## A function to convert hazard to probability for making starting conditions
hazProb <- function(h){
	return(1 - exp(-h))
}

######################################################################

## This code is terse and tricky, but probably OK if total population size 
## (including absent R class) = 1.
## Will be better to add N explicitly as a parameter
groupSIR <- function(t, y, parms){
	with(parms, {
		n <- length(cvec)
		S = y[1:n]
		I = y[n+(1:n)]
		Lambda = sum(I*cvec)/mean(cvec)
		trans = Lambda*S*cvec
		return(list(c(-trans, trans-I)))
  })
}

## ADD ... args for lsoda
groupSim <- function(cbar, kappa, Tfinal=20, nGroups=10, h0=1e-3, steps=100){
	groups <- 1:nGroups
	cvec <- makeGroups(n=nGroups, m=cbar, kappa=kappa)
	I0 <- hazProb(h0*cvec/mean(cvec))
	sim <- lsoda(
		y = c(1/nGroups-I0, I0) 
		, times = (0:steps)*Tfinal/steps
		, func = groupSIR
		, parms = list(cvec=cvec)
	)
	Smat <- sim[, 1 + groups]
	Imat <- sim[, nGroups + 1 + groups]
	return(tibble(time = sim[,1]
		, S = rowSums(Smat)
		, cS = (Smat |> sweep(2, FUN="*", as.array(cvec)) |> rowSums())/S
		, I = rowSums(Imat)
		, cI = (Imat |> sweep(2, FUN="*", as.array(cvec)) |> rowSums())/I
	))
}

groupSimPlot <-  function(cbar, kappa, Tfinal=20, nGroups=10, h0=1e-3, steps=100, desc="blank"){
	sim <- groupSim(cbar, kappa, Tfinal, nGroups, h0, steps)

	title <- paste("cbar =", cbar, "kappa =", kappa)
	
	(ggplot(sim)
		+ aes(x=time)
		+ geom_line(aes(y=I))
		+ geom_line(aes(y=S), color="blue")
		+ ylab("proportion of pop")
		+ ggtitle(title)
	)
}

