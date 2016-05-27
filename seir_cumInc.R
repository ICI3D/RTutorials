# seir.R
# JRCP 06.05.09
# SEB 05.28.16
# Updated for 2016
#
# NOTE: The model and parameters used in this tutorial modified from:
#
# Earn et al. 2000 A Simple Model for Complex Dynamical Transitions in Epidemics.
# Science 287: 667-670.

beta.calc <- function(Ro,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  Ro/((sigma/(mu+sigma))*(1/(mu+gamma)))
}

S.star <- function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  N*((mu+sigma)/beta)*((mu+gamma)/sigma)
}
E.star <- function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  mu*(N-S.star(beta,N,mu,sigma,gamma))/(mu+sigma)
}
I.star <- function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  (sigma/(mu+gamma))*E.star(beta,N,mu,sigma,gamma)
}
R.star <- function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  (gamma/mu)*I.star(beta,N,mu,sigma,gamma)
}

N0 <- 500000

S.star(beta.calc(13),N0)+E.star(beta.calc(13),N0)+I.star(beta.calc(13),N0)+R.star(beta.calc(13),N0)

S.star(beta.calc(18),N0)
E.star(beta.calc(18),N0)
I.star(beta.calc(18),N0)
R.star(beta.calc(18),N0)

E.star(beta.calc(13),N0)+I.star(beta.calc(13),N0)

E.star(beta.calc(18),300000)+I.star(beta.calc(18),300000)

E.star(beta.calc(13),7200)+I.star(beta.calc(13),7200)
E.star(beta.calc(18),7200)+I.star(beta.calc(18),7200)

## NOTE: The rest of the script is used for the BONUS question.
## You are encouraged to continue commenting the script below this
## line but you should first complete benchmark questions 3 and
## 4 to ensure that you have mastered the script up to this point.

library(deSolve)

seir <- function(t,y,params){
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]
        CI <- y[5]
	
	beta <- params["beta"]
	N <- params["N"]
	mu <- params["mu"]
	sigma <- params["sigma"]
	gamma <- params["gamma"]
	nu <- mu*N

        newInfecteds <- beta*S*I/N
	dSdt <- nu-newInfecteds-mu*S
	dEdt <- newInfecteds-mu*E-sigma*E
	dIdt <- sigma*E-mu*I-gamma*I
	dRdt <- gamma*I-mu*R
        dCIt <- newInfecteds
		
	return(list(c(dSdt,dEdt,dIdt,dRdt, dCIt)))
}

## Note that N0 is defined earlier in the script as 500,000.

param.vals <- c(
  beta=beta.calc(18),
  N=N0,
  mu=.02/365.25,
  sigma=1/8,
  gamma=1/5
)

times <- seq(0,40*365,1)

S0 <- 20000
E0 <- 200
I0 <- 125
CI0 <- 0

## Stop and think about this. What is the level of immunity in
## the population for these initial conditions and a population
## size N0?

init <- c(sus=S0,exp=E0,inf=I0,rec=N0-S0-E0-I0, cumInc=0)

tc <- data.frame(lsoda(init,times,seir,param.vals))


## cumulative incidence
plot(tc$time/365,tc$CI0,type="l",xlab="Time (years)",ylab="cumulative incidence",bty="n")

calcIncidence <- function(tc, everyDays = 7) {
    weekTimes <- tc$time[tc$time %%7 ==0]
    cumIncAtWeeks <- tc[tc$time %in% weekTimes, 'cumInc']
    weeklyIncidence <- diff(cumIncAtWeeks)
    
    incDat <- data.frame(time = weekTimes[-1], incidence = weeklyIncidence)
    return(incDat)
}

incDat <- calcIncidence(tc, everyDays = 7)
head(incDat,50)
## plot incidence
plot(incDat$time/365, incDat$incidence, type="l",xlab="Time (years)",ylab="weekly incidence",bty="n")


plot(tc$time/365,tc$sus/N0,type="l",xlab="Time (years)",ylab="Proportion of population",bty="n",ylim=c(0,max(tc$sus/N0)))
text(50,1.1*S.star(beta.calc(18),N0)/N0,"S(t)")
lines(tc$time/365,(tc$exp+tc$inf)/N0,col="green")
text(50,0.2*S.star(beta.calc(18),N0)/N0,"E(t)+I(t)",col="green")

plot(tc$exp/N0,tc$sus/N0,type="l",xlab="EXP",ylab="SUS",bty="n")
points(E.star(beta.calc(18),N0)/N0,S.star(beta.calc(18),N0)/N0,pch="*",col="red")
points(E0/N0,S0/N0,col="blue")

plot(tc$inf/N0,tc$sus/N0,type="l",xlab="INF",ylab="SUS",bty="n")
points(I.star(beta.calc(18),N0)/N0,S.star(beta.calc(18),N0)/N0,pch="*",col="red")
points(I0/N0,S0/N0,col="blue")


