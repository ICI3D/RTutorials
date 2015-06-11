## Gillespie algorithm benchmark question: key for part (c)
## Meaningful Modeling of Epidemiological Data, 2012
## AIMS, Muizenberg
## Juliet Pulliam, 2011,2012

## The question leads you through the construction of a stochastic
## SIRS model using the Gillespie algorithm.
##
## This is example code for part (c).

rm(list=ls())

event <- function(time,S,I,R,N=pop.size,R0=basic.reproductive.ratio,rho=rate.of.waning){

  trans.rate <- R0*S*I/N
  recov.rate <- I
  loss.rate <- rho*R

  tot.rate <- trans.rate+recov.rate+loss.rate
  
  event.time <- time+rexp(1,tot.rate)
  
  dd <- runif(1)
  if(dd<trans.rate/tot.rate){
   	S <- S-1
   	I <- I+1
  }else{
  	if(dd<(trans.rate+recov.rate)/tot.rate){
  	  I <- I-1
  	  R <- R+1  	  
  	}else{
  	  R <- R-1
  	  S <- S+1
  	}
  }
  
  return(data.frame(time=event.time,S=S,I=I,R=R))
}


pop.size <- 1000
basic.reproductive.ratio <- 4
rate.of.waning <- 0.01

sim.time <- 0
ts <- data.frame(time=sim.time,S=pop.size-1,I=1,R=0)
next.time <- ts
while(next.time$time<15&next.time$I>0){
  next.time <- event(next.time$time,next.time$S,next.time$I,next.time$R)
  ts <- rbind(ts,next.time)
}

plot(ts$time,ts$I,type="l",xlab="Time",ylab="Number infectious",bty="n")
