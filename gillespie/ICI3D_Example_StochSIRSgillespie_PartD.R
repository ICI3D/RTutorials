## Gillespie algorithm benchmark question: key for part (d)
## Meaningful Modeling of Epidemiological Data, 2012
## AIMS, Muizenberg
## Juliet Pulliam, 2011,2012
##
## The question leads you through the construction of a stochastic
## SIRS model using the Gillespie algorithm.
##
## This is example code for part (d).

rm(list=ls())

event <- function(time,S,I,R,N=pop.size,R0=basic.reproductive.ratio,rho=rate.of.waning,external=external.introduction.rate,final.time=MAXTIME){
  
  trans.rate <- R0*S*I/N + external
  recov.rate <- I
  loss.rate <- rho*R
  
  tot.rate <- trans.rate+recov.rate+loss.rate
  
  if(tot.rate==0){
    return(data.frame(time=final.time,S=S,I=I,R=R))
  }
  
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

MAXTIME <- 100
NUMRUNS <- 5

pop.size <- 1000
basic.reproductive.ratio <- 4
rate.of.waning <- 0.01 # Note: Try increasing this value to 0.05... what happens?

par(mfcol=c(2,1),mar=c(4,2,1,1))
plot(NA,NA,xlim=c(0,MAXTIME),ylim=c(0,pop.size/2),xlab="Time",ylab="Number infectious",bty="n")

# Version with external source of infection (external set to 1/5)
for(ii in 1:NUMRUNS){
  sim.time <- 0
  ts <- data.frame(time=sim.time,S=pop.size-1,I=1,R=0)
  next.time <- ts
  while(next.time$time<MAXTIME){
    next.time <- event(next.time$time,next.time$S,next.time$I,next.time$R,external=1/5)
    ts <- rbind(ts,next.time)
  }
  
  lines(ts$time,ts$I,col=ii)
}

plot(NA,NA,xlim=c(0,MAXTIME),ylim=c(0,pop.size/2),xlab="Time",ylab="Number infectious",bty="n")

# Version without external source of infection (external set to 0)
for(ii in 1:NUMRUNS){
  sim.time <- 0
  ts <- data.frame(time=sim.time,S=pop.size-1,I=1,R=0)
  next.time <- ts
  while(next.time$time<MAXTIME){
    next.time <- event(next.time$time,next.time$S,next.time$I,next.time$R,external=0)
    ts <- rbind(ts,next.time)
  }
  
  lines(ts$time,ts$I,col=ii)
}