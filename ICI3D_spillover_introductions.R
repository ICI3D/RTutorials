## Stochastic SIR simulation with spillover introductions
## Clinic on Dynamical Approaches to Infectious Disease Data
## International Clinics on Infectious Disease Dynamics and Data Program
## University of Florida, Gainesville, FL, USA
##
## Rebecca Borchering, 2015, updated 2018
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)

rm(list=ls())

N=50   # population size

# choose parameter values
parms=c(lambda=.02,      # spillover rate
        beta=.2,      	 # contact rate
        gamma=.1)        # recovery rate


# initiate counters
count.infections= 0
count.spillovers= 0

## Compartments:
## (S,I,R) = (susceptible, infectious,removed)

## Transitions:
## Event                           Change        										Rate
## Spillover (S)									 (S,I,R)->(S-1,I+1,R)							lambda*S/N
## Infection (S)                   (S,I,R)->(S-1,I+1,R)             beta*I*S/N
## Recovery/Removal (I)            (S,I,R)->(S,I-1,R+1)             gamma*I


event <- function(time,S,I,R,params){
  with(as.list(params),{
    
    # update rates
    rates <- c(spillover = ifelse(S>0,lambda*S/N,0), # no spillover infections if S depleted
               infect = beta*I*S/N,
               recover = gamma*I)
    
    totRate <- sum(rates)
    
    if(totRate==0){eventTime <- final.time}else{
      
      # calculate time until next event
      eventTime <- time+rexp(1,totRate)
      
      # choose type of event
      eventType <- sample(c("Spillover","Infect","Recover"),1,replace=F,prob=rates/totRate)
      
      # update compartments based on the event type
      switch(eventType,
             "Spillover" = {
               S <- S-1
               I <- I+1
               count.spillovers = count.spillovers+1
             },
             "Infect" = {
               S <- S-1
               I <- I+1
               count.infections = count.infections+1
             },
             "Recover" = {
               I <- I-1
               R <- R+1
             }
      )}
    
    return(data.frame(time=eventTime,S=S,I=I,R=R,
                      count.spillovers=count.spillovers,count.infections = count.infections))
  })
}

simulateSIR <- function(t,y,params){
  with(as.list(y),{
    ts <- data.frame(time=0,S=round(S),I=round(I),R=round(R),
                     count.spillovers=0,count.infections=0)
    nextEvent <- ts
    while(nextEvent$time<final.time){
      nextEvent <- event(nextEvent$time,nextEvent$S,nextEvent$I,nextEvent$R,params)
      ts <- rbind(ts,nextEvent)
    }
    return(ts)
  })
}

final.time=400
parms=parms

# run the simulation
tsTest <- simulateSIR(final.time,c(S=N-1,I=1,R=0),parms)

# plot the infectious individuals over time
plot(tsTest$time,tsTest$I,type='s',main="Number Infected", ylim=c(0,N),xlim=c(0,final.time),bty="n",
     xlab="Time",  ylab="", cex.main=2, cex.lab=1.5, cex.axis=1.25, lwd =2)

# plot all compartments over time
plot(tsTest$time,tsTest$S,type='s',ylim=c(0,N+20),bty="n",ylab="Number of individuals",xlab="time",lwd=2)
lines(tsTest$time,tsTest$I,type='s',col='red2',lwd=2)
lines(tsTest$time,tsTest$R,type='s',col='purple',lwd=2)
legend(x="topright",c("S","I","R"),
       lty=1,col=c("black","red2","purple"),bty="n",lwd=2)

# print the total number of infections
sum(tsTest$count.infections)

# print the number of spillover events
sum(tsTest$count.spillovers)
