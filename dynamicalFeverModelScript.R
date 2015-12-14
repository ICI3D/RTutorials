## Dynamical Fever: computer exercise
## Clinic on Meaningful Modeling of Epidemiological Data &
## Clinic on Dynamical Approaches to Infectious Disease Data
## International Clinics on Infectious Disease Dynamics and Data Program
##
## Juliet R.C. Pulliam, 2012-2015
## Some Rights Reserved
## CC BY-NC 3.0 (http://creativecommons.org/licenses/by-nc/3.0/)

rm(list=ls())                   # Clear all variables and functions

SEED <- 29121012
set.seed(SEED)

## SIR.CB() -- Function that runs a generation-based (stochastic Reed-Frost) chain binomial
## SIR model for a given population size and R0 value, outputting a data frame with
## columns representing, time, the number of susceptibles at each time, and the number of
## cases at each time. The chain binomial is a stochastic model, so the output varies
## between function calls, even for the same parameter values and initial conditions.
sir.ex1.cb <- function(VaxPct.Pop1=0,
                       VaxPct.Pop2=0,
                       R0=2,
                       N1=1000,
                       N2=1000,
                       MAXTIME=30,
                       I1.0=1,
                       pp=0.002){

  qq.1 <- 1-R0/(N1-1)  # Pairwise probability of avoiding potentially infectious contact (Pop1)
  qq.2 <- 1-pp        # Pairwise probability of avoiding potentially infectious contact (Pop2)

  Cases.Pop1 <- I1.0
  Sus.Pop1 <- max(0,N1-I1.0-round(VaxPct.Pop1*N1/100))
  FOI.Pop1 <- (1-qq.1^Cases.Pop1)

  Cases.Pop2 <- 0
  Sus.Pop2 <- N2-round(VaxPct.Pop2*N2/100)
  FOI.Pop2 <- (1-qq.2^Cases.Pop1)

  for(Time in 1:MAXTIME){

    Cases.Pop1 <- c(Cases.Pop1,rbinom(1,Sus.Pop1[Time],FOI.Pop1[Time]))
    Sus.Pop1 <- c(Sus.Pop1,Sus.Pop1[Time]-Cases.Pop1[Time+1])
    FOI.Pop1 <- c(FOI.Pop1,(1-qq.1^Cases.Pop1[Time+1]))

    Cases.Pop2 <- c(Cases.Pop2,rbinom(1,Sus.Pop2[Time],FOI.Pop2[Time]))
    Sus.Pop2 <- c(Sus.Pop2,Sus.Pop2[Time]-Cases.Pop2[Time+1])
    FOI.Pop2 <- c(FOI.Pop2,(1-qq.2^Cases.Pop1[Time+1]))
  }
  return(data.frame(Time=0:MAXTIME,Cases.Pop1,Sus.Pop1,Cases.Pop2,Sus.Pop2,FOI.Pop1,FOI.Pop2))
}

run.example <- function(VaxPct.Pop1=0,VaxPct.Pop2=0){
  sir.ex1.cb(VaxPct.Pop1,VaxPct.Pop2)
}

plot.cases <- function(epi=run.example()){
  barplot(epi$Cases.Pop1,names.arg=epi$Time,
          cex.lab=2,
          ylim=c(0,200),
          cex.axis = 1.1,
          xlab='Time',
          ylab='Cases',
          cex.main = 1.5,
          main='Dogs',
          xaxt='n') -> bb
  axis(1,bb[seq(0,30,5)+1],seq(0,30,5),cex.axis = 1.1,xpd=T,line=.2)
  barplot(epi$Cases.Pop2,names.arg=epi$Time,
          cex.lab=2,
          ylim=c(0,200),
          cex.axis = 1.1,
          xlab='Time',
          ylab='Cases',
          cex.main = 1.5,
          main='People',
          xaxt='n') -> bb
  axis(1,bb[seq(0,30,5)+1],seq(0,30,5),cex.axis = 1.1,xpd=T,line=.2)
}

plot.example <- function(epi=run.example(),plot.Re=FALSE){
  if(plot.Re){
    par(mar=c(5,5,2,1),mfrow=c(3,2))
  }else{
    par(mar=c(5,5,2,1),mfrow=c(2,2))
  }
  plot.cases(epi)
  plot(epi$Time,epi$FOI.Pop1,
       type='s',      # Use a 'step' plot because time is treated as discrete
       bty='n',
       lwd=2,
       cex.lab=2,
       ylim=c(0,1),
       cex.axis = 1.1,
       xlab='Time',
       ylab='Force of Infection',
       cex.main = 1.5,
       main='')
  plot(epi$Time,epi$FOI.Pop2,
       type='s',      # Use a 'step' plot because time is treated as discrete
       bty='n',
       lwd=2,
       cex.lab=2,
       ylim=c(0,1),
       cex.axis = 1.1,
       xlab='Time',
       ylab='Force of Infection',
       cex.main = 1.5,
       main='')
  if(plot.Re){
    plot(epi$Time,epi$FOI.Pop1*epi$Sus.Pop1/epi$Cases.Pop1,
         type='p',      # Use a 'step' plot because time is treated as discrete
         bty='n',
         lwd=2,
         col=2,
         cex.lab=2,
         ylim=c(0,2),
         cex.axis = 1.1,
         xlab='Time',
         ylab=expression(R[effective]),
         cex.main = 1.5,
         main='')
    abline(h=1)
    lines(epi$Time,2/999*epi$Sus.Pop1,col=4)
  }
}

data.2008 <- run.example()
data.2009 <- run.example()
data.2011 <- run.example()
data.2010 <- run.example()
data.2012 <- run.example(40,0) # Dog vaccine developed - 40 pct dogs vaxed
data.xxxx <- run.example(80,0) # Dog vaccine coverage up to 80 pct
data.2013 <- run.example(50,0) # Dog vaccine coverage down to 50 pct
data.2014 <- run.example(20,50) # Human vaccine developed - 50 pct coverage, dog coverage down to 20
data.2015 <- run.example(0,80) # Human vaccine coverage up to 80 percent


total.cases <- function(epi=run.example()){
  c(Dogs=sum(epi$Cases.Pop1),People=sum(epi$Cases.Pop2))
}

epi.duration <- function(epi=run.example()){
  c(Dogs=diff(range(epi$Time[epi$Cases.Pop1>0]))+1,People=diff(range(epi$Time[epi$Cases.Pop2>0]))+1)
}



plot.example(run.example(VaxPct.Pop1=40))
plot.example(run.example(VaxPct.Pop2=40))

vax.eff <- function(VAXPCT,POP,REPS=100){
  switch(POP,
         Pop1 = {
           replicate(REPS,sum(run.example(VaxPct.Pop1=VAXPCT)$Cases.Pop1))
         },
         Pop2 = {
           replicate(REPS,sum(run.example(VaxPct.Pop2=VAXPCT)$Cases.Pop2))
         }
  )
}


CasesByVax1 <- sapply(seq(0,100,10),vax.eff,POP='Pop1')
CasesByVax2 <-sapply(seq(0,100,10),vax.eff,POP='Pop2')

CasesByVax1.mean <-apply(CasesByVax1,2,mean)
CasesByVax2.mean <-apply(CasesByVax2,2,mean)
CasesByVax1.median <-apply(CasesByVax1,2,median)
CasesByVax2.median <-apply(CasesByVax2,2,median)

par(mfcol=c(2,1),bty='n',pch=19,cex.main=1.5,cex.axis=1.1,cex.lab=1.2,xpd=T)
plot(seq(0,100,10),CasesByVax1.mean,
     main='Population 1',
     xlab='Percent vaccinated',
     ylab='Number of cases',
     ylim=c(0,1000))
points(seq(0,100,10),CasesByVax1.median,col='red',cex=.7)
plot(seq(0,100,10),CasesByVax2.mean,
     main='Population 2',
     xlab='Percent vaccinated',
     ylab='Number of cases',
     ylim=c(0,1000))
points(seq(0,100,10),CasesByVax2.median,col='red',cex=.7)

par(omd=c(0,1,.5,1),mar=c(4,4.5,4,1))
boxplot(CasesByVax1,boxwex=.5,pch=16,cex=.5,names=seq(0,100,10),
        main='Dogs',cex.main=1.5,
        xlab='Percent vaccinated',cex.lab=1.2,
        ylab='Number of cases')
points(1:11,CasesByVax1.mean,col='red',cex=.7,pch=16)
par(omd=c(0,1,0,.5),new=T)
boxplot(CasesByVax2,boxwex=.5,pch=16,cex=.5,names=seq(0,100,10),
        main='People',cex.main=1.5,
        xlab='Percent vaccinated',cex.lab=1.2,
        ylab='Number of cases')
points(1:11,CasesByVax2.mean,col='red',cex=.7,pch=16)


save.image('Ex1.Rdata')
