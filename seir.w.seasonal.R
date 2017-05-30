# seir.w.seasonal.R
# JRCP 06.05.09

library(deSolve)

Ro.calc<-function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  beta*N*(sigma/(mu+sigma))*(1/(mu+gamma))
}

Ro.calc(0.0000009,500000)

beta.calc<-function(Ro,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  Ro/(N*(sigma/(mu+sigma))*(1/(mu+gamma)))
}

seir.earn<-function(t,y,params){
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]
	
	beta<-params[1]
	N<-params[2]
	mu<-params[3]
	sigma<-params[4]
	gamma<-params[5]
	nu<-mu*N
	
	dSdt <- nu-beta*S*I-mu*S
	dEdt <- beta*S*I-mu*E-sigma*E
	dIdt <- sigma*E-mu*I-gamma*I
	dRdt <- gamma*I-mu*R
		
	return(list(c(dSdt,dEdt,dIdt,dRdt)))
}

beta<-beta.calc(18,500000)#0.0000009#2.2*10^-8#7.2*10^-6#3.6#2*10^-8
N<-500000
mu<-.02/365.25
sigma<-1/8
gamma<-1/5

times<-seq(0,500,.01)

if(1){
  S0<-N/10
  E0<-N*10^-4
  I0<-N*10^-4
}else{
  S0<-N-1
  E0<-1
  I0<-0
}

init<-c(sus=S0,exp=E0,inf=I0,rec=N-S0-E0-I0)

tc<-data.frame(lsoda(init,times,seir.earn,c(beta,N,mu,sigma,gamma)))

par(mfcol=c(2,2))
plot(tc$time,tc$sus,type="l")
plot(tc$time,tc$exp,type="l")
plot(tc$time,tc$inf,type="l")
plot(tc$sus,tc$inf,type="l")

seir<-function(t,y,params){
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]
	
	nu<-mu*N
	
	dSdt <- nu-beta*S*I-mu*S
	dEdt <- beta*S*I-mu*E-sigma*E
	dIdt <- sigma*E-mu*I-gamma*I
	dRdt <- gamma*I-mu*R
		
	return(list(c(dSdt,dEdt,dIdt,dRdt)))
}
init<-c(sus=S0,exp=E0,inf=I0,rec=N-S0-E0-I0)

tc<-data.frame(lsoda(init,times,seir,NA))



S.star<-function(beta,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  ((mu+sigma)/beta)*((mu+gamma)/sigma)
}
E.star<-function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  mu*(N-S.star(beta,mu,sigma,gamma))/(mu+sigma)
}
I.star<-function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  (sigma/(mu+gamma))*E.star(beta,N,mu,sigma,gamma)
}
R.star<-function(beta,N,mu=0.02/365.25,sigma=1/8,gamma=1/5){
  (gamma/mu)*I.star(beta,N,mu,sigma,gamma)
}



# Seasonal beta
beta.t<-function(x,beta,amp=0.08,per=365.35){
  beta+beta*amp*cos(x*(2*pi/per))
}

seir.seasonal<-function(t,y,params){
	S <- y[1]
	E <- y[2]
	I <- y[3]
	R <- y[4]
	time<-y[5]
	
	beta<-params[1]
	N<-params[2]
	mu<-params[3]
	sigma<-params[4]
	gamma<-params[5]
	nu<-mu*N
#	beta.t<-beta+0.08*sin(time*(2*pi/365.25))#*sin(time*(2*pi/365.25))
	
	dSdt <- nu-beta.t(time,beta)*S*I-mu*S
	dEdt <- beta.t(time,beta)*S*I-mu*E-sigma*E
	dIdt <- sigma*E-mu*I-gamma*I
	dRdt <- gamma*I-mu*R
	dtimedt<-1
		
	return(list(c(dSdt,dEdt,dIdt,dRdt,dtimedt)))
}

beta<-beta.calc(18,500000)
N<-500000
mu<-.02/365.25
sigma<-1/8
gamma<-1/5

#times<-seq(0,365.25*20,.01)
times<-seq(0,365.25*50,.1)

if(1){
  S0<-N/10
  E0<-N*10^-4
  I0<-N*10^-4
}else{
  S0<-N-1
  E0<-1
  I0<-0
}

init<-c(sus=S0,exp=E0,inf=I0,rec=N-S0-E0-I0,day<-0)

tc<-data.frame(lsoda(init,times,seir.seasonal,c(beta,N,mu,sigma,gamma),hmax=.2))

par(mfcol=c(2,1))
plot(tc$time/365.25,tc$inf/N,type="l",xlab="Time in years",ylab="Proportion infectious",main="SEIR model with seasonal forcing")
#plot(tc$time/365.25,tc$inf/N,type="l",ylim=c(0,.01),xlab="Time in days",ylab="Proportion infectious")
#plot(tc$time/365.25,log10(tc$inf/N),type="l",xlab="Time in days",ylab="Log10(proportion infectious)")
plot(tc$time/365.25,sqrt(tc$inf/N),type="l",xlab="Time in days",ylab="SQRT(proportion infectious)")

par(mfcol=c(1,1))
plot(tc$time/365.25,tc$inf/N,type="l",xlab="Time in years",ylab="Proportion infectious",main="SEIR model with seasonal forcing")
plot(tc$time/365.25,tc$inf/N,type="l",xlab="Time in years",ylab="Proportion infectious",main="SEIR model with seasonal forcing",xlim=c(35,50))
