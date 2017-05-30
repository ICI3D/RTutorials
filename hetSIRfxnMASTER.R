## Gillespie with individual mixing heterogeneity
## Steve Bellan DAIDD 2012

## The question leads you through the construction of a stochastic SIR
## model using the Gillespie algorithm that allows for gamma
## distributed contact mixing patterns.

rm(list=ls())


het.epidemic <- function(runs = 1,
                         pop.size = 300,
                         beta.mean = 1,
                         beta.var = .5,
                         gmma = 1,
                         end.time = 500,
                         verbose = F,
                         tell.run = T,
                         shape = 2,
                         rate = 10,
                         xmax.het = 10,
                         xmax.fsize = pop.size,
                         fsize.breaks = 30,
                         return.ts = F,
                         browse = F)
  {
    if(browse) browser()                #debugging
    theta = beta.var / beta.mean
    kk = beta.mean / theta
    mxdst <- rgamma(pop.size, shape = kk, scale = theta)
    ## order individuals by riskiness
    mxdst <- mxdst[rev(order(mxdst))]
    breaks <- seq(0, ceiling(max(mxdst)), by = .1)
    hist(mxdst, col="black", breaks = breaks, xlab = "contact rate (1/day)", las = 1,
         xlim = c(0, xmax.het),
         main = "distribution of average R")
    abline(v=beta.mean, col = "red")
    ## make individuals' data frame with mixing distr, time inf & rec
    iframe <- data.frame(id = 1:pop.size, tinf = NA, trec = NA, mxdst = mxdst)
    plot(0,0,type="l",xlab="days", bty="n", xlim = c(0, end.time), las = 1,
         ylim = c(0, 1.2*pop.size), ylab = "# people", main = "time series")
    legend("topright", c("Susceptible","Infected","Recovered"), col = c("black","red","blue"), lwd = 1, lty = 1, bty = "n", bg ="white")
    f.size <- rep(NA,runs)
    for(ii in 1:runs)
      {
        if(tell.run) print(paste("on run",ii,"of",runs))
        iframe$tinf <- NA
        iframe$trec <- NA
        first.inf <- sample(1:pop.size, 1, prob = mxdst)
        iframe$tinf[first.inf] <- 0 # first individual is infected at time 0
        iframe$trec[first.inf] <- rexp(1, gmma) # choose recovery time
        ## initialize time series data frame
        sim.time <- 0
        ts <- data.frame(time=sim.time,S=pop.size-1,I=1)
        next.time <- ts
        someone.inf <- T
        while(next.time$time<end.time & someone.inf)
          {
            ## calculate current inf rate  
            inf.rate <- mean(mxdst) * pop.size
            ## Choose potential event time
            new.time <- rexp(1, inf.rate) + next.time$time
            ## choose infector
            cur.tor <- sample(1:pop.size, 1, prob = mxdst)
            ## has this id been infected?
            cur.inf <- !is.na(iframe$tinf[cur.tor])
            ## if so, have they recovered before the event time?
            if(cur.inf)
              {
                cur.inf <- iframe$trec[cur.tor] > new.time
              }
            if(cur.inf)                           # then choose infected
              {
                cur.ted <- sample(c(1:pop.size)[-cur.tor], 1, prob = mxdst[-cur.tor])
                ## is this ID currently susceptible?
                cur.sus <- is.na(iframe$tinf[cur.ted])
                if(cur.sus) ## then infect them and determine an event time
                  {
                    if(verbose)   print(paste(cur.tor,"infected",cur.ted))
                    iframe$tinf[cur.ted] <- new.time
                    next.time$S <- next.time$S - 1
                    next.time$I <- next.time$I + 1
                    next.time$time <- new.time
                    ts <- rbind(ts, next.time)
                    ## also choose recovery time based on recovery rate
                    iframe$trec[cur.ted] <- new.time + rexp(1, gmma)
                  }else{ ## no new inf but update time
                    next.time$time <- new.time
                    ts <- rbind(ts, next.time)
                  }          
              }else{ ## no new inf but update time
                next.time$time <- new.time
                ts <- rbind(ts, next.time)
              }
            ## is someone still infected at this time?
            someone.inf <- sum(iframe$trec > new.time, na.rm = T) > 0
          }
        ## need to divid infected between recovered & infectious
        fts <- ts
        ## calculate recovered at each time step
                                        #browser()
        fts$R <- as.numeric(lapply(fts$time, function(x) {sum(iframe$trec < x, na.rm = T)}))
        fts$I <- fts$I-fts$R
        if(sum(fts$I < 0) > 1) browser()
        ## plot results
        lines(fts$time,fts$S, col = "black")
        lines(fts$time,fts$I, col = "red")
        lines(fts$time,fts$R, col = "blue")
        f.size[ii] <- sum(!is.na(iframe$tinf))
      }
    hist(f.size, breaks = fsize.breaks, xlab = "cumulative # infected (final size)", las = 1,
         xlim  = c(0, max(xmax.fsize,f.size)), ylim = c(0, runs), col = 'black',
         ylab = "frequency", main = "outbreak size distribution")
    ## final size distribution
    if(return.ts)       return(fts)
  }
save(het.epidemic, file= 'HetSIR_functions.Rdata')

het.epidemic(beta.mean = .8, beta.var = .1, runs = 30, end.time = 10, pop.size = 100, gmma = 1) 

hetSIRS.epidemic <- function(runs = 1,
                             pop.size = 300,
                             beta.mean = 1,
                             beta.var = .5,
                             gmma = 1,
                             rho = 0, # lose immunity
                             end.time = 5,
                             tell.run = T,
                             verbose = F,
                             shape = 2,
                             rate = 10,
                             xmax.het = 10,
                             xmax.fsize = 300,
                             return.ts = F,
                             browse = F)
  {
    theta = beta.var / beta.mean
    kk = beta.mean / theta
    mxdst <- rgamma(pop.size, shape = kk, scale = theta)
    ## order individuals by riskiness
    mxdst <- mxdst[rev(order(mxdst))]
    breaks <- seq(0, ceiling(max(mxdst)), by = .1)
    hist(mxdst, col="black", breaks = breaks, xlab = "heterogeneity",
         xlim = c(0, xmax.het),
         main = "distribution of average R")
    abline(v=beta.mean, col = "red")
    ## make individuals' data frame with mixing distr, time inf & rec
    iframe <- data.frame(id = 1:pop.size, tinf = NA, trec = NA, tloss = NA,mxdst = mxdst)
    plot(0,0,type="l",xlab="Time", bty="n", xlim = c(0, end.time),
         ylim = c(0, 1.2*pop.size), ylab = "# people", main = "time series")
    legend("topright", c("Susceptible","Infected","Recovered"), col = c("black","red","blue"), lwd = 1, lty = 1, bty = "n", bg ="white")
    f.size <- rep(NA,runs)
    if(browse) browser()                #debugging
    for(ii in 1:runs)
      {
        if(tell.run) print(paste("on run",ii,"of",runs))
        iframe$tinf <- NA
        iframe$trec <- NA
        first.inf <- sample(1:pop.size, 1, prob = mxdst)
        iframe$tinf[first.inf] <- 0 # first individual is infected at time 0
        iframe$trec[first.inf] <- rexp(1, gmma) # choose recovery time
        ## initialize time series data frame
        sim.time <- 0
        ts <- data.frame(time=sim.time,S=pop.size-1,I=1, R = 0)
        next.time <- ts
        someone.inf <- T
        cumulativeI <- 1
        while(next.time$time<end.time & someone.inf)
          {
            ## calculate current inf rate  
            inf.rate <- mean(mxdst) * pop.size
            ## Choose potential event time
            new.time <- rexp(1, inf.rate) + next.time$time
            ## figure out if anyone has recovered between last event and this one; I->R
            recoveries <- sum(iframe$trec > ts$time[nrow(ts)] & iframe$trec < new.time, na.rm = T)
            next.time$I <-  next.time$I - recoveries
            next.time$R <-  next.time$R + recoveries
            ## figure out if anyone has lost immunity between last event and this one; R->S
            who.loss <- which(iframe$tloss > ts$time[nrow(ts)] & iframe$tloss < new.time)
            ilosses <- length(who.loss)
            next.time$R <-  next.time$R - ilosses
            next.time$S <-  next.time$S + ilosses
            ## reset tinf,trec,tloss for individuals who lost immunity
            if(ilosses > 0)            iframe[who.loss, c("tinf","trec","tloss")] <- NA
            ## choose infec*tor*
            cur.tor <- sample(1:pop.size, 1, prob = mxdst)
            ## has this id been infected?
            cur.inf <- !is.na(iframe$tinf[cur.tor])
            ## if so, have they recovered before the event time?
            if(cur.inf)
              {
                cur.inf <- iframe$trec[cur.tor] > new.time
              }
            if(cur.inf)                 # then choose (potentially) infec*ted* individual
              {                         # choose from everyone except infec*tor*
                cur.ted <- sample(c(1:pop.size)[-cur.tor], 1, prob = mxdst[-cur.tor])
                ## is this ID currently susceptible?
                cur.sus <- is.na(iframe$tinf[cur.ted])
                if(cur.sus) ## then infect them and determine an event time
                  {
                    cumulativeI <- cumulativeI + 1
                    if(verbose)   print(paste(cur.tor,"infected",cur.ted))
                    iframe$tinf[cur.ted] <- new.time
                    next.time$S <- next.time$S - 1
                    next.time$I <- next.time$I + 1
                    next.time$time <- new.time
                    ts <- rbind(ts, next.time)
                    ## also choose recovery time based on recovery rate
                    iframe$trec[cur.ted] <- new.time + rexp(1, gmma)
                    ## also choose immunity loss time based on loss rate
                    iframe$tloss[cur.ted] <- iframe$trec[cur.ted] + rexp(1, rho) #
                  }else{ ## no new inf but update time
                    next.time$time <- new.time
                    ts <- rbind(ts, next.time)
                  }          
              }else{ ## no new inf but update time
                next.time$time <- new.time
                ts <- rbind(ts, next.time)
              }
            ## is someone still infected at this time?
            someone.inf <- sum(iframe$trec > new.time, na.rm = T) > 0
          }
        ## need to divid infected between recovered & infectious
        fts <- ts
        ## calculate recovered at each time step
        ## plot results
        lines(fts$time,fts$S, col = "black")
        lines(fts$time,fts$I, col = "red")
        lines(fts$time,fts$R, col = "blue")
        f.size[ii] <- cumulativeI
      }
    hist(f.size, breaks = 100, xlab = "cumulative # infected (final size)",
         xlim = c(0, max(xmax.fsize,f.size)), 
         ylab = "frequency", main = "outbreak size distribution", col = "black")
    ## final size distribution
    if(return.ts)       return(fts)
  }

save.image(file="hetSIR.Rdata")
