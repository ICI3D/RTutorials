##################################################
## Introduction to Sampling & Variability
## Steve Bellan
## Meaningful Modeling of Epidemiological Data 2015
## African Institute of Mathematical Sciences, Muizenberg, South Africa

## Pick a research question:
## 
## Does having HIV affect TB

set.seed(1)
n <- 200 ## sample size
hivstatus <- rep(c(0,1), each = n/2)
table(hivstatus)
tbIncRate <- c(.05, .15)
myDat <- data.frame(hiv = hivstatus, tb = NA)
numPerms <- 999

myDat$tb <- rbinom(n, 1, prob = rep(tbIncRate, each = n/2))
xtabs(~hiv + tb, myDat)
idr <- function(dat) with(dat, mean(tb[hiv==1])/mean(tb[hiv==0]))
Null_IDR_Vector <- rep(NA, numPerms)
for(ii in 1:numPerms) {
    permDat <- myDat
    permDat$tb <- sample(permDat$tb, n)
    xtabs(~hiv + tb, permDat)
    Null_IDR_Vector[ii] <- idr(permDat)
}
idr(myDat)
Full_IDR_Vector <- c(Null_IDR_Vector, idr(myDat))
par('ps' = 24)
hist(log(Full_IDR_Vector), 
     xlab = 'null IDR', ylab = 'frequency', bty = 'n',
     col = 'gray', main = '', xaxt='n')
axis(1, at = log(c(.2,.5,1,2,5)), lab = c(.2,.5,1,2,5))
abline(v = log(idr(myDat)), col = 'red', lwd =2)
2*min(mean(Full_IDR_Vector >= idr(myDat)), mean(Full_IDR_Vector <= idr(myDat)))


pValFxn <- function(FullVector, ObsVal)
    2*min(mean(FullVector >= ObsVal), mean(FullVector <= ObsVal))

trueIDRvals <- seq(1, 5, l = 10)
noHIV_tb_inc <- .05
n <- 500 ## sample size
hivstatus <- rep(c(0,1), each = n/2)
numSims <- 1000
numPerms <- 999
powerChiSqVector <- rep(NA, length(trueIDRvals))

for(rr in 1:length(trueIDRvals)) {
    print(paste0('on IDR ', rr, ' of ', length(trueIDRvals)))
    trueIDR <- trueIDRvals[rr]
    ## Power Analysis
    tbIncRate <- c(noHIV_tb_inc, trueIDR * noHIV_tb_inc)
    myDat <- data.frame(hiv = hivstatus, tb = NA)
    pValueVectorChiSq <- pValueVector <- rep(NA, numSims)
    for(jj in 1:numSims) {
        if(jj %% 20 == 0) print(paste0('on simulation ', jj, ' of ', numSims))
        myDat$tb <- rbinom(n, 1, prob = rep(tbIncRate, each = n/2))
        ## Null_IDR_Vector <- rep(NA, numPerms)
        ## for(ii in 1:numPerms) {
        ##     permDat <- myDat
        ##     permDat$tb <- sample(permDat$tb, n)
        ##     Null_IDR_Vector[ii] <- idr(permDat)
        ## }
        ## Full_IDR_Vector <- c(Null_IDR_Vector, idr(myDat))
        ## pValueVector[jj] <- pValFxn(Full_IDR_Vector, idr(myDat))
        myTab <- xtabs(~hiv + tb, myDat)
        chisqResult <- chisq.test(myTab)
        pValueVectorChiSq[jj] <- chisqResult$p.value
        ## if(pValueVector[jj] > 1) browser()
    }
    ## debug(pValFxn)
    ## undebug(pValFxn)
    ## pValueVector
    ## pValueVectorChiSq
    ## plot(data.frame(pValueVector, pValueVectorChiSq))
    ## abline(a = 0, b = 1)
    ## power <- mean(pValueVector <= .05)
    powerChiSqVector[rr] <- mean(pValueVectorChiSq <= .05)
}

par(mar = c(6,6,1,1))
powerTable <- data.frame(idr = trueIDRvals, power = powerChiSqVector)
plot(powerTable, xlab = 'IDR', ylab = 'statistical power', main = '',
     type = 'b',
     col = 'blue',
     lwd = 3,
     bty = 'n')
abline(h=.05, col = 'red', lty = 2, lwd = 4)
