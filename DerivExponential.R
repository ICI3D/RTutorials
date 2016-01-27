timeStep <- .001
maxTime <- 1000
time <- seq(0, maxTime, by = timeStep)
dt <- data.frame(time=time, eventHappened=NA)
dt$eventHappened <- rbinom(length(time), 1, prob = .001)

head(dt) ## event almost never happens

sum(dt$eventHappened) ## number of times event happened
mean(dt$eventHappened) ## proportion of times event happened

## to find the times at which an event happened
head(dt$time[eventHappened>0]) ##
tail(dt$time[eventHappened>0]) ##
hist(diff($time[dt$eventHappened==1]), breaks = 100, col = 'black')

## to find the difference between values in a vector c(3,5,8)
diff(c(3,5,8))
