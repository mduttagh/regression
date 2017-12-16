#
# Estimating a demand function - it's about time
#
gasdemand <- read.csv("c:/class/ascii/demand2.csv")
attach(gasdemand)
demande <- lm(logGpc~logPG+logI+logPD+logPN+logPS)
summary(demande)
#
# Fox's car package
#
library(car)
vif(demande)
std.rese <- rstandard(demande)
ts.plot(ts(std.rese),ylab="Standardized residuals")
#
# The lmtest package provides the Durbin-Watson statistic
#
library(lmtest)
dwtest(logGpc~logPG+logI+logPD+logPN+logPS)
#
# Runs test function given in the file for the "Ordinary least squares estimation and 
# time series data" handout
#
runs.test(std.rese, cont.corr=T)
acf(std.rese, xlim=c(1,15), ylim=c(-.4,.8))
n <- length(logGpc)
#
# Approximate z-statistics for significance of each autocorrelation
#
acf(std.rese,plot=F)$acf[2:15]*sqrt(n)
logGpc.lag <- c(NA,logGpc[1:(n-1)])
logPG.lag <- c(NA,logPG[1:(n-1)])
demandf <- lm(logGpc~logGpc.lag+logPG+logPG.lag)
summary(demandf)
vif(demandf)
std.resf <- rstandard(demandf)
hatf <- hatvalues(demandf)
cookf <- cooks.distance(demandf)
std.resf <- c(NA,std.resf)
hatf <- c(NA,hatf)
cookf <- c(NA,cookf)
ts.plot(ts(std.resf),ylab="Standardized residuals")
cbind(Year,logGpc,std.resf,hatf,cookf)
gasdemand.adj <- gasdemand
gasdemand.adj$logGpc[32] <- 1.04708
attach(gasdemand.adj)
demandg <- lm(logGpc~logGpc.lag+logPG+logPG.lag)
summary(demandg)
vif(demandg)
std.resg <- rstandard(demandg)
hatg <- hatvalues(demandg)
cookg <- cooks.distance(demandg)
std.resg <- c(NA,std.resg)
hatg <- c(NA,hatg)
cookg <- c(NA,cookg)
ts.plot(ts(std.resg),ylab="Standardized residuals")
cbind(Year,logGpc,std.resg,hatg,cookg)
acf(na.omit(std.resg), xlim=c(1,15), ylim=c(-.4,.4))
acf(na.omit(std.resg),plot=F)$acf[2:15]*sqrt(n)
runs.test(na.omit(std.resg), cont.corr=T)
plot(fitted(demandg),std.resg[2:n],xlab="Fitted values",ylab="Standardized residuals")
attach(gasdemand)
Year1991 <- as.numeric(Year==1991)
demandh <- lm(logGpc~logGpc.lag+logPG+logPG.lag+Year1991)
summary(demandh)
