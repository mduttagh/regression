#
# Eruptions of the Old Faithful Geyser
#
geyser <- read.csv("c:/class/ascii/geyser1.csv")
attach(geyser)
hist(Interval,xlab="Time interval until next eruption")
plot(Duration,Interval)
geysera <- lm(Interval ~ Duration)
summary(geysera)
std.resa <- rstandard(geysera)
ts.plot(ts(std.resa),ylab="Standardized residuals")
library(lmtest)
#
# Durbin-Watson testing often looks only for positive autocorrelation, but this is a situation where
# negative autocorrelation is relevant
#
dwtest(Interval~Duration, alternative="two.sided")
#
# Runs test function given in code for "Ordinary least squares and time series estimation" handout
#
runs.test(std.resa, cont.corr=T)
acf(std.resa, xlim=c(1,23), ylim=c(-.3, .3))
n <- length(Interval)
acf(std.resa,plot=F)$acf[2:23]*sqrt(n)
Interval.star <- Interval + .256* c(NA,Interval[1:(n-1)])
Duration.star <- Duration + .256* c(NA,Duration[1:(n-1)])
geyserb <- lm(Interval.star ~ Duration.star)
summary(geyserb)
std.resb <- rstandard(geyserb)
hatb <- hatvalues(geyserb)
cookb <- cooks.distance(geyserb)
std.resb <- c(NA,std.resb)
hatb <- c(NA,hatb)
cookb <- c(NA,cookb)
ts.plot(ts(std.resb),ylab="Standardized residuals")
dwtest(Interval.star~Duration.star, alternative="two.sided")
runs.test(na.omit(std.resb), cont.corr=T)
acf(na.omit(std.resb), xlim=c(1,23), ylim=c(-.25, .25))
acf(na.omit(std.resb),plot=F)$acf[2:23]*sqrt(n-1)
plot(fitted(geyserb),std.resb[2:n],xlab="Fitted values",ylab="Standardized residuals")
cbind(std.resb,hatb,cookb)
qqnorm(std.resb)
geyser.new <- read.csv("c:/class/ascii/geyser2.csv")
pred.ols <- coef(geysera)[1] + coef(geysera)[2]*geyser.new$Duration
pred.gls <- coef(geyserb)[1]/(1+.256) + coef(geyserb)[2]*geyser.new$Duration
error.ols <- geyser.new$Interval - pred.ols
error.gls <- geyser.new$Interval - pred.gls
summary(error.ols)
summary(error.gls)


