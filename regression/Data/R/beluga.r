#
# Nursing behaviors of beluga calves (Delphinapterus leucas) born in captivity
#
# Note that time series diagnostics (Durbin-Watson test, runs test, ACF plot) and regression
# diagnostics are not given here since they weren't given in the paper, but they would of
# course actually be constructed
#
whale <- read.csv("c:/class/ascii/whale.csv")
attach(whale)
Day <- trunc((Period1-1)/4)+1
boxplot(split(Nursing1,Day),xlab="Day",ylab="Hudson's nursing time")
boxplot(split(Nursing2,Day),xlab="Day",ylab="Casey's nursing time")
#
# Construct moving average
#
nursma1 <- rep(NA,217)
for (i in 6:222) nursma1[i] <- mean(Nursing1[(i-5):(i+6)])
#
# Time series plot of moving average
#
ts.plot(nursma1,ylab="Three day moving average")
boxplot(split(Bouts1,Day),xlab="Day",ylab="Hudson's nursing bouts")
boxplot(split(Bouts2,Day),xlab="Day",ylab="Casey's nursing bouts")
n1 <- 228
Nursing1.lag <- c(NA,Nursing1[1:(n1-1)])
Bouts1.lag <- c(NA,Bouts1[1:(n1-1)])
Lockons1.lag <- c(NA,Lockons1[1:(n1-1)])
Change.Nursing1 <- Nursing1 - Nursing1.lag
n2 <- 223
Nursing2 <- Nursing2[1:n2]
Nursing2.lag <- c(NA,Nursing2[1:(n2-1)])
Bouts2.lag <- c(NA,Bouts2[1:(n2-1)])
Lockons2.lag <- c(NA,Lockons2[1:(n2-1)])
Change.Nursing2 <- Nursing2 - Nursing2.lag
plot(Lockons1,Change.Nursing1,xlab="Number of lockons",ylab="Change in nursing from previous period")
plot(Lockons1.lag,Change.Nursing1,xlab="Number of lockons in previous time period",ylab="Change in nursing from previous period")
plot(Nursing1.lag,Change.Nursing1,xlab="Nursing time in previous time period",ylab="Change in nursing from previous period")
Nursing1.lag <- Nursing1.lag[6:228]
Lockons1 <- Lockons1[6:228]
Lockons1.lag <- Lockons1.lag[6:228]
Bouts1 <- Bouts1[6:228]
Bouts1.lag <- Bouts1.lag[6:228]
Change.Nursing1 <- Change.Nursing1[6:228]
Period1 <- Period1[6:228]
beluga1 <- lm(Change.Nursing1~Period1+Lockons1+Lockons1.lag+Nursing1.lag)
summary(beluga1)
Nursing2.lag <- Nursing2.lag[4:223]
Lockons2 <- Lockons2[4:223]
Lockons2.lag <- Lockons2.lag[4:223]
Bouts2 <- Bouts2[4:223]
Bouts2.lag <- Bouts2.lag[4:223]
Change.Nursing2 <- Change.Nursing2[4:223]
Period2 <- Period2[4:223]
beluga2 <- lm(Change.Nursing2~Period2+Lockons2+Bouts2+Bouts2.lag+Nursing2.lag)
summary(beluga2)
#
# Validate Casey's model on Hudson, and Hudson's model on Casey
#
hudsonpred <- 139.05+.406*Nursing1.lag-.835*Period1+5.63*Lockons1+10.818*Bouts1-11.048*Bouts1.lag
hudsonpred <- c(rep(NA,5),hudsonpred)
ts.plot(ts(Nursing1),ts(hudsonpred), gpars=list(lty=c(1:2)))
caseypred <- 43.53+.484*Nursing2.lag-.205*Period2+6.731*Lockons2-4.46*Lockons2.lag
caseypred <- c(rep(NA,3),caseypred)
ts.plot(ts(Nursing2),ts(caseypred), gpars=list(lty=c(1:2)))
