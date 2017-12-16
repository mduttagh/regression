#
# Mortgage rates
#
mortgage <- read.csv("c:/class/ascii/limortg.csv")
attach(mortgage)
summary(mortgage)
hist(Rate)
hist(Fees)
plot(Fees,Rate)
boxplot(split(Rate,Jumbo), xlab="Jumbo", ylab="Rate")
morta <- lm(Rate ~ Fees)
summary(morta)
mortb <- lm(Rate ~ Fees + Jumbo)
summary(mortb)
#
# Load John Fox's car package (see http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/)
#
library(car)
vif(mortb)
mortc <- lm(Rate ~ Fees + Jumbo + Fees*Jumbo)
summary(mortc)
vif(mortc)
mcon <- matrix(c(0,0,1,0,0,0,0,1),nrow=2,byrow=T)
linearHypothesis(mortc,mcon)
#
# A simpler way to get a partial F-test
#
anova(morta,mortc)
#
# To construct the plot we first set up the outside, then we add the data points, and then the fitted lines
#
plot(Fees, Rate, type="n")
points(Fees[Jumbo==0], Rate[Jumbo==0], pch=1)
points(Fees[Jumbo==1], Rate[Jumbo==1], pch=2)
abline(lsfit(Fees[Jumbo==0],Rate[Jumbo==0]))
abline(lsfit(Fees[Jumbo==1],Rate[Jumbo==1]), lty=2)
plot(fitted(mortc),residuals(mortc),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(mortc))
newmort <- mortgage[-4,]
attach(newmort)
mortd <- lm(Rate ~ Fees)
summary(mortd)
morte <- lm(Rate ~ Fees + Jumbo)
summary(morte)
vif(morte)
mortf <- lm(Rate ~ Fees + Jumbo + Fees*Jumbo)
summary(mortf)
vif(mortf)
plot(fitted(mortf),residuals(mortf),xlab="Fitted values",ylab="Residuals")
qqnorm(residuals(mortf))
plot(Fees,residuals(mortf),ylab="Residuals")
plot(Jumbo,residuals(mortf),ylab="Residuals")
