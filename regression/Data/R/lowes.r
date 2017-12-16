#
# Modeling Lowe's sales
#
lowes <- read.csv("c:/class/ascii/lowes.csv")
attach(lowes)
hist(Sales)
Log.sales <- log10(Sales)
plot(Housing.starts,Log.sales)
plot(Mortgage,Log.sales)
plot(Time,Log.sales)
lowes1 <- lm(Log.sales ~ Housing.starts + Mortgage + Time)
summary(lowes1)
#
# Fox's car package
#
library(car)
vif(lowes1)
stdres <- rstandard(lowes1)
plot(fitted(lowes1),stdres,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(stdres)
#
# Handout doesn't include diagnostics, but they should be examined and reported
#
cbind(stdres, hatvalues(lowes1), cooks.distance(lowes1))
plot(Time,stdres,ylab="Standardized residuals")
#
# This is how you add the lines between the points
#
lines(Time,stdres)
boxplot(split(stdres,Quarter))
lowes2 <- lm(Log.sales ~ Housing.starts + Mortgage + Time + Time.sq + Q3)
summary(lowes2)
vif(lowes2)
lowes3 <- lm(Log.sales ~ Housing.starts + Time + Time.sq + Q3)
summary(lowes3)
stdres <- rstandard(lowes3)
plot(fitted(lowes3),stdres,xlab="Fitted values",ylab="Standardized residuals")
plot(Time,stdres,ylab="Standardized residuals")
lines(Time,stdres)
qqnorm(stdres)
plot(Housing.starts,stdres,ylab="standardized residuals")
boxplot(split(stdres,Quarter))
G1980s <- Year < 1990
Housing80s <- Housing.starts*G1980s
Mortgage80s <- Mortgage*G1980s
Time80s <- Time*G1980s
Q380s <- Q3*G1980s
lowes4 <- lm(Log.sales ~ Housing.starts + Mortgage + Time + Q3 + G1980s + Housing80s + 
  Mortgage80s + Time80s + Q380s)
summary(lowes4)
vif(lowes4)
lowes5 <- lm(Log.sales ~ Housing.starts + Mortgage + Time + Q3 + G1980s +  
  Mortgage80s + Time80s + Q380s)
summary(lowes5)
vif(lowes5)

