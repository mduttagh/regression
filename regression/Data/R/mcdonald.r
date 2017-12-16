#
# CAPM: Do you want fries with that?
#
mcdonald <- read.csv("c:/class/ascii/mcdonald.csv")
attach(mcdonald)
mcdonald
plot(Market.return,McDonalds.return)
mcdonalda <- lm(McDonalds.return ~ Market.return)
summary(mcdonalda)
library(car)
#
# Test for slope coefficient equaling 1
#
linearHypothesis(mcdonalda,c(0,1),rhs=1)
std.resa <- rstandard(mcdonalda)
hata <- hatvalues(mcdonalda)
cooka <- cooks.distance(mcdonalda)
cbind(std.resa, hata, cooka)
#
# Levene's test
#
absres <- abs(std.resa)
plot(Market.return,absres)
#
# This fits a local linear version of loess; See the book Smoothing Methods in Statistics by J.S. Simonoff
# (http://people.stern.nyu.edu/jsimonof/SmoothMeth/) for discussion of such smoothing methods
#
mcdonalds.lo <- loess(absres ~ Market.return,degree=1,span=.5)
lo.est <- predict(mcdonalds.lo, data.frame(Market.return = seq(min(Market.return), max(Market.return), .001)))
#
# This adds the loess line to the scatter plot
#
lines(seq(min(Market.return), max(Market.return), .001), lo.est)
Marketsquared <- Market.return*Market.return
summary(lm(absres ~ Market.return+Marketsquared))
lgsressq <- log(std.resa^2)
wt <- 1/exp(fitted(lm(lgsressq ~ Market.return+Marketsquared)))
#
# A way of doing this that would be expected to work a bit better is to base the weights on a Poisson regression
# with squared residuals as the response rather than a least squares regression with logged squared residuals
# as the response:
#
#   wt2 <- 1/fitted(glm(std.resa^2~Market.return+Marketsquared, family=poisson))
#
# See Section 10.7 of the Chatterjee and Simonoff Handbook of Regression Analysis book
# 
mcdonaldb <- lm(McDonalds.return ~ Market.return, weight=wt)
summary(mcdonaldb)
linearHypothesis(mcdonaldb,c(0,1),rhs=1)
std.resb <- rstandard(mcdonaldb)
hatb <- hatvalues(mcdonaldb)
cookb <- cooks.distance(mcdonaldb)
cbind(std.resb, hatb, cookb)
absres <- abs(std.resb)
summary(lm(absres ~ Market.return+Marketsquared))
plot(Market.return,std.resb,ylab="Standardized residuals")
mcdonalds.lo2 <- loess(std.resb ~ Market.return,degree=1,span=.5)
lo2.est <- predict(mcdonalds.lo2, data.frame(Market.return = seq(min(Market.return), max(Market.return), .001)))
lines(seq(min(Market.return), max(Market.return), .001),lo2.est)
plot(Market.return,absres,ylab="Absolute standardized residuals")
mcdonalds.lo3 <- loess(absres ~ Market.return,degree=1,span=.5)
lo3.est <- predict(mcdonalds.lo3, data.frame(Market.return = seq(min(Market.return), max(Market.return), .001)))
lines(seq(min(Market.return), max(Market.return), .001),lo3.est)
plot(c(1:length(McDonalds.return)),std.resb,xlab="Observation order",ylab="Standardized residuals")
qqnorm(std.resb)
McDonalds.excess <- McDonalds.return - Riskless.rate
Market.excess <- Market.return - Riskless.rate
mcdonaldc <- lm(McDonalds.excess ~ Market.excess)
summary(mcdonaldc)
std.resc <- rstandard(mcdonaldc)
Marketexsquared <- Market.excess*Market.excess
lgsressq <- log(std.resc^2)
wt <- 1/exp(fitted(lm(lgsressq ~ Market.excess+Marketexsquared)))
mcdonaldd <- lm(McDonalds.excess ~ Market.excess, weight=wt)
summary(mcdonaldd)
#
# Prediction intervals in R from a WLS fit are constructed the same way as from an OLS fit, except that a vector of weights for the new data
# must be supplied (that is, R will do the necessary claculations for you once you provide the weights):
#   predict(RlmObject, newdata, interval=c("prediction"), weights=wt)
#

