#
# The flight of the space shuttle Challenger
#
challenger <- read.csv("c:/class/ascii/chal1.csv")
attach(challenger)
plot(Temperature,Damaged/O.rings,xlab="Temperature",ylab="Proportion failing")
#
# To fit logistic regression when data are in the (Number of successes, Number of trials) format,
# use Success/Trial as the left-hand side of the formula, and Trial as a weight variable. Logistic
# regression is fit using the glm() function, telling R that the family is binomial. Note
# that R does not give likelihood ratio tests, but rather Wald tests; likelihood ratio
# tests are determined using the drop1() command.
#
challog1 <- glm(Damaged/O.rings ~ Temperature, weights=O.rings, family=binomial)
summary(challog1)
drop1(challog1, test="LRT")
#
# Odds ratios are the slopes exponentiated (not the intercept)
#
exp(coef(challog1)[-1])
#
# You can get a 95% confidence interval for the odds ratio by exponentiating the ends of a confidence
# interval for the slope
#
exp(confint.default(challog1))
#
# To get the statistic for the overall significance, you need to subtract the 
# deviance of this model from the deviance of the model based on no
# predictors, which is given as the null.deviance attribute
#
gstat <- challog1$null.deviance - deviance(challog1)
cbind(gstat, 1-pchisq(gstat,length(coef(challog1))-1))
#
# R doesn't give many of the results that Minitab does, including the Hosmer-Lemeshow test
# and the association measures. It also doesn't give the Pearson goodness-of-fit statistic, but
# that can be obtained indirectly, since it is the sum of squares of the Pearson residuals.
# Measures of association can only be obtained if the data are in the form of 0/1 responses; 
# in this context, the data being at the level of individual O-rings, rather than individual flights. 
# The Hosmer-Lemeshow test is available as part of the ResourceSelection package. 
# It will not give the same results as that in Minitab when there are ties in the data (that is, multiple
# observations with the same estimated response), as there are here. You can also print out the
# underlying counts that determine the HL test, as is optional in Minitab. 
#
library(ResourceSelection}
hl <- hoslem.test(Damaged/O.rings, fitted(challog1), g=6)
hl
cbind(hl$observed,hl$expected)
pearres1 <- residuals(challog1,type="pearson")
pearson <- sum(pearres1^2)
c(pearson, 1-pchisq(pearson,21))
c(deviance(challog1), 1-pchisq(deviance(challog1),21))
#
# The AIC value given here is not the same as that given by Minitab, but comparisons between AIC values
# for different models will be the same (which is all that matters).
#
AIC(challog1)
#
# The glm.diag() function from the boot package gives diagnostics for generalized linear models, while the
# glm.diag.plots() function gives diagnostic plots. The rp, h, and cook attributes refer to standardized
# Pearson residuals, leverage values, and Cook's distances, respectively. The built-in functions for
# hatvalues() and cooks.distance() will work for glm objects as well, but the standardized residuals
# produced by rstandard() are not the standardized Pearson residuals.
#
library(boot)
chaldiag1 <- glm.diag(challog1)
cbind(chaldiag1$rp, chaldiag1$h, chaldiag1$cook)
glm.diag.plots(challog1, chaldiag1)
chall2 <- challenger[-21,]
attach(chall2)
challog2 <- glm(Damaged/O.rings ~ Temperature, weights=O.rings, family=binomial)
summary(challog2)
drop1(challog2, test="LRT")
exp(coef(challog2)[-1])
gstat <- challog2$null.deviance - deviance(challog2)
cbind(gstat, 1-pchisq(gstat,length(coef(challog2))-1))
pearres2 <- residuals(challog2,type="pearson")
pearson <- sum(pearres2^2)
c(pearson, 1-pchisq(pearson,21))
c(deviance(challog2), 1-pchisq(deviance(challog2),20))
#
# Don't forget, this AIC value is not comparable to that of the model using the entire data set, since the
# two models are based on different data sets.
#
AIC(challog2)
chaldiag2 <- glm.diag(challog2)
cbind(chaldiag2$rp, chaldiag2$h, chaldiag2$cook)
glm.diag.plots(challog2, chaldiag2)
#
# Plot of fitted curve
#
# Get the fitted curve by creating a fine grid of temperature values as a data frame, and then using the predict()
# function with type="response"
#
#
plot(challenger$Temperature, challenger$Damaged/challenger$O.rings, xlab="Temperature", 
  ylab="Probability of O-ring damage", xlim=c(30,81), ylim=c(0,1))
new.data <- data.frame(Temperature=c(300:810)/10)
lines(new.data$Temperature, predict(challog2,new.data,type="response"))


