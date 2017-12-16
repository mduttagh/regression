#
# Predicting bankruptcy in the telecommunications industry
#
bankruptcy <- read.csv("c:/class/ascii/bankruptcy.csv")
attach(bankruptcy)
bankruptcy
boxplot(split(WC.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="WC.TA")
boxplot(split(RE.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="RE.TA")
boxplot(split(EBIT.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="EBIT.TA")
boxplot(split(S.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="S.TA")
boxplot(split(BVE.BVL,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="BVE.BVL")
bank1 <- glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial, maxit=500)
summary(bank1)
gstat <- bank1$null.deviance - deviance(bank1)
cbind(gstat, 1-pchisq(gstat,length(coef(bank1))-1))
exp(coef(bank1)[-1])
#
# The MKmisc library gives the Hosmer-Lemeshow test; it is given in the output as the statistic C.
#
library(ResourceSelection)
hoslem.test(Bankrupt, fitted(bank1))
#
# The rms library includes a different test that is appropriate for 0/1 response data with no replications. To use it, 
# first use the lrm() command in the rms library to fit the model, and then use the residuals() command to get the test. 
# This function also gives all of the summary measures of association in the stats component, with Somers D
# being represented by Dxy
#
library(rms)
bank1.lrm <- lrm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, x=T, y=T)
residuals(bank1.lrm, type="gof")
bank1.lrm$stats
library(leaps)
leaps(cbind(WC.TA, RE.TA, EBIT.TA, S.TA, BVE.BVL),Bankrupt,nbest=2)
#
# The bestglm library will perform best subsets for generalized linear models (including logistic regression), and also
# allows factor (categorical) variables, which leaps() does not. The default criterion used is BIC, but AIC can be
# requested as an option (from which AICc can be calculated if wished). There really is no reason to use the "quick and
# dirty" OLS-based leaps() in R, since bestglm gives the actual best subsets; you should use bestglm.
#
library(bestglm)
logitbest <- bestglm(data.frame(cbind(WC.TA, RE.TA, EBIT.TA, S.TA, BVE.BVL),Bankrupt), IC="AIC")
logitbest$Subsets
bank2 <- glm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, family=binomial, maxit=500, x=T)
summary(bank2)
gstat <- bank2$null.deviance - deviance(bank2)
cbind(gstat, 1-pchisq(gstat,length(coef(bank2))-1))
exp(coef(bank2)[-1])
drop1(bank2, test="LRT")
hoslem.test(Bankrupt, fitted(bank2))
bank2.lrm <- lrm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, x=T, y=T)
residuals(bank2.lrm, type="gof")
bank2.lrm$stats
library(boot)
bankdiag2 <- glm.diag(bank2)
spearson2 <- residuals(bank2, type="pearson")/sqrt(1-bankdiag2$h)
cbind(spearson2,bankdiag2$cook,bankdiag2$h)
plot(fitted(bank2), rstudent(bank2), xlab="Estimated probabilities", ylab="Standardized Pearson resids")
bankrupt2 <- bankruptcy[-1,]
attach(bankrupt2)
#
# The results aren't exactly the same when there is separation, but the symptoms of the problem
# are similar
#
bank3 <- glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial, maxit=500)
summary(bank3)
bank4 <- glm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, family=binomial, maxit=500, x=T)
summary(bank4)
gstat <- bank4$null.deviance - deviance(bank4)
cbind(gstat, 1-pchisq(gstat,length(coef(bank4))-1))
exp(coef(bank4)[-1])
drop1(bank4, test="LRT")
hoslem.test(Bankrupt, fitted(bank4))
bank4.lrm <- lrm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, x=T, y=T)
residuals(bank4.lrm, type="gof")
bank4.lrm$stats
bankdiag4 <- glm.diag(bank4)
spearson4 <- residuals(bank4, type="pearson")/sqrt(1-bankdiag4$h)
bankfit4 <- predict(bank4,type="response")
cbind(spearson4,bankdiag4$cook,bankdiag4$h,bankfit4)
plot(fitted(bank4), rstudent(bank4), xlab="Estimated probabilities", ylab="Standardized Pearson resids")
#
# Classification table
#
bankrupt.predict <- as.numeric(fitted(bank4) > .5)
table(Bankrupt, bankrupt.predict)
#
# If we had a set of new (validation) data we could check the ability of the model to make future classifications.
# Say newdata is a data set (data frame) based on other companies, with all of the same variable names as were
# used in the original model fit. Probabilities of bankruptcy for the new data are obtained using the predict()
# command:
#
#           pred.new <- predict(bank4, newdata, type="response")
#
# A classification table would be constructed the same way as when it is based on the original data:
#
#           newclass.predict <- as.numeric(pred.new > .5)
#           table(newdata$Bankrupt, newclass.predict)
#
# To get prospective probability estimates, adjust the retrospective logits and the transform back.
# The retrospective logits are obtained from the predict() function
#
prosplogit <- predict(bank4) + log((.1*25)/(.9*24))
prospprob <- exp(prosplogit)/(1 + exp(prosplogit))
prospprob
