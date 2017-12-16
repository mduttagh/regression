#
# News flash! Smoking makes you live longer!
#
smoking <- read.csv("c:/class/ascii/smoking.csv")
attach(smoking)
smoke1 <- glm(Survived/At.risk ~ Smoker, weights=At.risk, family=binomial)
summary(smoke1)
exp(coef(smoke1)[-1])
gstat <- smoke1$null.deviance - deviance(smoke1)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke1))-1))
pearres1 <- residuals(smoke1,type="pearson")
pearson <- sum(pearres1^2)
c(pearson, 1-pchisq(pearson,12))
c(deviance(smoke1), 1-pchisq(deviance(smoke1),12))
smoking
plot(Age,Survived/At.risk,ylab="Survival rate")
smoke2 <- glm(Survived/At.risk ~ Smoker + Age, weights=At.risk, family=binomial)
summary(smoke2)
exp(coef(smoke2)[-1])
gstat <- smoke2$null.deviance - deviance(smoke2)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke2))-1))
pearres2 <- residuals(smoke2,type="pearson")
pearson <- sum(pearres2^2)
c(pearson, 1-pchisq(pearson,11))
c(deviance(smoke2), 1-pchisq(deviance(smoke2),11))
drop1(smoke2, test="LRT")
plot(Age,log((Survived/At.risk)/(1-Survived/At.risk)),ylab="Logit of survival")
Age.squared <- Age*Age
smoke3 <- glm(Survived/At.risk ~ Smoker + Age + Age.squared, weights=At.risk, family=binomial)
summary(smoke3)
exp(coef(smoke3)[-1])
gstat <- smoke3$null.deviance - deviance(smoke3)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke3))-1))
pearres3 <- residuals(smoke3,type="pearson")
pearson <- sum(pearres3^2)
c(pearson, 1-pchisq(pearson,10))
c(deviance(smoke3), 1-pchisq(deviance(smoke3),10))
drop1(smoke3, test="LRT")
plot(fitted(smoke3), pearres3, xlab="Fitted probabilities", ylab="Pearson residuals")
#
# The factor() function tells R to treat Age as a factor variable
#
# Note that by default R uses indicator variables, as Minitab does for logistic regression
#
smoke4 <- glm(Survived/At.risk ~ Smoker + factor(Age), weights=At.risk, family=binomial)
summary(smoke4)
exp(coef(smoke4)[-1])
gstat <- smoke4$null.deviance - deviance(smoke4)
cbind(gstat, 1-pchisq(gstat,length(coef(smoke4))-1))
#
# The LR test for the Age effect is the difference between the deviance statistic for the model that
# includes it and the deviance for the model that has no Age effect. It is compared to a chi-squared
# distribution on k-1 degrees of freedom, where k is the number of Age categories (here 7-1=6). 
#
agetest <- deviance(smoke1)-deviance(smoke4)
cbind(agetest, 1-pchisq(agetest,6))
#
# The car library gives LR tests (and others as options) for glm objects as well as lm and aov objects
#
library(car)
Anova(smoke4, type=3)
pearres4 <- residuals(smoke4,type="pearson")
pearson <- sum(pearres4^2)
c(pearson, 1-pchisq(pearson,6))
c(deviance(smoke4), 1-pchisq(deviance(smoke4),6))
#
# Table of smoking percentages
#
cbind(unique(Age),smoking[Smoker==1,]$At.risk/(smoking[Smoker==1,]$At.risk
  + smoking[Smoker==0,]$At.risk))

