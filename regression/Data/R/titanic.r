#
# The sinking of the Titanic
#
titanic <- read.csv("c:/class/ascii/titanic.csv")
attach(titanic)
cbind(Economic.status,Age.group,Gender,Survived/At.risk,At.risk)
#
# This uses the Hmisc package
#
library(Hmisc)
summary(Survived ~ Economic.status, fun=sum)/summary(At.risk ~ Economic.status, fun=sum)
summary(Survived ~ Age.group, fun=sum)/summary(At.risk ~ Age.group, fun=sum)
summary(Survived ~ Gender, fun=sum)/summary(At.risk ~ Gender, fun=sum)
#
# Note how to get the values needed for the two-way table. Commands are given in pairs, with the percentages being the counts from the 
# second row in each cell of the first table divided by the counts from the second row in each cell of the second table 
#
summary(Survived ~ Economic.status*Gender, method="cross", fun=sum)
summary(At.risk ~ Economic.status*Gender, method="cross", fun=sum)
summary(Survived ~ Economic.status*Age.group, method="cross", fun=sum)
summary(At.risk ~ Economic.status*Age.group, method="cross", fun=sum)
summary(Survived ~ Gender*Age.group, method="cross", fun=sum)
summary(At.risk ~ Gender*Age.group, method="cross", fun=sum)
#
# Fitting various models; note AIC values are given as part of the output
#
titanic1 <- glm(Survived/At.risk ~ Economic.status, family=binomial, weights=At.risk)
summary(titanic1)
exp(coef(titanic1))[-1]
library(car)
Anova(titanic1, type=3)
titanic2 <- glm(Survived/At.risk ~ Age.group, family=binomial, weights=At.risk)
summary(titanic2)
exp(coef(titanic2)[-1])
Anova(titanic2, type=3)
titanic3 <- glm(Survived/At.risk ~ Gender, family=binomial, weights=At.risk)
summary(titanic3)
exp(coef(titanic3)[-1])
Anova(titanic3, type=3)
titanic4 <- glm(Survived/At.risk ~ Economic.status + Age.group, family=binomial, weights=At.risk)
summary(titanic4)
exp(coef(titanic4)[-1])
Anova(titanic4, type=3)
titanic5 <- glm(Survived/At.risk ~ Economic.status + Gender, family=binomial, weights=At.risk)
summary(titanic5)
exp(coef(titanic5)[-1])
Anova(titanic5, type=3)
titanic6 <- glm(Survived/At.risk ~ Age.group + Gender, family=binomial, weights=At.risk)
summary(titanic6)
exp(coef(titanic6)[-1])
Anova(titanic6, type=3)
titanic7 <- glm(Survived/At.risk ~ Economic.status + Age.group + Gender, family=binomial, weights=At.risk)
summary(titanic7)
exp(coef(titanic7)[-1])
Anova(titanic7, type=3)
#
# Interaction effects fit as was done for two-way ANOVA
#
titanic8 <- glm(Survived/At.risk ~ Economic.status*Age.group, family=binomial, weights=At.risk)
summary(titanic8)
exp(coef(titanic8)[-1])
Anova(titanic8, type=3)
titanic9 <- glm(Survived/At.risk ~ Economic.status*Gender, family=binomial, weights=At.risk)
summary(titanic9)
exp(coef(titanic9)[-1])
Anova(titanic9, type=3)
titanic10 <- glm(Survived/At.risk ~ Age.group*Gender, family=binomial, weights=At.risk)
summary(titanic10)
exp(coef(titanic10)[-1])
Anova(titanic10, type=3)
#
# Any models that include the Economic status by Age group interaction will have a missing coefficient because of the lack of crew 
# members who were children. R will still fit the model, but the Anova() function will not work. The tests for the interaction term
# must be calculated directly by comparing the deviance of the model with the interaction to the model with only Economic status and
# Age group main effects
#
titanic11 <- glm(Survived/At.risk ~ Economic.status*Age.group + Gender, family=binomial, weights=At.risk)
summary(titanic11)
exp(coef(titanic11)[-1])
econagetest <- deviance(titanic7)-deviance(titanic11)
cbind(econagetest, 1-pchisq(econagetest,2))
titanic12 <- glm(Survived/At.risk ~ Economic.status*Gender + Age.group, family=binomial, weights=At.risk)
summary(titanic12)
exp(coef(titanic12)[-1])
Anova(titanic12, type=3)
titanic13 <- glm(Survived/At.risk ~ Age.group*Gender + Economic.status, family=binomial, weights=At.risk)
summary(titanic13)
exp(coef(titanic13)[-1])
Anova(titanic13, type=3)
titanic14 <- glm(Survived/At.risk ~ Economic.status*Age.group + Economic.status*Gender, family=binomial, weights=At.risk, maxit=100)
summary(titanic14)
exp(coef(titanic14)[-1])
econagetest <- deviance(titanic12)-deviance(titanic14)
cbind(econagetest, 1-pchisq(econagetest,2))
econgendertest <- deviance(titanic11)-deviance(titanic14)
cbind(econgendertest, 1-pchisq(econgendertest,3))
titanic15 <- glm(Survived/At.risk ~ Economic.status*Age.group + Age.group*Gender, family=binomial, weights=At.risk)
summary(titanic15)
exp(coef(titanic15)[-1])
econagetest <- deviance(titanic13)-deviance(titanic15)
cbind(econagetest, 1-pchisq(econagetest,2))
agegendertest <- deviance(titanic11)-deviance(titanic15)
cbind(agegendertest, 1-pchisq(agegendertest,1))
titanic16 <- glm(Survived/At.risk ~ Economic.status*Gender + Age.group*Gender, family=binomial, weights=At.risk)
summary(titanic16)
exp(coef(titanic16)[-1])
Anova(titanic16, type=3)
titanic17 <- glm(Survived/At.risk ~ Economic.status*Gender + Age.group*Gender + Economic.status*Age.group, family=binomial, weights=At.risk, maxit=100)
summary(titanic17)
exp(coef(titanic17)[-1])
econagetest <- deviance(titanic16)-deviance(titanic17)
cbind(econagetest, 1-pchisq(econagetest,2))
econgendertest <- deviance(titanic15)-deviance(titanic17)
cbind(econgendertest, 1-pchisq(econgendertest,3))
agegendertest <- deviance(titanic14)-deviance(titanic17)
cbind(agegendertest, 1-pchisq(agegendertest,1))
fitted(titanic12)
fitted(titanic14)
fitted(titanic16)
