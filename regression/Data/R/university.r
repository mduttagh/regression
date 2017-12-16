#
# What makes a top university top?
#
university <- read.csv("c:/class/ascii/university.csv")
data.frame(university$School,university$SAT.1st.quartile,university$SAT.3rd.quartile,university$Top.10.,
  university$Acceptance.rate,university$Tier)
#
# This makes Tier an ordered factor, so analysis can be based on the correct ordering, rather than
# the default alphabetical ordering. We will need both orderings (increasing and decreasing).
#
Tier.ordered.up <- ordered(university$Tier,c("Tier 4","Tier 3","Tier 2","Top 50"))
Tier.ordered.down <- ordered(university$Tier,c("Top 50","Tier 2","Tier 3","Tier 4"))
boxplot(split(university$SAT.1st.quartile,Tier.ordered.up),style.bxp="old",xlab="Tier",ylab="SAT 1st quartile")
boxplot(split(university$SAT.3rd.quartile,Tier.ordered.up),style.bxp="old",xlab="Tier",ylab="SAT 3rd quartile")
boxplot(split(university$Top.10.,Tier.ordered.up),style.bxp="old",xlab="Tier",ylab="Top 10%")
boxplot(split(university$Acceptance.rate,Tier.ordered.up),style.bxp="old",xlab="Tier",ylab="Acceptance rate")
#
# The function multinom() fits a nominal logistic regression. It requires loading the MASS and nnet libraries;
# see http://www.stats.ox.ac.uk/pub/MASS4/
#
# The response for each observation is a multinomial vector. This can be represented in two ways:
#        (1) If there is a single trial for each observation, the actual value observed can be the
#            the response. The responses together form a factor.
#        (2) If there are multiple trials for observations, each observation has a set of K counts
#            corresponding to the number of outcomes at each level. The responses together form a
#            matrix.
#
library(nnet)
library(MASS)
#
# Missing data is not allowed. By omitting these observations up front the results are different
# from those in Minitab, where the effect of missing values depends on which variables are being
# used in the model.
#
univqual <- data.frame(School=university$School,SAT.1st.quartile=university$SAT.1st.quartile,
  SAT.3rd.quartile=university$SAT.3rd.quartile,Top.10.=university$Top.10.,
  Acceptance.rate=university$Acceptance.rate, Tier=Tier.ordered.down)
univqual <- na.omit(univqual)
#
# Nominal regression
#
attach(univqual)
#
# Using the ordering from best to worst group gives logits with the best group (Top 50) as the reference
#
univnom1 <- multinom(Tier ~ SAT.1st.quartile + SAT.3rd.quartile + Top.10. + Acceptance.rate)
#
# Wald tests for each coefficient of each underlying logistic regression can be obtined from the output
#
summary(univnom1)
exp(coef(univnom1))[,-1]
#
# The Anova() function from the car package will work here, and gives overall tests of significance for
# each variable, but it gives LR tests rather than Wald tests (which isn't necessarily a bad thing).
#
library(car)
Anova(univnom1, type=3)
plot(SAT.1st.quartile,SAT.3rd.quartile)
univnom2 <- multinom(Tier ~ SAT.3rd.quartile + Top.10. + Acceptance.rate)
summary(univnom2)
exp(coef(univnom2))[,-1]
Anova(univnom2, type=3)
#
# The fitted() command gives the estimated probabilities
#
data.frame(Tier,fitted(univnom2))
#
# The predict() command gives the chosen group, based on the estimated probabilities
#
table(Tier,predict(univnom2))
#
# Ordinal regression
#
# The polr() function, also from the MASS library, fits the proportional odds model
#
univord1 <- polr(Tier ~ SAT.1st.quartile + SAT.3rd.quartile + Top.10. + Acceptance.rate)
#
# The summary function doesn't work in S-PLUS here, because of instability in the fitting
#
summary(univord1)
exp(coef(univord1))
data.frame(Tier,fitted(univord1))
table(Tier,predict(univord1))
#
# As an alternative, use the lrm() function from the Design library 
#
library(Design)
univord1 <- lrm(Tier ~ SAT.1st.quartile + SAT.3rd.quartile + Top.10. + Acceptance.rate)
univord1
exp(coef(univord1))
#
# This is how to get the estimated probabilities if you used lrm()
#
data.frame(Tier,predict(univord1,type="fitted.ind"))
univstud <- data.frame(School=university$School,Alumni.giving.rate=university$Alumni.giving.rate,
  Pct.under.20=university$Pct.under.20,Freshman.retention=university$Freshman.retention,
  Graduation.rate=university$Graduation.rate, Reputation=university$Reputation,Tier=Tier.ordered.down)
univstud <- na.omit(univstud)
attach(univstud)
univord2 <- polr(Tier ~ Alumni.giving.rate + Pct.under.20 + Freshman.retention + Graduation.rate + Reputation)
summary(univord2)
exp(coef(univord2))
data.frame(Tier,fitted(univord2))
table(Tier,predict(univord2))
univord2 <- lrm(Tier ~ Alumni.giving.rate + Pct.under.20 + Freshman.retention + Graduation.rate + Reputation)
univord2
exp(coef(univord2))[-(1:3)]
data.frame(Tier,predict(univord2,type="fitted.ind"))
