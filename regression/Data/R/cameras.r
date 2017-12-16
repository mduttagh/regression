#
# Prices of digital cameras
#
cameras <- read.csv("c:/class/ascii/cameras.csv")
#
# The default coding for categorical (factor) variables in R is indicator variable coding (what R
# calls treatment contrasts), with the first category being the reference category. You can tell R
# to use effect codings by assigning what R calls sum contrasts to the factor variable with the
# command 
#       contrasts(cameras$Category) = contr.sum(6)
# (the "6" is because there are 6 levels to the Category variable). Of course, none of this affects
# the actual fit of the model, F-tests, etc.
#
attach(cameras)
#
# This applied functions like mean and sd to subgroups of the data
#
sapply(split(Price,Category),summary)
sapply(split(Price,Category),sd)
boxplot(split(Price,Category))
#
# Note that since Category includes text, it is automatically a factor
# variable. If X defines the groups using only numbers, it has to be
# entered into the model as factor(X).
#
cameraa <- lm(Price ~ Category)
anova(cameraa)
summary(cameraa)
#
# Function for multiple comparisons in R. It does not give the Tukey adjustment, but
# does give the Bonferroni adjustment, which is usually similar
#
pairwise.t.test(Price,Category,p.adj="bonf")
#
# A better alternative is the multcomp package, which does give Tukey comparisons
#
library(multcomp)
camera.tukey = glht(cameraa, linfct=mcp(Category="Tukey"))
summary(camera.tukey) 
#
# This gives the letter summary of significant group differences, along with boxplots
#
plot(cld(camera.tukey)) 
#
# This gives simultaneous confidence intervals (and a plot of them) for the pairwise differences. Note that
# the group difference labels on the vertical axis might not fit in the plot if the group names are too
# long; I haven't figured out a way to fix this.
#
print(confint(camera.tukey))
plot(print(confint(camera.tukey)))
#
# The following functions can be used for any regression fit, but are useful in that they automatically
# handle ANOVA models.
#
sresa = rstandard(cameraa)
hata = hatvalues(cameraa)
cooka = cooks.distance(cameraa)
cbind(sresa,hata,cooka)
plot(fitted(cameraa),sresa,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(sresa,ylab="Standardized residuals")
#
# Levene's test
#
absres <- abs(sresa)
anova(lm(absres ~ Category))
#
# Analysis for logged price
#
log.Price <- log10(Price)
boxplot(split(log.Price,Category))
camerab <- lm(log.Price ~ Category)
anova(camerab)
summary(glht(camerab, linfct=mcp(Category="Tukey")))  
sresb = rstandard(camerab)
hatb = hatvalues(camerab)
cookb = cooks.distance(camerab)
cbind(sresb,hatb,cookb)
plot(fitted(camerab),sresb,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(sresb,ylab="Standardized residuals")
absres <- abs(sresb)
anova(lm(absres ~ Category))
data.frame(Brand,Model,sresb,hatb,cookb)
boxplot(split(sresb,CR.Best.Buy))
#
# Getting weights
#
groupsd <- sapply(split(sresa,Category),sd)
wt <- 1/(groupsd[as.numeric(Category)]^2)
#
# Setting weights for point-and-shoot cameras to 1
#
wt[Category=="4 (Point-and-shoot)"] <- 1
camerac <- lm(Price ~ Category,weight=wt)
anova(camerac)
summary(camerac)
summary(glht(camerac, linfct=mcp(Category="Tukey")))  
sresc = rstandard(camerac)
plot(fitted(camerac),sresc,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(sresc,ylab="Standardized residuals")
absres <- abs(sresc)
anova(lm(absres ~ Category))
camerad <- lm(log.Price ~ as.numeric(Category))
summary(camerad)
#
# Partial F-test
#
anova(camerad,camerab)
#
# As is true for any regression (lm) model, predictions are made using the predict() command. Put the predictor values (here camera categories)
# you are interested in in a new data frame with the same variable name, put associated weights in a weight variable if doing WLS,
# and then (for predictions based on camerac, for example)
#
#       predict(camerac, data.new, interval="prediction", weights=data.new$wt)
#
