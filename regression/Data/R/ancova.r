#
# Analysis of covariance
#
breaks <- read.csv("c:/class/ascii/breaks.csv")
#
# Makes analyses based on effect codings, and also puts days of week in order for things like plots
#
breaks$Day.of.week <- factor(breaks$Day.of.week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"), ordered=T)
contrasts(breaks$Day.of.week) <- contr.sum(5)
attach(breaks)
summary(Total_Breaks)
summary(Trade_Total)
summary(Break_Rate)
hist(Break_Rate)
hist(Logged.break.rate)
boxplot(split(Logged.break.rate,Day.of.week))
#
# This works too
#
boxplot(Logged.break.rate~Day.of.week)
plot(Trade_Total,Logged.break.rate)
#
# Note that Day of week would have been automatically recognized as categorical anyway, since it is a text variable
#
breaka <- aov(Logged.break.rate ~ Trade_Total + Day.of.week)
coef(breaka)
library(car)
Anova(breaka, type=3)
summary(lm(Logged.break.rate ~ Trade_Total + Day.of.week))
#
# This gives effects for the categorical predictor; that is, estimates of the alphas. The least squares mean for 
# each day of the week is this value added to the contributions from the numerical variable(s) evaluated at 
# their means
#
model.tables(breaka,cterms="Day.of.week")
#
# LS means; actually seem to be very slightly off. A more correct version would be based on the slopes
# themselves
#
model.tables(breaka,cterms="Day.of.week")$tables$Day.of.week+coef(breaka)[1]+coef(breaka)[2]*mean(Trade_Total)
sapply(split(Logged.break.rate,Day.of.week),summary)
library(multcomp)
break.tukey <- glht(breaka, linfct=mcp(Day.of.week="Tukey"))
summary(break.tukey)
print(confint(break.tukey))
plot(print(confint(break.tukey)))
std.resa <- rstandard(breaka)
hata <- hatvalues(breaka)
cooka <- cooks.distance(breaka)
plot(fitted(breaka),std.resa,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resa,ylab="Standardized residuals")
#
# This gives all of the diagnostics, just for fun (?)
#
cbind(std.resa, hata, cooka)
sapply(split(std.resa,Day.of.week),summary)
boxplot(split(std.resa,Day.of.week))
plot(Trade_Total,std.resa,ylab="Standardized residuals")
absres <- abs(std.resa)
Anova(aov(absres ~ Day.of.week), type=3)
n <- length(Logged.break.rate)
plot(c(1:n),std.resa,xlab="Order of data",ylab="Standardized residuals")
#
# Function from earlier time series handouts
#
runs.test(std.resa, cont.corr=T)
acf(std.resa, xlim=c(1,24), ylim=c(-.2,.2))
#
# Getting weights
#
groupsd <- sapply(split(std.resa,Day.of.week),sd)
wt <- 1/(groupsd[as.numeric(Day.of.week)]^2)
breakb <- aov(Logged.break.rate ~ Trade_Total + Day.of.week, weight=wt)
coef(breakb)
Anova(breakb, type=3)
summary(lm(Logged.break.rate ~ Trade_Total + Day.of.week, weight=wt))
#
# LS means, based on effect coding slopes
#
coef(breakb)[1]+coef(breakb)[2]*mean(Trade_Total)+c(coef(breakb)[3:6],-sum(coef(breakb)[3:6]))
#
std.resb <- rstandard(breakb)
hatb <- hatvalues(breakb)
cookb <- cooks.distance(breakb)
plot(fitted(breakb),std.resb,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resb,ylab="Standardized residuals")
cbind(std.resb, hatb, cookb)
breaks2 <- data.frame(breaks, wt)
breaks2$Logged.break.rate[(std.resb > 3)] <- NA
rm(wt)
breaks2 <- na.omit(breaks2)
attach(breaks2)
breakc <- aov(Logged.break.rate ~ Trade_Total + Day.of.week, weight=wt)
coef(breakc)
Anova(breakc, type=3)
summary(lm(Logged.break.rate ~ Trade_Total + Day.of.week, weight=wt))
#
# Don't forget that diagnostics based on this model fit MUST use the wt=wt setting within the call to lsfit();
# see either the "Prices of digital cameras" code or the "Modeling television viewership" code for examples
#
# LS means, based on effect coding slopes
#
coef(breakc)[1]+coef(breakc)[2]*mean(Trade_Total)+c(coef(breakc)[3:6],-sum(coef(breakc)[3:6]))
#
# As is true for any regression (lm or aov) model, predictions are made using the predict() command. Put the predictor values 
# you are interested in in a new data frame with the same variable names, and put associated weights in a weight variable if doing WLS
#
data.new <- data.frame(Trade_Total=c(3100), Day.of.week=c("Monday"), wt=c(.5251))
predict(breakc, data.new, interval="prediction", weights=data.new$wt)
breakd <- aov(Logged.break.rate ~ Trade_Total + Day.of.week + Trade_Total*Day.of.week, weight=wt)
coef(breakd)
Anova(breakd, type=3)
summary(lm(Logged.break.rate ~ Trade_Total + Day.of.week + Trade_Total*Day.of.week, weight=wt))
#
# The effects package provides a command that will give a plot of the different slopes for the different groups
# corresponding to the interaction. By setting the "xlevels" setting to a large number within the effects call
# it gives the straight lines we want. It will also give the parallel lines for the constant shift model if
# that is what has been fit in the model
#
library(effects)
#
# Interaction (different slopes)
#
plot(effect(term="Trade_Total*Day.of.week", mod=breakd, xlevels=100), multiline=TRUE)
#
# Constant shift (same slopes); will get a note that interaction isn't in the model, but the parallel lines are
# plotted
#
plot(effect(term="Trade_Total*Day.of.week", mod=breakc, xlevels=100), multiline=TRUE)
