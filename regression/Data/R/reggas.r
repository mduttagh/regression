#
# Estimating a demand function
#
gasdemand <- read.csv("c:/class/ascii/demand2.csv")
attach(gasdemand)
plot(logPG,logGpc,xlab="Logged price of gasoline",ylab="Logged gasoline consumption")
plot(Year,logGpc)
lines(Year, logGpc)
cor(cbind(logGpc,logPG,logI,logPNC,logPUC,logPPT,logPD,logPN,logPS,YR))
demanda <- lm(logGpc ~ logPG+logI+logPNC+logPUC+logPPT+logPD+logPN+logPS+YR+YRSQ)
summary(demanda)
#
# Load John Fox's car package (see http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/)
#
library(car)
vif(demanda)
#
# Best subsets is performed using the function leaps, which orders models by C_p by default; other criteria
# can be specified as well
#
# Must first install and load the "leaps" package
#
library(leaps)
leaps(cbind(logPG,logI,logPNC,logPUC,logPPT,logPD,logPN,logPS,YR,YRSQ),logGpc,nbest=2)
leaps(cbind(logPG,logI,logPNC,logPUC,logPPT,logPD,logPN,logPS,YR,YRSQ),logGpc,nbest=2,method="adjr2")
leaps(cbind(logPG,logI,logPNC,logPUC,logPPT,logPD,logPN,logPS,YR,YRSQ),logGpc,nbest=2,method="r2")
#
# Load Bill Venables and Brian Ripley's MASS library (see http://www.stats.ox.ac.uk/pub/MASS4/Software.html)
#
library(MASS)
#
# The extractAIC function will give the AIC value for any regression; with adjustment, it will
# give AIC_C. Note that the actual values of AIC or AIC_C are not the same as those given in
# the handout, but they differ by a constant, so all model comparisons are the same.
# AIC
#
extractAIC(lm(logGpc~1))
extractAIC(lm(logGpc~logI))
extractAIC(lm(logGpc~logI+logPS))
extractAIC(lm(logGpc~logPG+logI+YRSQ))
extractAIC(lm(logGpc~logPG+logI+logPN+logPS))
extractAIC(lm(logGpc~logPG+logI+logPD+logPN+logPS))
extractAIC(lm(logGpc~logPG+logI+logPUC+logPD+logPN+logPS))
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS))
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS+YR))
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS+YR+YRSQ))
extractAIC(lm(logGpc~logPG+logI+logPNC+logPUC+logPPT+logPD+logPN+logPS+YR+YRSQ))
#
# AIC_C 
#
n <- length(logGpc)
extractAIC(lm(logGpc~1))+2*2*3/(n-3)
extractAIC(lm(logGpc~logI))+2*3*4/(n-4)
extractAIC(lm(logGpc~logI+logPS))+2*4*5/(n-5)
extractAIC(lm(logGpc~logPG+logI+YRSQ))+2*5*6/(n-6)
extractAIC(lm(logGpc~logPG+logI+logPN+logPS))+2*6*7/(n-7)
extractAIC(lm(logGpc~logPG+logI+logPD+logPN+logPS))+2*7*8/(n-8)
extractAIC(lm(logGpc~logPG+logI+logPUC+logPD+logPN+logPS))+2*8*9/(n-9)
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS))+2*9*10/(n-10)
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS+YR))+2*10*11/(n-11)
extractAIC(lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS+YR+YRSQ))+2*11*12/(n-12)
extractAIC(lm(logGpc~logPG+logI+logPNC+logPUC+logPPT+logPD+logPN+logPS+YR+YRSQ))+2*12*13/(n-13)
demandb <- lm(logGpc~logPG+logI+logPUC+logPPT+logPD+logPN+logPS)
summary(demandb)
demandc <- lm(logGpc~logPG+logI+logPUC+logPD+logPN+logPS)
summary(demandc)
demandd <- lm(logGpc~logPG+logI+logPD+logPN+logPS)
summary(demandd)
vif(demandd)
std.resd <- rstandard(demandd)
hatd <- hatvalues(demandd)
cookd <- cooks.distance(demandd)
cbind(std.resd,hatd,cookd)
plot(fitted(demandd),std.resd,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resd)
#
# Added variable plot and partial correlation constructed by hand.
# The car library has a function av.plots that constructs added variable plots.
# Type av.plots(demandd), and then choose (by number) the variable that you are
# conditioning on (in this case logPG, which would be entered as 2).
#
adjcons <- residuals(lm(logGpc~logI+logPD+logPN+logPS))
adjprice <- residuals(lm(logPG~logI+logPD+logPN+logPS))
cor(adjcons,adjprice)
plot(adjprice,adjcons,xlab="Adjusted gasoline price index",ylab="Adjusted gasoline consumption")
demande <- lm(logGpc~logPG+logI+YRSQ)
summary(demande)
vif(demande)
std.rese <- rstandard(demande)
qqnorm(std.rese)
plot(fitted(demande),std.rese,xlab="Fitted values",ylab="Standardized residuals")
plot(logPG,std.rese,ylab="Standardized residuals")
plot(logI,std.rese,ylab="Standardized residuals")
plot(YRSQ,std.rese,ylab="Standardized residuals")
