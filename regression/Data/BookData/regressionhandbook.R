#
# This is the code used to construct the figures and output in the book "Handbook of Regression Analysis" by Samprit Chatterjee and
# Jeffrey S. Simonoff, published by John Wiley and Sons in 2013 as part of their Handbooks in Applied Statistics series. Note that
# only the code used to produce the output and figures in the book is included here; this should not be taken to necessarily represent 
# all of the analyses that would be done in a complete analysis of the data. Although the code has been tested, I cannot guarantee that 
# it is error-free; it should be viewed as being available at the user's risk. 
#
# Note that libraries are loaded once for use in a given chapter.
#
# Chapter 1
#
#
# Section 1.2
#
# Figure 1.1
#
set.seed(100)
x = runif(30,0,10)
x[28] = 8.5
y = 10 + 2*x + rnorm(30)*3
#
# pch = 1 open circle, 2 triangle, 3 plus, 4 x, 5 diamond, 6 inverted triangle; 19 solid circle
#
plot(x, y, pch=19)
abline(a=10, b=2)
text(5.8, 18, expression(E(y) == beta[0] + beta[1]*x), pos=4)
arrows(6, 18.8, 5.9, 21.8, length=.15)
segments(x, y, x, 10+2*x, lty=3)
dev.off()
#
# Figure 1.2
#
plot(x, y, pch=19)
abline(a=10, b=2, col="grey")
regxy = lm(y~x)
abline(regxy)
text(6.6, 18, expression(hat(E)(y) == hat(beta)[0] + hat(beta)[1]*x), pos=4)
arrows(7.4, 18.8, 7.4, coef(regxy)%*%c(1,7.4), length=.15)
segments(x, y, x, fitted(regxy), lty=3)
dev.off()
#
# Figure 1.3
#
#
# http://cran.r-project.org/web/packages/scatterplot3d/index.html
#
library(scatterplot3d)
set.seed(100)
x1 = runif(30,0,10)
x2 = runif(30,0,10)
y = 10 + 2*x1 + 2*x2 + rnorm(30)*3
lin3d = scatterplot3d(x1, x2, y, pch = 19, xlab=expression(x[1]), ylab=expression(x[2]),
  box=F)
regx1x2y = lm(y~x1+x2)
lin3d$plane3d(regx1x2y, lty="solid")
orig = lin3d$xyz.convert(x1, x2, y)
plane = lin3d$xyz.convert(x1, x2, fitted(regx1x2y))
i.negpos = 1 + (resid(regx1x2y) > 0)
segments(orig$x, orig$y, plane$x, plane$y, lty = (2:1)[i.negpos])
dev.off()
#
# Section 1.4
#
# Figure 1.4
#
homeprices = read.csv("homeprices.csv")
par(mfrow=c(3,2))
plot(homeprices$Bedrooms, homeprices$Sale.price, xlab="Number of bedrooms", ylab="Sale price", pch = 19)
plot(homeprices$Bathrooms, homeprices$Sale.price, xlab="Number of bathrooms", ylab="Sale price", pch = 19)
plot(homeprices$Living.area, homeprices$Sale.price, xlab="Living area", ylab="Sale price", pch = 19)
plot(homeprices$Lot.size, homeprices$Sale.price, xlab="Lot size", ylab="Sale price", pch = 19)
plot(homeprices$Year.built, homeprices$Sale.price, xlab="Year built", ylab="Sale price", pch = 19)
plot(homeprices$Property.tax, homeprices$Sale.price, xlab="Property taxes", ylab="Sale price", pch = 19)
dev.off()
homeprice.lm = lm(Sale.price ~ Bedrooms+Bathrooms+Living.area+Lot.size+Year.built+Property.tax, data=homeprices)
summary(homeprice.lm)
new.homes = data.frame(Bedrooms=homeprices$Bedrooms.new[1:20], Bathrooms=homeprices$Bathrooms.new[1:20], Living.area=homeprices$Living.area.new[1:20],
  Lot.size=homeprices$Lot.size.new[1:20], Year.built=homeprices$Year.built.new[1:20], Property.tax=homeprices$Property.tax.new[1:20], Sale.price=homeprices$Sale.price.new[1:20]) 
predict(homeprice.lm, new.homes[1,], interval=c("p"))
predict(homeprice.lm, new.homes[1,], interval=c("c"))
predict(homeprice.lm, new.homes[1,], interval=c("p"), level=.5)
#
# Figure 1.5
#
par(mfrow=c(1,2))
plot(fitted(homeprice.lm), resid(homeprice.lm), xlab="Fitted values", ylab="Residuals", main="(a)", pch = 19)
qqnorm(resid(homeprice.lm), main="(b)", pch = 19)
dev.off()
#
# Figure 1.6
#
par(mfrow=c(3,2))
plot(homeprices$Bedrooms, resid(homeprice.lm), xlab="Number of bedrooms", ylab="Residuals", pch = 19)
plot(homeprices$Bathrooms, resid(homeprice.lm), xlab="Number of bathrooms", ylab="Residuals", pch = 19)
plot(homeprices$Living.area, resid(homeprice.lm), xlab="Living area", ylab="Residuals", pch = 19)
plot(homeprices$Lot.size, resid(homeprice.lm), xlab="Lot size", ylab="Residuals", pch = 19)
plot(homeprices$Year.built, resid(homeprice.lm), xlab="Year built", ylab="Residuals", pch = 19)
plot(homeprices$Property.tax, resid(homeprice.lm), xlab="Property taxes", ylab="Residuals", pch = 19)
dev.off()
#
# Chapter 2
#
#
# Section 2.2
#
# Figure 2.1
#
library(scatterplot3d)
par(mfrow=c(2,1))
set.seed(101)
x1 = rnorm(30)
x2 = rnorm(30)
x2 = .999*x1 + sqrt(1 - .999^2)*x2
x1 = 5 + 2.5*x1
x2 = 5 + 2.5*x2
y = 10 + 2*x1 + 2*x2 + rnorm(30)*3
x115 = x1[15]
x1[15] = 5
lin3d = scatterplot3d(x1, x2, y, pch = 19, xlab=expression(x[1]), ylab=expression(x[2]),
  type="h", box=F, color=c(rep(1,14),5,rep(1,15)))
collinx1x2y = lm(y~x1+x2)
collinx1x2y
lin3d$plane3d(collinx1x2y, lty="solid")
x1[15] = x115
x2[15] = 5
lin3d = scatterplot3d(x1, x2, y, pch = 19, xlab=expression(x[1]), ylab=expression(x[2]),
  type="h", box=F, color=c(rep(1,14),5,rep(1,15)))
collinx1x2y = lm(y~x1+x2)
collinx1x2y
lin3d$plane3d(collinx1x2y, lty="solid")
dev.off()
#
# http://cran.r-project.org/web/packages/car/index.html
#
library(car)
vif(homeprice.lm)
#
# Section 2.3
#
# http://cran.r-project.org/web/packages/leaps/index.html
#
library(leaps)
#
# This is not the output provided in the book
#
leaps(homeprices[,1:6], homeprices[,7])
sigmahat = c(52576,54932,61828,48091,51397,51870,47092,47635,48346,46885,47304,47380,47094,47162,47599,47381)
npred = c(rep(c(1:5),each=3),6)
nobs = 85
aic = nobs*log(sigmahat^2) + nobs*log((nobs-npred-1)/nobs) + 2.*npred + 2
aic
homeprice.lm4 = lm(Sale.price ~ Bedrooms+Bathrooms+Living.area+Year.built, data=homeprices)
summary(homeprice.lm4)
vif(homeprice.lm4)
homeprice.lm3 = lm(Sale.price ~ Bathrooms+Living.area+Year.built, data=homeprices)
summary(homeprice.lm3)
vif(homeprice.lm3)
#
# Figure 2.2
#
par(mfrow=c(1,2))
plot(fitted(homeprice.lm3), resid(homeprice.lm3), xlab="Fitted values", ylab="Residuals", main="(a)", pch = 19)
qqnorm(resid(homeprice.lm3), main="(b)", pch = 19)
dev.off()
homeprice.pred = predict(homeprice.lm3, new.homes, interval=c("p"))
#
# Figure 2.3
#
plot(homeprice.pred[,1], new.homes$Sale.price, ylim=c(100000,650000), xlab="Predicted sale price",
  ylab="Observed sale price", pch=19)
abline(0, 1, lty="dotted")
points(homeprice.pred[,1], homeprice.pred[,2], pch="_")
points(homeprice.pred[,1], homeprice.pred[,3], pch="_")
dev.off()
new.homes$Sale.price-homeprice.pred[,1]
mean(new.homes$Sale.price)
mean(homeprice.pred[,1]-new.homes$Sale.price)
mean(abs(homeprice.pred[,1]-new.homes$Sale.price))
sd(homeprice.pred[,1]-new.homes$Sale.price)
mean((homeprice.pred[,1]-new.homes$Sale.price)[-c(13,16)])
mean(abs((homeprice.pred[,1]-new.homes$Sale.price)[-c(13,16)]))
sd((homeprice.pred[,1]-new.homes$Sale.price)[-c(13,16)])
#
# Section 2.4
#
election2004 = read.csv("election2004.csv")
#
# Figure 2.4
#
par(mfrow=c(1,2))
plot(election2004$Bush.pct.2000, election2004$Change.in.Bush.pct, xlab="2000 Bush pct.", 
  ylab="Change in voting pct.", pch = 19, main="(a)")
boxplot(election2004$Change.in.Bush.pct~election2004$e.Voting, xlab="2004 electronic voting",
  ylab="Change in voting pct.", main="(b)")
dev.off()
election.pooled = lm(Change.in.Bush.pct~Bush.pct.2000, data=election2004)
summary(election.pooled)
election.cshift = lm(Change.in.Bush.pct~Bush.pct.2000+e.Voting, data=election2004)
summary(election.cshift)
vif(election.cshift)
Bush.2000Xe.Voting = election2004$Bush.pct.2000 * election2004$e.Voting
election2004 = data.frame(election2004, Bush.2000Xe.Voting)
election.full = lm(Change.in.Bush.pct~Bush.pct.2000+e.Voting+Bush.2000Xe.Voting, data=election2004)
summary(election.full)
vif(election.full)
anova(election.pooled, election.full)
#
# Figure 2.5
#
plot(election2004$Bush.pct.2000[election2004$e.Voting==0], 
  election2004$Change.in.Bush.pct[election2004$e.Voting==0], xlab="2000 Bush percentage", 
  ylab="Change in voting percentage", pch = 3)
points(election2004$Bush.pct.2000[election2004$e.Voting==1], 
  election2004$Change.in.Bush.pct[election2004$e.Voting==1])
abline(-5.239, .162, lty="dotted")
abline(4.434, -.038, lty="dashed")
legend(35, 10.5, c("e-Voting", "No e-Voting"), pch=c(1, 3), lty=c("dashed","dotted"))
dev.off()
#
# Figure 2.6
#
par(mfrow=c(2,2))
plot(election2004$Voter.turnout.2000, election2004$Change.in.Bush.pct, xlab="2000 Turnout", 
  ylab="Change in voting pct.", pch = 19, main="(a)")
plot(election2004$Voter.turnout.2004, election2004$Change.in.Bush.pct, xlab="2004 Turnout", 
  ylab="Change in voting pct.", pch = 19, main="(b)")
plot(election2004$Median.income, election2004$Change.in.Bush.pct, xlab="Median income", 
  ylab="Change in voting pct.", pch = 19, main="(c)")
plot(election2004$Hispanic.population.pct, election2004$Change.in.Bush.pct, xlab="Hispanic pct.", 
  ylab="Change in voting pct.", pch = 19, main="(d)")
dev.off()
election.full.control = lm(Change.in.Bush.pct~Bush.pct.2000+e.Voting+Bush.2000Xe.Voting+Voter.turnout.2000+
  Voter.turnout.2004+Median.income+Hispanic.population.pct, data=election2004)
summary(election.full.control)
vif(election.full.control)
election2004 = data.frame(election2004, Change.in.turnout=election2004$Voter.turnout.2004-
  election2004$Voter.turnout.2000)
election.full.control2 = lm(Change.in.Bush.pct~Bush.pct.2000+e.Voting+Bush.2000Xe.Voting+Change.in.turnout+
  Median.income+Hispanic.population.pct, data=election2004)
summary(election.full.control2)
vif(election.full.control2)
anova(election.full.control2, election.full.control)
#
# Figure 2.7
#
par(mfrow=c(3,3))
plot(fitted(election.full.control2), resid(election.full.control2), xlab="Fitted values", ylab="Residuals", 
  pch = 19)
qqnorm(resid(election.full.control2), pch = 19)
plot(election2004$Bush.pct.2000, resid(election.full.control2), xlab="2000 Bush pct.", 
  ylab="Residuals", pch = 19)
boxplot(resid(election.full.control2)~election2004$e.Voting, xlab="2004 electronic voting",
  ylab="Residuals")
plot(election2004$Change.in.turnout, resid(election.full.control2), xlab="Change in turnout", 
  ylab="Residuals", pch = 19)
plot(election2004$Median.income, resid(election.full.control2), xlab="Median income", 
  ylab="Residuals", pch = 19)
plot(election2004$Hispanic.population.pct, resid(election.full.control2), xlab="Hispanic pct.", 
  ylab="Residuals", pch = 19)
dev.off()
#
# Chapter 3
#
# Section 3.2
#
# Figure 3.1
#
set.seed(103)
x = rnorm(99)
z = rnorm(99)
y = .8*x + sqrt(.36)*z
x = x+5
y = y+5
x1 = c(x, 15)
y1 = c(y, 3.5)
plot(x1, y1, pch = 19, xlab="x", ylab="y")
abline(lsfit(x1, y1))
abline(lsfit(x, y), lty=2)
text(14.6,3.5,"A")
dev.off()
#
# Section 3.3
#
# Figure 3.2
#
par(mfrow=c(2,1))
x1 = c(x,15)
y1 = c(y, 1 + .85*15)
plot(x1, y1, pch = 19, xlab="x", ylab="y")
abline(lsfit(x1, y1))
abline(lsfit(x, y), lty=2)
x1 = c(x,5)
y1 = c(y, 8.5 + .85*5)
plot(x1, y1, pch = 19, xlab="x", ylab="y")
abline(lsfit(x1, y1))
abline(lsfit(x, y), lty=2)
dev.off()
#
# Section 3.4
#
# Figure 3.3
#
sresprice1 = rstandard(homeprice.lm3)
hatprice1 = hatvalues(homeprice.lm3)
cookprice1 = cooks.distance(homeprice.lm3)
par(mfrow=c(3,1))
plot(c(1:85), sresprice1, ylim=c(-2.5,2.5), xlab="Index", ylab="Std. residuals", pch=19, 
  main="Standardized residuals")
abline(h=2.5, lty=3)
abline(h=-2.5, lty=3)
plot(c(1:85), hatprice1, xlab="Index", ylab="Leverage", pch=19, main="Diagonal elements of the hat matrix")
abline(h=2.5*4/85, lty=3)
plot(c(1:85), cookprice1, xlab="Index", ylab="Cook's D", pch=19, main="Cook's distances")
dev.off()
#
# Figure 3.4
#
plot(hatprice1, cookprice1, pch = 19, xlab="Leverage values", ylab="Cook's distances")
dev.off()
#
# Figure 3.5
#
homeprices2 = homeprices[-c(4,11,29,48,71),]
library(leaps)
leaps(homeprices2[,1:6], homeprices2[,7])
homeprice.lm3.out = lm(Sale.price ~ Bathrooms+Living.area+Year.built, data=homeprices2)
summary(homeprice.lm3.out)
library(car)
vif(homeprice.lm3.out)
homeprice.lm2.out = lm(Sale.price ~ Bathrooms+Living.area, data=homeprices2)
summary(homeprice.lm2.out)
vif(homeprice.lm2.out)
sresprice2 = rstandard(homeprice.lm2.out)
hatprice2 = hatvalues(homeprice.lm2.out)
cookprice2 = cooks.distance(homeprice.lm2.out)
par(mfrow=c(3,1))
plot(c(1:80), sresprice2, ylim=c(-2.5,2.5), xlab="Index", ylab="Std. residuals", pch=19, 
  main="Standardized residuals")
abline(h=2.5, lty=3)
abline(h=-2.5, lty=3)
plot(c(1:80), hatprice2, ylim=c(0,.15), xlab="Index", ylab="Leverage", pch=19, 
  main="Diagonal elements of the hat matrix")
abline(h=2.5*4/80, lty=3)
plot(c(1:80), cookprice2, xlab="Index", ylab="Cook's D", pch=19, main="Cook's distances")
dev.off()
#
# Chapter 4
#
# Section 4.4
#
movies2009 = read.csv("movies2009.csv")
#
# Figure 4.1
#
par(mfrow=c(2,2))
plot(movies2009$Opening, movies2009$Total.Gross, xlab="Opening weekend gross", ylab="Total domestic gross",
  pch = 19)
plot(movies2009$Screens, movies2009$Total.Gross, xlab="Opening screens", ylab="Total domestic gross",
  pch = 19)
plot(movies2009$Budget, movies2009$Total.Gross, xlab="Production budget", ylab="Total domestic gross",
  pch = 19)
plot(movies2009$RT, movies2009$Total.Gross, xlab="Rotten Tomatoes rating", ylab="Total domestic gross",
  pch = 19)
dev.off()
Log.domestic.gross = log10(movies2009$Total.Gross)
Log.opening.gross = log10(movies2009$Opening)
Log.budget = log10(movies2009$Budget)
movies2009 = data.frame(movies2009, Log.domestic.gross, Log.opening.gross, Log.budget)
#
# Figure 4.2
#
par(mfrow=c(2,2))
plot(movies2009$Log.opening.gross, movies2009$Log.domestic.gross, xlab="Logged opening weekend gross",
  ylab="Logged total domestic gross", pch = 19)
plot(movies2009$Screens, movies2009$Log.domestic.gross, xlab="Opening screens", 
  ylab="Logged total domestic gross", pch = 19)
plot(movies2009$Log.budget, movies2009$Log.domestic.gross, xlab="Logged production budget", 
  ylab="Logged total domestic gross", pch = 19)
plot(movies2009$RT, movies2009$Log.domestic.gross, xlab="Rotten Tomatoes rating", 
  ylab="Logged total domestic gross", pch = 19)
dev.off()
movies.lm1 = lm(Log.domestic.gross ~ Log.opening.gross + Screens + Log.budget + RT, data=movies2009)
summary(movies.lm1)
library(car)
vif(movies.lm1)
library(leaps)
leaps(movies2009[-c(9,21,36,43,58,78,102,108,117),c(12,4,13,5)], movies2009[-c(9,21,36,43,58,78,102,108,117),11])
movies.lm2 = lm(Log.domestic.gross ~ Log.opening.gross + Log.budget + RT, data=movies2009)
summary(movies.lm2)
vif(movies.lm2)
sresmovie2 = rstandard(movies.lm2)
hatmovie2 = hatvalues(movies.lm2)
cookmovie2 = cooks.distance(movies.lm2)
#
# Figure 4.3
#
par(mfrow=c(3,1))
plot(c(1:120), sresmovie2, ylim=c(-2.5,2.5), xlab="Index", ylab="Std. residuals", pch=19, 
  main="Standardized residuals")
abline(h=2.5, lty=3)
abline(h=-2.5, lty=3)
plot(c(1:120), hatmovie2, ylim=c(0,.45), xlab="Index", ylab="Leverage", pch=19, 
  main="Diagonal elements of the hat matrix")
abline(h=2.5*4/120, lty=3)
plot(c(1:120), cookmovie2, xlab="Index", ylab="Cook's D", pch=19, main="Cook's distances")
dev.off()
leaps(movies2009[-c(9,21,36,43,58,78,102,106,108,117),c(12,4,13,5)], 
  movies2009[-c(9,21,36,43,58,78,102,106,108,117),11])
movies.lm3 = lm(Log.domestic.gross ~ Log.opening.gross + Log.budget + RT, data=movies2009[-106,])
summary(movies.lm3)
vif(movies.lm3)
sresmovie3 = rstandard(movies.lm3)
hatmovie3 = hatvalues(movies.lm3)
cookmovie3 = cooks.distance(movies.lm3)
#
# Figure 4.4
#
par(mfrow=c(3,1))
plot(c(1:119), sresmovie3, ylim=c(-2.5,2.5), xlab="Index", ylab="Std. residuals", pch=19, 
  main="Standardized residuals")
abline(h=2.5, lty=3)
abline(h=-2.5, lty=3)
plot(c(1:119), hatmovie3, xlab="Index", ylab="Leverage", pch=19, 
  main="Diagonal elements of the hat matrix")
abline(h=2.5*4/119, lty=3)
plot(c(1:119), cookmovie3, xlab="Index", ylab="Cook's D", pch=19, main="Cook's distances")
dev.off()
#
# Figure 4.5
#
par(mfrow=c(3,2))
plot(fitted(movies.lm3), sresmovie3, xlab="Fitted values", ylab="Std. residuals", 
  pch = 19)
qqnorm(sresmovie3, pch = 19)
plot(movies2009$Log.opening.gross[-c(9,21,36,43,58,78,102,106,108,117)], sresmovie3, 
  xlab="Logged opening weekend gross", ylab="Std. residuals", pch = 19)
plot(movies2009$Log.budget[-c(9,21,36,43,58,78,102,106,108,117)], sresmovie3, xlab="Logged production budget", 
  ylab="Std. residuals", pch = 19)
plot(movies2009$RT[-c(9,21,36,43,58,78,102,106,108,117)], sresmovie3, xlab="Rotten Tomatoes rating", 
  ylab="Std. residuals", pch = 19)
dev.off()
movies2010 = read.csv("movies2010.csv")
Log.domestic.gross = log10(movies2010$Total.Gross)
Log.opening.gross = log10(movies2010$Opening)
Log.budget = log10(movies2010$Budget)
movies2010 = data.frame(movies2010, Log.domestic.gross, Log.opening.gross, Log.budget)
movievalidate = predict(movies.lm3, movies2010, interval=c("p"))
data.frame(movies2010$Movie,10^movievalidate[,2],movies2010$Total.Gross,10^movievalidate[,3])
#
# Chapter 5
#
# Section 5.2
#
# Figure 5.1
#
# http://cran.r-project.org/web/packages/lattice/index.html
#
library(lattice)
rho = rep(c(-90:90)/100, 6)
lambda = rep(c(0,1,3,5,7,9)/10, each=181)
efficiency = (1+rho*lambda)*(1+rho^2-2*rho*lambda)/((1-rho^2)*(1-rho*lambda))
xyplot(efficiency ~ rho | lambda, type="l", col=1, xlab=as.expression(substitute(paste(rho))),
  strip = strip.custom(var.name = as.expression(substitute(paste(lambda))), strip.levels=c(TRUE,TRUE)),
                panel=function(...){
		panel.xyplot(...)
		panel.abline(v=0)
		panel.abline(h=1)}, ylab="Inefficiency of OLS estimates")
dev.off()
#
# Figure 5.2
#
bias = ((1-rho*lambda)/(1+rho*lambda) - 1)*100
xyplot(bias ~ rho | lambda, type="l", col=1, xlab=as.expression(substitute(paste(rho))),
  strip = strip.custom(var.name = as.expression(substitute(paste(lambda))), strip.levels=c(TRUE,TRUE)),
                panel=function(...){
		panel.xyplot(...)
		panel.abline(v=0)
		panel.abline(h=0)
		panel.abline(h=-100,lty=2)}, ylab="Percentage bias of estimated variance")
dev.off()
#
# Section 5.4
#
ecommerce = read.csv("ecommerce.csv")
#
# Figure 5.3
#
Year = seq(1999.9167, 2011.1667, .25)
plot(Year, ecommerce$e.Commerce.sales, type="l", ylab="e-Commerce retail sales")
dev.off()
#
# Figure 5.4
#
plot(ecommerce$Total.sales, ecommerce$e.Commerce.sales, ylab="e-Commerce retail sales",
  xlab="Total retail sales", pch=19)
dev.off()
ecommerce.lm1 = lm(e.Commerce.sales ~ Total.sales, data=ecommerce)
summary(ecommerce.lm1)
#
# http://cran.r-project.org/web/packages/lmtest/index.html
#
library(lmtest)
dwtest(ecommerce.lm1)
resid.acf = acf(resid(ecommerce.lm1))
resid.acf$acf[1] = NA
rdiag = ls.diag(ecommerce.lm1)
#
# Figure 5.5
#
par(mfrow=c(1,2))
plot(Year, rdiag$std.res, ylab="Standardized residuals", main="(a)", pch = 19)
plot(resid.acf, main="(b)")
dev.off()
Quarter = c("Q4",rep(c("Q1","Q2","Q3","Q4"),11),"Q1")
#
# Figure 5.6
#
boxplot(rdiag$std.res~Quarter, ylab="Standardized residuals")
dev.off()
ecommerce.lm2 = lm(e.Commerce.sales ~ Total.sales+Quarter.1+Quarter.2+Quarter.3, data=ecommerce)
summary(ecommerce.lm2)
anova(ecommerce.lm1, ecommerce.lm2)
dwtest(ecommerce.lm2)
resid.acf = acf(resid(ecommerce.lm2))
resid.acf$acf[1] = NA
rdiag = ls.diag(ecommerce.lm2)
#
# Figure 5.7
#
par(mfrow=c(1,2))
plot(Year, rdiag$std.res, ylab="Standardized residuals", main="(a)", pch = 19)
plot(resid.acf, main="(b)")
dev.off()
ecommerce.lm3 = lm(e.Commerce.sales ~ Total.sales+Quarter.1+Quarter.2+Quarter.3+Recession, data=ecommerce)
summary(ecommerce.lm3)
dwtest(ecommerce.lm3)
resid.acf = acf(resid(ecommerce.lm3))
resid.acf$acf[1] = NA
rdiag = ls.diag(ecommerce.lm3)
#
# Figure 5.8
#
par(mfrow=c(3,2))
plot(fitted(ecommerce.lm3), rdiag$std.res, xlab="Fitted values", ylab="Standardized residuals", 
  main="(a)", pch = 19)
plot(Year, rdiag$std.res, ylab="Standardized residuals", main="(b)", pch = 19)
qqnorm(rdiag$std.res, main="(c)", pch = 19)
plot(resid.acf, main="(d)")
boxplot(rdiag$std.res~Quarter, ylab="Standardized residuals", main="(e)")
boxplot(rdiag$std.res~ecommerce$Recession, xlab="Recession", ylab="Standardized residuals", main="(f)")
dev.off()
#
# http://cran.r-project.org/web/packages/tseries/index.html
#
library(tseries)
runs.test(factor(sign(resid(ecommerce.lm3))))
sandpindices = read.csv("sandpindices.csv")
#
# Figure 5.9
#
par(mfrow=c(1,2))
plot(sandpindices$S.P.Small.cap, sandpindices$S.P.Large.cap, xlab="S&P Small-cap Index", ylab="S&P Large-cap index", pch = 19)
plot(sandpindices$S.P.Mid.cap, sandpindices$S.P.Large.cap, xlab="S&P Mid-cap Index", ylab="S&P Large-cap index", pch = 19)
dev.off()
log.S.P.Small.cap = log(sandpindices$S.P.Small.cap)
log.S.P.Mid.cap = log(sandpindices$S.P.Mid.cap)
log.S.P.Large.cap = log(sandpindices$S.P.Large.cap)
#
# Figure 5.10
#
par(mfrow=c(1,2))
plot(log.S.P.Small.cap, log.S.P.Large.cap, xlab="Logged S&P Small-cap Index", ylab="Logged S&P Large-cap index", pch = 19)
plot(log.S.P.Mid.cap, log.S.P.Large.cap, xlab="Logged S&P Mid-cap Index", ylab="Logged S&P Large-cap index", pch = 19)
dev.off()
sandpindices.lm1 = lm(log.S.P.Large.cap ~ log.S.P.Small.cap + log.S.P.Mid.cap)
summary(sandpindices.lm1)
dwtest(sandpindices.lm1)
resid.acf = acf(resid(sandpindices.lm1))
resid.acf$acf[1] = NA
rdiag = ls.diag(sandpindices.lm1)
Year = 2006 + c(1:1259)*5/1260
#
# Figure 5.11
#
par(mfrow=c(1,2))
plot(Year, rdiag$std.res, ylab="Standardized residuals", main="(a)", pch = 19)
plot(resid.acf, main="(b)")
dev.off()
lag.log.S.P.Large.cap = c(NA, log.S.P.Large.cap[1:(length(log.S.P.Large.cap)-1)])
sandpindices.lm2 = lm(log.S.P.Large.cap ~ lag.log.S.P.Large.cap)
summary(sandpindices.lm2)
resid.acf = acf(resid(sandpindices.lm2))
resid.acf$acf[1] = NA
rdiag = ls.diag(sandpindices.lm2)
#
# Figure 5.12
#
par(mfrow=c(2,2))
plot(Year[2:1259], rdiag$std.res, xlab="Year", ylab="Standardized residuals", main="(a)", pch = 19)
plot(resid.acf, main="(b)")
qqnorm(rdiag$std.res, main="(c)", pch = 19)
dev.off()
S.P.Large.cap.return = log.S.P.Large.cap-lag.log.S.P.Large.cap
S.P.Mid.cap.return = log.S.P.Mid.cap-c(NA,log.S.P.Mid.cap[1:(length(log.S.P.Mid.cap)-1)])
S.P.Small.cap.return = log.S.P.Small.cap-c(NA,log.S.P.Small.cap[1:(length(log.S.P.Small.cap)-1)])
sandpindices.lm3 = lm(S.P.Large.cap.return ~ S.P.Small.cap.return + S.P.Mid.cap.return)
summary(sandpindices.lm3)
dwtest(sandpindices.lm3)
resid.acf = acf(resid(sandpindices.lm3))
resid.acf$acf[1] = NA
rdiag = ls.diag(sandpindices.lm3)
#
# Figure 5.13
#
par(mfrow=c(3,2))
plot(fitted(sandpindices.lm3), rdiag$std.res, xlab="Fitted values", ylab="Standardized residuals", 
  main="(a)", pch = 19)
plot(Year[-1], rdiag$std.res, xlab="Year", ylab="Standardized residuals", main="(b)", pch = 19)
qqnorm(rdiag$std.res, main="(c)", pch = 19)
plot(resid.acf, main="(d)")
plot(S.P.Small.cap.return[-1], rdiag$std.res, xlab="Small-cap log return", ylab="Standardized residuals", main="(e)", pch = 19)
plot(S.P.Mid.cap.return[-1], rdiag$std.res, xlab="Mid-cap log return", ylab="Standardized residuals", main="(f)", pch = 19)
dev.off()
runs.test(factor(sign(resid(sandpindices.lm3))))
oldfaithful = read.csv("oldfaithful.csv")
#
# Figure 5.14
#
plot(oldfaithful$Duration, oldfaithful$Interval, xlab="Duration of previous eruption", 
  ylab="Time to next eruption", pch = 19)
dev.off()
oldfaithful.lm1 = lm(Interval ~ Duration, data=oldfaithful)
summary(oldfaithful.lm1)
dwtest(oldfaithful.lm1, alternative="two.sided")
resid.acf <- acf(resid(oldfaithful.lm1))
resid.acf$acf[1] = NA
resid.acf
rdiag = ls.diag(oldfaithful.lm1)
#
# Figure 5.15
#
par(mfrow=c(2,2))
plot(fitted(oldfaithful.lm1), rdiag$std.res, xlab="Fitted values", ylab="Standardized residuals", 
  main="(a)", pch = 19)
plot(c(1:222), rdiag$std.res, xlab="Eruption number", ylab="Standardized residuals", main="(b)", pch = 19)
plot(resid.acf, main="(c)")
qqnorm(rdiag$std.res, main="(d)", pch = 19)
dev.off()
rho = resid.acf$acf[2]
Durationstar = oldfaithful$Duration - rho*c(NA, oldfaithful$Duration[1:(nrow(oldfaithful)-1)])
Intervalstar = oldfaithful$Interval - rho*c(NA, oldfaithful$Interval[1:(nrow(oldfaithful)-1)])
oldfaithful.lm2 = lm(Intervalstar ~ Durationstar)
summary(oldfaithful.lm2)
dwtest(oldfaithful.lm2, alternative="two.sided")
resid.acf <- acf(resid(oldfaithful.lm2))
resid.acf$acf[1] = NA
rdiag = ls.diag(oldfaithful.lm2)
#
# Figure 5.16
#
par(mfrow=c(2,2))
plot(fitted(oldfaithful.lm2), rdiag$std.res, xlab="Fitted values", ylab="Standardized residuals", 
  main="(a)", pch = 19)
plot(c(2:222), rdiag$std.res, xlab="Eruption number", ylab="Standardized residuals", main="(b)", pch = 19)
plot(resid.acf, main="(c)")
qqnorm(rdiag$std.res, main="(d)", pch = 19)
dev.off()
runs.test(factor(sign(resid(oldfaithful.lm2))))
rdiag$hat
c(max(rdiag$hat),2.5*2/221)
rdiag$cooks
max(rdiag$cooks)
#
# Chapter 6
#
# Section 6.2
# 
# Figure 6.1
#
intplot1 = data.frame(Rows=rep(c("Row 1","Row 2","Row 3"),3),Columns=rep(c("Col. 1","Col. 2",
  "Col. 3"),each=3),Response=c(20,40,30,30,50,40,40,60,50))
intplot2 = data.frame(Rows=rep(c("Row 1","Row 2","Row 3"),3),Columns=rep(c("Col. 1","Col. 2",
  "Col. 3"),each=3),Response=c(20,40,30,20,10,0,50,40,60))
par(mfrow=c(2,1))
interaction.plot(intplot1$Rows, intplot1$Columns, intplot1$Response, xlab="Rows", ylab="Mean response",
  trace.label="Columns", fixed=T, lty=c(1,2,4), main="Without an interaction effect")
interaction.plot(intplot2$Rows, intplot2$Columns, intplot2$Response, xlab="Rows", ylab="Mean response",
  trace.label="Columns", fixed=T, lty=c(1,2,4), main="With an interaction effect")
dev.off()
#
# Section 6.4
#
Log.dvd = log10(movies2009$DVD)
movies2009 = data.frame(movies2009, Log.dvd)
movies2009.subset = movies2009[movies2009$Rating!="G" & movies2009$Genre!="Documentary" & 
  movies2009$Genre!="Musical",]
movies2009.subset = na.omit(data.frame(Rating=movies2009.subset$Rating, Genre=movies2009.subset$Genre, 
  Log.dvd=movies2009.subset$Log.dvd))
library(car)
movies2009.subset$Genre = recode(movies2009.subset$Genre,"c('Action','Adventure')='Action/Adventure'; 
  c('Horror','Romance','Thriller')='Drama'") 
levels(movies2009.subset$Rating) = c(NA,"PG","PG-13","R")
#
# Figure 6.2
#
par(mfrow=c(1,2))
boxplot(movies2009.subset$Log.dvd~movies2009.subset$Rating, xlab="MPAA rating", 
  ylab="Logged DVD sales")
boxplot(movies2009.subset$Log.dvd~movies2009.subset$Genre, xlab="Genre",
  ylab="Logged DVD sales", xaxt="n")
axis(1, at=c(1:3), labels=c("Action/Adventure", "Comedy", "Drama"), cex.axis=.6)
dev.off()
contrasts(movies2009.subset$Rating) = contr.sum(3)
contrasts(movies2009.subset$Genre) = contr.sum(3)
dvd.aov1 = aov(Log.dvd ~ Rating + Genre + Rating*Genre, data=movies2009.subset)
Anova(dvd.aov1, type="III")
dvd.lm1 = lm(Log.dvd ~ Rating + Genre + Rating*Genre, data=movies2009.subset)
summary(dvd.lm1)
sresdvd1 = rstandard(dvd.lm1)
#
# Figure 6.3
#
par(mfrow=c(1,2))
boxplot(sresdvd1~movies2009.subset$Rating, xlab="MPAA rating", 
  ylab="Standardized residuals")
boxplot(sresdvd1~movies2009.subset$Genre, xlab="Genre",
  ylab="Standardized residuals", xaxt="n")
axis(1, at=c(1:3), labels=c("Action/Adventure", "Comedy", "Drama"), cex.axis=.6)
dev.off()
Abs.resid = abs(sresdvd1)
dvd.levene1 = aov(Abs.resid ~ Rating + Genre, data=movies2009.subset)
Anova(dvd.levene1, type="III")
genrevar = by(sresdvd1, movies2009.subset$Genre, var)
wt = recode(movies2009.subset$Genre, "'Action/Adventure'=1/genrevar[1]; 'Comedy'=1/genrevar[2];
  'Drama'=1/genrevar[3]", as.factor.result=F)
dvd.aov1.wls = aov(Log.dvd ~ Rating + Genre + Rating*Genre, weights=wt, data=movies2009.subset)
Anova(dvd.aov1.wls, type="III")
dvd.lm1.wls = lm(Log.dvd ~ Rating + Genre + Rating*Genre, weights=wt, data=movies2009.subset)
summary(dvd.lm1.wls)
sresdvd1.wls = rstandard(dvd.lm1.wls)
#
# Figure 6.4
#
par(mfrow=c(1,2))
boxplot(sresdvd1.wls~movies2009.subset$Rating, xlab="MPAA rating", 
  ylab="Standardized residuals")
boxplot(sresdvd1.wls~movies2009.subset$Genre, xlab="Genre",
  ylab="Standardized residuals", xaxt="n")
axis(1, at=c(1:3), labels=c("Action/Adventure", "Comedy", "Drama"), cex.axis=.6)
dev.off()
Abs.resid = abs(sresdvd1.wls)
dvd.levene1.wls = aov(Abs.resid ~ Rating + Genre, data=movies2009.subset)
Anova(dvd.levene1.wls, type="III")
#
# Figure 6.5
#
interaction.plot(movies2009.subset$Rating, movies2009.subset$Genre, movies2009.subset$Log.dvd, 
  xlab="MPAA Rating", ylab="Mean logged sales", trace.label="Genre", fixed=T, lty=c(1,2,4), xpd=F)
dev.off()
dvd.aov2.wls = aov(Log.dvd ~ Rating + Genre, weights=wt, data=movies2009.subset)
Anova(dvd.aov2.wls)
dvd.lm2.wls = lm(Log.dvd ~ Rating + Genre, weights=wt, data=movies2009.subset)
summary(dvd.lm2.wls)
dvd.aov3.wls = aov(Log.dvd ~ Genre, weights=wt, data=movies2009.subset)
Anova(dvd.aov3.wls)
dvd.lm3.wls = lm(Log.dvd ~ Genre, weights=wt, data=movies2009.subset)
summary(dvd.lm3.wls)
#
# http://cran.r-project.org/web/packages/multcomp/index.html
#
library(multcomp)
summary(glht(dvd.aov3.wls, linfct=mcp(Genre="Tukey")))  
sresdvd3.wls = rstandard(dvd.lm3.wls)
hatdvd3.wls = hatvalues(dvd.lm3.wls)
cookdvd3.wls = cooks.distance(dvd.lm3.wls)
#
# Figure 6.6
#
par(mfrow=c(3,2))
plot(fitted(dvd.lm3.wls), sresdvd3.wls, xlab="Fitted values", ylab="Std. residuals", 
  pch = 19)
qqnorm(sresdvd3.wls, pch = 19)
boxplot(sresdvd3.wls~movies2009.subset$Genre, xlab="Genre",
  ylab="Std. residuals", xaxt="n")
axis(1, at=c(1:3), labels=c("Action/Adventure", "Comedy", "Drama"), cex.axis=.6)
plot(c(1:118), sresdvd3.wls, ylim=c(-2.8,3), xlab="Index", ylab="Std. residuals", pch=19)
abline(h=2.5, lty=3)
abline(h=-2.5, lty=3)
plot(c(1:118), hatdvd3.wls, ylim=c(0, .065), xlab="Index", ylab="Leverage", pch=19)
abline(h=2.5*3/118, lty=3)
plot(c(1:118), cookdvd3.wls, xlab="Index", ylab="Cook's D", ylim=c(0, .08), pch=19)
dev.off()
Log.dvd = log10(movies2010$DVD)
movies2010 = data.frame(movies2010, Log.dvd)
movies2010$Genre = recode(movies2010$Genre,"c('Action','Adventure')='Action/Adventure'; 
  c('Horror','Romance','Thriller')='Drama'") 
wt2010 = recode(movies2010$Genre, "'Action/Adventure'=1/genrevar[1]; 'Comedy'=1/genrevar[2];
  'Drama'=1/genrevar[3]", as.factor.result=F)
dvdvalidate = predict(dvd.lm3.wls, movies2010, weights=wt2010, interval=c("p"))
data.frame(movies2010$Movie,dvdvalidate[,2],movies2010$Log.dvd,dvdvalidate[,3])
data.frame(movies2010$Movie,10^dvdvalidate[,2],movies2010$DVD,10^dvdvalidate[,3])
#
# Chapter 7
#
# Section 7.3
#
Log.international.gross = log10(movies2009$International)
movies2009 = data.frame(movies2009, Log.international.gross)
#
# Figure 7.1
#
par(mfrow=c(1,2))
plot(movies2009$Log.domestic.gross, movies2009$Log.international.gross, xlab="Logged domestic grosses",
  ylab="Logged international grosses", pch=19)
boxplot(movies2009$Log.international~movies2009$Rating, xlab="MPAA rating", 
  ylab="Logged international grosses")
dev.off()
contrasts(movies2009$Rating) = contr.sum(4)
intl.lm1 = lm(Log.international.gross ~ Log.domestic.gross + Rating, data=movies2009)
library(car)
Anova(intl.lm1, type="III")
summary(intl.lm1)
intl.aov1 = aov(Log.international.gross ~ Log.domestic.gross + Rating, data=movies2009)
library(multcomp)
summary(glht(intl.aov1, linfct=mcp(Rating="Tukey")))  
intl.lm2 = lm(Log.international.gross ~ Log.domestic.gross + Rating + Log.domestic.gross*Rating, data=movies2009)
Anova(intl.lm2, type="III")
summary(intl.lm2)
#
# Figure 7.2
#
par(mfrow=c(1,2))
plot(movies2009$Log.domestic.gross, movies2009$Log.international.gross, xlab="Logged domestic grosses",
  ylab="Logged international grosses", pch=".", main="Constant slope")
abline(a=coef(intl.lm1)[1]+coef(intl.lm1)[3], b=coef(intl.lm1)[2], lty=1)  
abline(a=coef(intl.lm1)[1]+coef(intl.lm1)[4], b=coef(intl.lm1)[2], lty=2)  
abline(a=coef(intl.lm1)[1]+coef(intl.lm1)[5], b=coef(intl.lm1)[2], lty=4)  
abline(a=coef(intl.lm1)[1]-coef(intl.lm1)[3]-coef(intl.lm1)[4]-coef(intl.lm1)[5], b=coef(intl.lm1)[2], lty=5)  
plot(movies2009$Log.domestic.gross, movies2009$Log.international.gross, xlab="Logged domestic grosses",
  ylab="Logged international grosses", pch=".", main="Varying slopes")
abline(a=coef(intl.lm2)[1]+coef(intl.lm2)[3], b=coef(intl.lm2)[2]+coef(intl.lm2)[6], lty=1)  
abline(a=coef(intl.lm2)[1]+coef(intl.lm2)[4], b=coef(intl.lm2)[2]+coef(intl.lm2)[7], lty=2)  
abline(a=coef(intl.lm2)[1]+coef(intl.lm2)[5], b=coef(intl.lm2)[2]+coef(intl.lm2)[8], lty=4)  
abline(a=coef(intl.lm2)[1]-coef(intl.lm2)[3]-coef(intl.lm2)[4]-coef(intl.lm2)[5], 
  b=coef(intl.lm2)[2]-coef(intl.lm2)[6]-coef(intl.lm2)[7]-coef(intl.lm2)[8], lty=5)  
dev.off()
#
# Figure 7.3
#
rdiag = ls.diag(intl.lm2)
par(mfrow=c(2,2))
plot(fitted(intl.lm2), rdiag$std.res, xlab="Fitted values", ylab="Standardized residuals", 
  main="(a)", pch = 19)
plot(movies2009$Log.domestic.gross[-c(119,120)], rdiag$std.res, xlab="Logged domestic grosses", 
  ylab="Standardized residuals", main="(b)", pch = 19)
boxplot(rdiag$std.res~movies2009$Rating[-c(119,120)], xlab="MPAA rating", ylab="Standardized residuals", main="(c)")
qqnorm(rdiag$std.res, main="(d)", pch = 19)
dev.off()
#
# Chapter 8
#
# Section 8.1
#
# Figure 8.1
#
x = c(-500:500)/10
prob1 = exp(x)/(1+exp(x))
prob2 = exp(1+.15*x)/(1+exp(1+.15*x))
prob3 = exp(1-.2*x)/(1+exp(1-.2*x))
prob4 = exp(2-.05*x)/(1+exp(2-.05*x))
plot(x, prob1, type="l", xaxt="n", xlab="X", ylab="Probability")
lines(x, prob2, lty=2)
lines(x, prob3, lty=3)
lines(x, prob4, lty=4)
dev.off()
#
# Section 8.3
#
# Figure 8.2
#
par(mfrow=c(1,2))
x = c(0:50)
prob1 = c(rep(.05,15), rep(.95,15), rep(.05,21))
prob2 = rep(.5,51)
plot(x, rbinom(51, 50, prob1)/50, xlab="X", ylab="Proportion of successes", ylim=c(0,1), pch = 19)
plot(x, rbinom(51, 50, prob2)/50, xlab="X", ylab="Proportion of successes", ylim=c(0,1), pch = 19)
dev.off()
#
# Section 8.4
#
# Figure 8.3
#
smoking = read.csv("smoking.csv")
prop = smoking$Survived/smoking$At.risk
logit = log(prop/(1-prop))
par(mfrow=c(1,2))
plot(smoking$Age.group[smoking$Smoker==0], prop[smoking$Smoker==0], xlab="Age", 
  ylab="Proportion survived", main="(a)")
points(smoking$Age.group[smoking$Smoker==1], prop[smoking$Smoker==1], pch=3)
legend(25, .3, c("Smoker", "Nonsmoker"), pch=c(3, 1), cex=.6)
plot(smoking$Age.group[smoking$Smoker==0], logit[smoking$Smoker==0], xlab="Age", 
  ylab="Empirical logit", main="(b)")
points(smoking$Age.group[smoking$Smoker==1], logit[smoking$Smoker==1], pch=3)
legend(25, .5, c("Smoker", "Nonsmoker"), pch=c(3, 1), cex=.6)
dev.off()
smoking1.logit = glm(Survived/At.risk ~ Age.group + Smoker, weights=At.risk, family=binomial, data=smoking)
summary(smoking1.logit)
lrtest1 = smoking1.logit$null.deviance - deviance(smoking1.logit)
c(lrtest1, 1-pchisq(lrtest1,2))
exp(coef(smoking1.logit)[2:3])
c(deviance(smoking1.logit), 1-pchisq(deviance(smoking1.logit),11))
pearres1 = residuals(smoking1.logit, type="pearson")
pearson = sum(pearres1^2)
c(pearson, 1-pchisq(pearson,11))
Age.group.sq = smoking$Age.group^2
smoking2.logit = glm(Survived/At.risk ~ Age.group + Age.group.sq + Smoker, weights=At.risk, family=binomial, 
  data=smoking)
summary(smoking2.logit)
c(deviance(smoking2.logit), 1-pchisq(deviance(smoking2.logit),10))
pearres2 = residuals(smoking2.logit, type="pearson")
pearson = sum(pearres2^2)
c(pearson, 1-pchisq(pearson,10))
c(deviance(smoking1.logit)-deviance(smoking2.logit), 
  1-pchisq(deviance(smoking1.logit)-deviance(smoking2.logit),1))
smoking3.logit = glm(Survived/At.risk ~ factor(Age.group) + Smoker, weights=At.risk, family=binomial, 
  data=smoking)
summary(smoking3.logit)
c(deviance(smoking3.logit), 1-pchisq(deviance(smoking3.logit),6))
pearres3 = residuals(smoking3.logit, type="pearson")
pearson = sum(pearres3^2)
c(pearson, 1-pchisq(pearson,6))
c(deviance(smoking1.logit)-deviance(smoking3.logit), 
  1-pchisq(deviance(smoking1.logit)-deviance(smoking3.logit),5))
c(deviance(smoking2.logit)-deviance(smoking3.logit), 
  1-pchisq(deviance(smoking2.logit)-deviance(smoking3.logit),4))
#
# Section 8.5
#
bankruptcy = read.csv("bankruptcy.csv")
#
# Figure 8.4
#
par(mfrow=c(3,2))
boxplot(split(bankruptcy$WC.TA,bankruptcy$Bankrupt), xlab="Bankrupt", 
  ylab="Working Capital / Total Assets")
boxplot(split(bankruptcy$RE.TA,bankruptcy$Bankrupt), xlab="Bankrupt", ylab="Retained Earnings / Total Assets")
boxplot(split(bankruptcy$EBIT.TA,bankruptcy$Bankrupt), xlab="Bankrupt", ylab="Earnings / Total Assets")
boxplot(split(bankruptcy$S.TA,bankruptcy$Bankrupt), xlab="Bankrupt", ylab="Sales / Total Assets")
boxplot(split(bankruptcy$BVE.BVL,bankruptcy$Bankrupt), xlab="Bankrupt", ylab="Equity / Liabilities")
dev.off()
bankrupt1.logit = glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial, data=bankruptcy)
summary(bankrupt1.logit)
#
# http://cran.r-project.org/web/packages/boot/index.html
#
library(boot)
bankrupt1.diag = glm.diag(bankrupt1.logit)
#
# Figure 8.5
#
plot(c(1:50), bankrupt1.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
dev.off()
bankruptcy.subset = bankruptcy[-1,]
bankrupt2.logit = glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial, 
  data=bankruptcy.subset)
summary(bankrupt2.logit)
#
# http://cran.r-project.org/web/packages/bestglm/index.html
#
library(bestglm)
bankrupt.best = bestglm(bankruptcy.subset[,-1], family=binomial, IC="AIC")
bankrupt.best$Subsets
#
somersD = function(probsuccesses, probfailures){
#
# Function to calculate Somers' D. This function requires the data to be given in the form of two vectors
# of estimated probabilities, one for individuals that were successes and one for individuals that were
# failures. Data that are in a form of {# successes, # trials} must be converted to 0/1 response form.
# The function takes advantage of the connection between Somers' D and the Wilcoxon-Mann-Whitney statistic.
#
ns = length(probsuccesses)
nf = length(probfailures)
wmw = as.numeric(wilcox.test(probsuccesses, probfailures, correct=F)$statistic)
list(D=2.*wmw/(ns*nf)-1)}
bankrupt3.logit = glm(Bankrupt ~ 1, family=binomial, data=bankruptcy.subset)
summary(bankrupt3.logit)
bankrupt4.logit = glm(Bankrupt ~ RE.TA, family=binomial, data=bankruptcy.subset)
summary(bankrupt4.logit)
#
# http://cran.r-project.org/web/packages/MKmisc/index.html
#
library(MKmisc)
HLgof.test(fitted(bankrupt4.logit), bankruptcy.subset$Bankrupt)
somersD(fitted(bankrupt4.logit)[bankruptcy.subset$Bankrupt==1], 
  fitted(bankrupt4.logit)[bankruptcy.subset$Bankrupt==0])
bankrupt5.logit = glm(Bankrupt ~ RE.TA + BVE.BVL, family=binomial, data=bankruptcy.subset)
summary(bankrupt5.logit)
HLgof.test(fitted(bankrupt5.logit), bankruptcy.subset$Bankrupt)
somersD(fitted(bankrupt5.logit)[bankruptcy.subset$Bankrupt==1], 
  fitted(bankrupt5.logit)[bankruptcy.subset$Bankrupt==0])
bankrupt6.logit = glm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, family=binomial, data=bankruptcy.subset)
summary(bankrupt6.logit)
HLgof.test(fitted(bankrupt6.logit), bankruptcy.subset$Bankrupt)
somersD(fitted(bankrupt6.logit)[bankruptcy.subset$Bankrupt==1], 
  fitted(bankrupt6.logit)[bankruptcy.subset$Bankrupt==0])
bankrupt7.logit = glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + BVE.BVL, family=binomial, data=bankruptcy.subset)
summary(bankrupt7.logit)
HLgof.test(fitted(bankrupt7.logit), bankruptcy.subset$Bankrupt)
#
# Figure 8.6
#
bankrupt6.diag = glm.diag(bankrupt6.logit)
par(mfrow=c(2,2))
plot(fitted(bankrupt6.logit), bankrupt6.diag$rp, xlab="Estimated probability", 
  ylab="Standardized residuals", pch=19)
plot(c(2:50), bankrupt6.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
plot(c(2:50), bankrupt6.diag$h, xlab="Index", ylab="Leverage", pch=19, type="b")
plot(c(2:50), bankrupt6.diag$cook, xlab="Index", ylab="Cook's D", pch=19, type="b")
dev.off()
table(bankruptcy.subset$Bankrupt, as.numeric(fitted(bankrupt6.logit) > .5))
#
# Chapter 9
#
# Section 9.2
#
# Figure 9.1
#
plot(c(10,10),c(-5,60),type="l",xlim=c(0,60),xaxt="n",
  yaxt="n",xlab="x",ylab=expression(y^"*"))
den <- dlogis(c(-1500:1500)/100,0,4)
lines(10+40*den,10+c(-1500:1500)/100)
lines(c(30,30),c(-5,60))
lines(30+40*den,30+c(-1500:1500)/100)
lines(c(45,45),c(-5,60))
lines(45+40*den,45+c(-1500:1500)/100)
lines(c(0,60),c(0,60))
axis(1,at=c(10,30,45),label=c(expression(x[1]),expression(x[2]),expression(x[3])))
axis(2,at=c(0,5,30,45),label=c(expression(alpha[1]),expression(alpha[2]),expression(alpha[3]),
  expression(alpha[4])))
x <- runif(100)*60
y <- x+rlogis(100)*4
points(x,y,pch=".")
abline(h=0,lty=2)
abline(h=5,lty=2)
abline(h=30,lty=2)
abline(h=45,lty=2)
dev.off()
#
# Section 9.4
#
bondrate = read.csv("bondrate.csv")
bondrate$Rating = ordered(bondrate$Rating, levels=c("(4) BBB","(3) A","(2) AA","(1) AAA"),
  labels=c("BBB","A","AA","AAA"))
Logged.population = log(bondrate$Population)
bondrate = data.frame(bondrate, Logged.population=Logged.population) 
#
# Figure 9.2
#
par(mfrow=c(2,2))
boxplot(split(bondrate$Logged.population,bondrate$Rating),xlab="Rating",ylab="Logged population")
boxplot(split(bondrate$Household.income,bondrate$Rating),xlab="Rating",ylab="Household income")
boxplot(split(bondrate$Nonprofits.in.top.10,bondrate$Rating),xlab="Rating",ylab="Nonprofits in top 10")
boxplot(split(bondrate$For.profits.in.top.10,bondrate$Rating),xlab="Rating",ylab="For profits in top 10")
dev.off()
#
# http://cran.r-project.org/web/packages/MASS/index.html
#
# http://cran.r-project.org/web/packages/nnet/index.html
#
library(MASS)
library(nnet)
bondnom0 = multinom(Rating ~ 1, data=bondrate, maxit=200)
bondnom1 = multinom(Rating ~ Logged.population+Household.income+Nonprofits.in.top.10+For.profits.in.top.10,
  data=bondrate, maxit=200)
bondnom1$AIC+2*(bondnom1$edf+1)*(bondnom1$edf+2)/(56-bondnom1$edf-2)
#
# Note: output in book for nominal logistic regression is based on Minitab; the standard errors given for 
# the R model fit given below are incorrect using nnet version 7.3.1 and R versions 2.12.2 and 2.15.2.
#
summary(bondnom1)
bondnom2 = multinom(Rating ~ Logged.population, data=bondrate, maxit=200)
bondnom2$AIC+2*(bondnom2$edf+1)*(bondnom2$edf+2)/(56-bondnom2$edf-2)
bondnom3 = multinom(Rating ~ Household.income, data=bondrate, maxit=200)
bondnom3$AIC+2*(bondnom3$edf+1)*(bondnom3$edf+2)/(56-bondnom3$edf-2)
bondnom4 = multinom(Rating ~ Nonprofits.in.top.10, data=bondrate, maxit=200)
bondnom4$AIC+2*(bondnom4$edf+1)*(bondnom4$edf+2)/(56-bondnom4$edf-2)
bondnom5 = multinom(Rating ~ For.profits.in.top.10, data=bondrate, maxit=200)
bondnom5$AIC+2*(bondnom5$edf+1)*(bondnom5$edf+2)/(56-bondnom5$edf-2)
bondnom6 = multinom(Rating ~ Logged.population+Household.income, data=bondrate, maxit=200)
bondnom6$AIC+2*(bondnom6$edf+1)*(bondnom6$edf+2)/(56-bondnom6$edf-2)
bondnom7 = multinom(Rating ~ Logged.population+Nonprofits.in.top.10, data=bondrate, maxit=200)
bondnom7$AIC+2*(bondnom7$edf+1)*(bondnom7$edf+2)/(56-bondnom7$edf-2)
summary(bondnom7)
bondnom8 = multinom(Rating ~ Logged.population+For.profits.in.top.10, data=bondrate, maxit=200)
bondnom8$AIC+2*(bondnom8$edf+1)*(bondnom8$edf+2)/(56-bondnom8$edf-2)
bondnom9 = multinom(Rating ~ Household.income+Nonprofits.in.top.10, data=bondrate, maxit=200)
bondnom9$AIC+2*(bondnom9$edf+1)*(bondnom9$edf+2)/(56-bondnom9$edf-2)
bondnom10 = multinom(Rating ~ Household.income+For.profits.in.top.10, data=bondrate, maxit=200)
bondnom10$AIC+2*(bondnom10$edf+1)*(bondnom10$edf+2)/(56-bondnom10$edf-2)
bondnom11 = multinom(Rating ~ Nonprofits.in.top.10+For.profits.in.top.10, data=bondrate, maxit=200)
bondnom11$AIC+2*(bondnom11$edf+1)*(bondnom11$edf+2)/(56-bondnom11$edf-2)
bondnom12 = multinom(Rating ~ Logged.population+Household.income+Nonprofits.in.top.10,
  data=bondrate, maxit=200)
bondnom12$AIC+2*(bondnom12$edf+1)*(bondnom12$edf+2)/(56-bondnom12$edf-2)
bondnom13 = multinom(Rating ~ Logged.population+Household.income+For.profits.in.top.10,
  data=bondrate, maxit=200)
bondnom13$AIC+2*(bondnom13$edf+1)*(bondnom13$edf+2)/(56-bondnom13$edf-2)
bondnom14 = multinom(Rating ~ Logged.population+Nonprofits.in.top.10+For.profits.in.top.10,
  data=bondrate, maxit=200)
bondnom14$AIC+2*(bondnom14$edf+1)*(bondnom14$edf+2)/(56-bondnom14$edf-2)
bondnom15 = multinom(Rating ~ Household.income+Nonprofits.in.top.10+For.profits.in.top.10,
  data=bondrate, maxit=200)
bondnom15$AIC+2*(bondnom15$edf+1)*(bondnom15$edf+2)/(56-bondnom15$edf-2)
c(deviance(bondnom4)-deviance(bondnom7), 1-pchisq(deviance(bondnom4)-deviance(bondnom7), 3))
c(deviance(bondnom0)-deviance(bondnom7), 1-pchisq(deviance(bondnom0)-deviance(bondnom7), 6))
#
# http://cran.r-project.org/web/packages/rms/index.html
#
library(rms)
bondord1 = lrm(Rating ~ Logged.population+Household.income+Nonprofits.in.top.10+For.profits.in.top.10,
  data=bondrate)
bondord1$deviance[2]+2*length(bondord1$coefficients)+2*(length(bondord1$coefficients))*
  (length(bondord1$coefficients)+2)/(56-length(bondord1$coefficients)-2)
bondord1
bondord2 = lrm(Rating ~ Logged.population, data=bondrate)
bondord2$deviance[2]+2*length(bondord2$coefficients)+2*(length(bondord2$coefficients))*
  (length(bondord2$coefficients)+2)/(56-length(bondord2$coefficients)-2)
bondord3 = lrm(Rating ~ Household.income, data=bondrate)
bondord3$deviance[2]+2*length(bondord3$coefficients)+2*(length(bondord3$coefficients))*
  (length(bondord3$coefficients)+2)/(56-length(bondord3$coefficients)-2)
bondord4 = lrm(Rating ~ Nonprofits.in.top.10, data=bondrate)
bondord4$deviance[2]+2*length(bondord4$coefficients)+2*(length(bondord4$coefficients))*
  (length(bondord4$coefficients)+2)/(56-length(bondord4$coefficients)-2)
bondord5 = lrm(Rating ~ For.profits.in.top.10, data=bondrate)
bondord5$deviance[2]+2*length(bondord5$coefficients)+2*(length(bondord5$coefficients))*
  (length(bondord5$coefficients)+2)/(56-length(bondord5$coefficients)-2)
bondord6 = lrm(Rating ~ Logged.population+Household.income, data=bondrate)
bondord6$deviance[2]+2*length(bondord6$coefficients)+2*(length(bondord6$coefficients))*
  (length(bondord6$coefficients)+2)/(56-length(bondord6$coefficients)-2)
bondord7 = lrm(Rating ~ Logged.population+Nonprofits.in.top.10, data=bondrate)
bondord7$deviance[2]+2*length(bondord7$coefficients)+2*(length(bondord7$coefficients))*
  (length(bondord7$coefficients)+2)/(56-length(bondord7$coefficients)-2)
bondord7
bondord8 = lrm(Rating ~ Logged.population+For.profits.in.top.10, data=bondrate)
bondord8$deviance[2]+2*length(bondord8$coefficients)+2*(length(bondord8$coefficients))*
  (length(bondord8$coefficients)+2)/(56-length(bondord8$coefficients)-2)
bondord9 = lrm(Rating ~ Household.income+Nonprofits.in.top.10, data=bondrate)
bondord9$deviance[2]+2*length(bondord9$coefficients)+2*(length(bondord9$coefficients))*
  (length(bondord9$coefficients)+2)/(56-length(bondord9$coefficients)-2)
bondord10 = lrm(Rating ~ Household.income+For.profits.in.top.10, data=bondrate)
bondord10$deviance[2]+2*length(bondord10$coefficients)+2*(length(bondord10$coefficients))*
  (length(bondord10$coefficients)+2)/(56-length(bondord10$coefficients)-2)
bondord11 = lrm(Rating ~ Nonprofits.in.top.10+For.profits.in.top.10, data=bondrate)
bondord11$deviance[2]+2*length(bondord11$coefficients)+2*(length(bondord11$coefficients))*
  (length(bondord11$coefficients)+2)/(56-length(bondord11$coefficients)-2)
bondord12 = lrm(Rating ~ Logged.population+Household.income+Nonprofits.in.top.10, data=bondrate)
bondord12$deviance[2]+2*length(bondord12$coefficients)+2*(length(bondord12$coefficients))*
  (length(bondord12$coefficients)+2)/(56-length(bondord12$coefficients)-2)
bondord13 = lrm(Rating ~ Logged.population+Household.income+For.profits.in.top.10, data=bondrate)
bondord13$deviance[2]+2*length(bondord13$coefficients)+2*(length(bondord13$coefficients))*
  (length(bondord13$coefficients)+2)/(56-length(bondord13$coefficients)-2)
bondord14 = lrm(Rating ~ Logged.population+Nonprofits.in.top.10+For.profits.in.top.10, data=bondrate)
bondord14$deviance[2]+2*length(bondord14$coefficients)+2*(length(bondord14$coefficients))*
  (length(bondord14$coefficients)+2)/(56-length(bondord14$coefficients)-2)
bondord15 = lrm(Rating ~ Household.income+Nonprofits.in.top.10+For.profits.in.top.10,
  data=bondrate)
bondord15$deviance[2]+2*length(bondord15$coefficients)+2*(length(bondord15$coefficients))*
  (length(bondord15$coefficients)+2)/(56-length(bondord15$coefficients)-2)
c(bondord7$deviance[1]-bondord7$deviance[2], 1-pchisq(bondord7$deviance[1]-bondord7$deviance[2], 2))
c(bondord7$deviance[2]-bondnom7$deviance, 1-pchisq(bondord7$deviance[2]-bondnom7$deviance, 4))
table(bondrate$Rating,predict(bondnom7))
predord7 = apply(predict(bondord7, type="fitted.ind"),1,which.max)
predord7 = ordered(predord7, levels=c(1:4), labels=c("BBB","A","AA","AAA"))
table(bondrate$Rating, predord7)
data.frame(bondrate$Rating,predict(bondord7, type="fitted.ind"))
Nonprofits.sq = bondrate$Nonprofits.in.top.10*bondrate$Nonprofits.in.top.10
bondord16 = lrm(Rating ~ Logged.population+Nonprofits.in.top.10+Nonprofits.sq, data=bondrate)
bondord16$deviance[2]+2*length(bondord16$coefficients)+2*(length(bondord16$coefficients))*
  (length(bondord16$coefficients)+2)/(56-length(bondord16$coefficients)-2)
bondord16
predord16 = apply(predict(bondord16, type="fitted.ind"),1,which.max)
predord16 = ordered(predord16, levels=c(1:4), labels=c("BBB","A","AA","AAA"))
table(bondrate$Rating, predord16)
#
# Chapter 10
#
# Section 10.5
#
floridashark = read.csv("floridashark.csv") 
#
# Figure 10.1
#
par(mfrow=c(1,2))
plot(floridashark$Year, floridashark$Attacks, xlab="Year", ylab="Attacks", pch=19)
plot(floridashark$Population, floridashark$Attacks, xlab="Population", ylab="Attacks", pch=19)
dev.off()
shark1.pois = glm(Attacks ~ Year + Population, family=poisson, data=floridashark)
summary(shark1.pois)
#
# Figure 10.2
#
plot(floridashark$Year, floridashark$Attacks/floridashark$Population, xlab="Year", ylab="Attack rate", pch=19)
dev.off()
shark2.pois = glm(Attacks ~ offset(log(Population))+Year, family=poisson, data=floridashark)
summary(shark2.pois)
shark3.pois = glm(Attacks ~ offset(log(Population))+Year+Population, family=poisson, data=floridashark)
summary(shark3.pois)
library(boot)
shark2.diag = glm.diag(shark2.pois)
#
# Figure 10.3
#
par(mfrow=c(2,2))
plot(fitted(shark2.pois), shark2.diag$rp, xlab="Estimated attacks", 
  ylab="Standardized residuals", pch=19)
plot(c(1:66), shark2.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
plot(c(1:66), shark2.diag$h, xlab="Index", ylab="Leverage", pch=19, type="b")
plot(c(1:66), shark2.diag$cook, xlab="Index", ylab="Cook's D", pch=19, type="b")
dev.off()
Post.911 = as.numeric(floridashark$Year >= 2002)
shark4.pois = glm(Attacks ~ offset(log(Population))+Year+Post.911+Post.911*Year, family=poisson, 
  data=floridashark)
summary(shark4.pois)
sum(residuals(shark4.pois, type = "pearson")^2)
c(deviance(shark2.pois)-deviance(shark4.pois), 1-pchisq(deviance(shark2.pois)-deviance(shark4.pois), 2))
shark4.diag = glm.diag(shark4.pois)
#
# Figure 10.4
#
par(mfrow=c(2,2))
plot(fitted(shark4.pois), shark4.diag$rp, xlab="Estimated attacks", 
  ylab="Standardized residuals", pch=19)
plot(c(1:66), shark4.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
plot(c(1:66), shark4.diag$h, xlab="Index", ylab="Leverage", pch=19, type="b")
plot(c(1:66), shark4.diag$cook, xlab="Index", ylab="Cook's D", pch=19, type="b")
dev.off()
library(MASS)
shark5.nb = glm.nb(Attacks ~ offset(log(Population))+Year+Post.911+Post.911*Year, data=floridashark)
summary(shark5.nb)
shark5.diag = glm.diag(shark5.nb)
#
# Figure 10.5
#
par(mfrow=c(2,2))
plot(fitted(shark5.nb), shark5.diag$rp, xlab="Estimated attacks", 
  ylab="Standardized residuals", pch=19)
plot(c(1:66), shark5.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
plot(c(1:66), shark5.diag$h, xlab="Index", ylab="Leverage", pch=19, type="b")
plot(c(1:66), shark5.diag$cook, xlab="Index", ylab="Cook's D", pch=19, type="b")
dev.off()
c(deviance(shark5.nb), 1-pchisq(deviance(shark5.nb), 62))
c(sum(residuals(shark5.nb, type = "pearson")^2), 1-pchisq(sum(residuals(shark5.nb, type = "pearson")^2), 62))
summary(lm(log(Attacks/Population) ~ Year+Post.911[c(11:27,29:66)]+Post.911[c(11:27,29:66)]*Year, 
  data=floridashark[c(11:27,29:66),]))
shark6.pois = glm(Fatalities ~ offset(log(Population))+Year+Attacks+Post.911+Post.911*Year, family=poisson,  
  data=floridashark)
summary(shark6.pois)
shark7.pois = glm(Fatalities ~ offset(log(Population))+Year+Attacks+Post.911, family=poisson, data=floridashark)
summary(shark7.pois)
shark8.pois = glm(Fatalities ~ offset(log(Population))+Year+Attacks, family=poisson, data=floridashark)
summary(shark8.pois)
shark8.diag = glm.diag(shark8.pois)
#
# Figure 10.6
#
par(mfrow=c(2,2))
plot(fitted(shark8.pois), shark8.diag$rp, xlab="Estimated fatalities", 
  ylab="Standardized residuals", pch=19)
plot(c(1:66), shark8.diag$rp, xlab="Index", ylab="Standardized residuals", pch=19, type="b")
plot(c(1:66), shark8.diag$h, xlab="Index", ylab="Leverage", pch=19, type="b")
plot(c(1:66), shark8.diag$cook, xlab="Index", ylab="Cook's D", pch=19, type="b")
dev.off()
shark9.pois = glm(Fatalities ~ offset(log(Population))+Year+Attacks, family=poisson, data=floridashark[-31,])
summary(shark9.pois)
#
# Section 10.7
#
Log.international.gross = log10(movies2010$International)
movies2010 = data.frame(movies2010, Log.international.gross)
rdiag = ls.diag(intl.lm2)
intl.wt = glm(rdiag$std.res^2 ~ Log.domestic.gross, family=poisson, data=movies2009[-c(119,120),]) 
intl.wt
wt.intl = 1/fitted(intl.wt)
intl.lmwls1 = lm(Log.international.gross ~ Log.domestic.gross + Rating + Log.domestic.gross*Rating, 
  weights=wt.intl, data=movies2009[-c(119,120),])
summary(intl.lmwls1)
intl.aovwls1 = aov(Log.international.gross ~ Log.domestic.gross + Rating + Log.domestic.gross*Rating, 
  weights=wt.intl, data=movies2009[-c(119,120),])
library(car)
Anova(intl.aovwls1, type="III")
intl.lmwls2 = lm(Log.international.gross ~ Log.domestic.gross + Rating, 
  weights=wt.intl, data=movies2009[-c(119,120),])
summary(intl.lmwls2)
intl.aovwls2 = aov(Log.international.gross ~ Log.domestic.gross + Rating, 
  weights=wt.intl, data=movies2009[-c(119,120),])
Anova(intl.aovwls2, type="III")
intl.lmwls3 = lm(Log.international.gross ~ Log.domestic.gross, weights=wt.intl, data=movies2009[-c(119,120),])
summary(intl.lmwls3)
par(mfrow=c(2,2))
plot(fitted(intl.lmwls3), rstudent(intl.lmwls3), xlab="Fitted values", ylab="Standardized residuals", 
  pch = 19)
qqnorm(rstudent(intl.lmwls3), pch = 19)
plot(c(1:127), hatvalues(intl.lmwls3), xlab="Index", ylab="Leverage", pch = 19)
plot(c(1:127), cooks.distance(intl.lmwls3), xlab="Index", ylab="Cook's D", pch = 19)
intl.lmwls4 = lm(Log.international.gross ~ Log.domestic.gross + Rating + Log.domestic.gross*Rating, 
  weights=wt.intl[-15], data=movies2009[-c(15,119,120),])
summary(intl.lmwls4)
intl.aovwls4 = aov(Log.international.gross ~ Log.domestic.gross + Rating + Log.domestic.gross*Rating, 
  weights=wt.intl[-15], data=movies2009[-c(15,119,120),])
Anova(intl.aovwls4, type="III")
AIC(intl.lmwls4)+2*(intl.lmwls4$rank+1)*(intl.lmwls4$rank+2)/(intl.lmwls4$df.residual-1)
intl.lmwls5 = lm(Log.international.gross ~ Log.domestic.gross + Rating, 
  weights=wt.intl[-15], data=movies2009[-c(15,119,120),])
summary(intl.lmwls5)
intl.aovwls5 = aov(Log.international.gross ~ Log.domestic.gross + Rating, 
  weights=wt.intl[-15], data=movies2009[-c(15,119,120),])
Anova(intl.aovwls5, type="III")
AIC(intl.lmwls5)+2*(intl.lmwls5$rank+1)*(intl.lmwls5$rank+2)/(intl.lmwls5$df.residual-1)
intl.lmwls6 = lm(Log.international.gross ~ Log.domestic.gross, weights=wt.intl[-15], 
  data=movies2009[-c(15,119,120),])
summary(intl.lmwls6)
AIC(intl.lmwls6)+2*(intl.lmwls6$rank+1)*(intl.lmwls6$rank+2)/(intl.lmwls6$df.residual-1)
#
# Figure 10.7
#
par(mfrow=c(2,2))
plot(fitted(intl.lmwls6), rstudent(intl.lmwls6), xlab="Fitted values", ylab="Standardized residuals", 
  pch = 19)
qqnorm(rstudent(intl.lmwls6), pch = 19)
plot(c(1:126), hatvalues(intl.lmwls6), xlab="Index", ylab="Leverage", pch=19)
plot(c(1:126), cooks.distance(intl.lmwls6), xlab="Index", ylab="Cook's D", pch=19)
dev.off()
10^predict(intl.lm2, movies2010, interval="prediction")
wt.2010 = 1/predict(intl.wt, movies2010, type="response")
10^predict(intl.lmwls6, movies2010, weights=wt.2010, interval="prediction")
#
# Chapter 11
#
# Section 11.4
#
# Figure 11.1
#
enzyme = read.csv("enzyme.csv")
puromycin = enzyme[enzyme$Treated==1,]
invconc = 1/puromycin$Concentration
invveloc = 1/puromycin$Velocity
puro.lm = lm(invveloc ~ invconc)
par(mfrow=c(3,1))
plot(invconc, invveloc, xlab="1 / Concentration", ylab="1 / Velocity", pch=19, 
  main="(a)")
abline(puro.lm)
theta1.lm = as.vector(1/coef(puro.lm)[1])
theta2.lm = as.vector(coef(puro.lm)[2]/coef(puro.lm)[1])
c(theta1.lm, theta2.lm)
plot(puromycin$Concentration, puromycin$Velocity, xlab="Concentration", ylab="Velocity", pch=19, 
  main="(b)")
concgrid = c(0:115)/100
michment = function(x, theta1, theta2){theta1*x/(theta2+x)}
lines(concgrid, michment(concgrid,theta1.lm,theta2.lm))
puro1.nls = nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm), data=puromycin)
puro2.nls = nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm), data=puromycin[-1,])
plot(puromycin$Concentration, puromycin$Velocity, xlab="Concentration", ylab="Velocity", pch=19, 
  main="(c)")
lines(concgrid, michment(concgrid,coef(puro1.nls)[1],coef(puro1.nls)[2]))
lines(concgrid, michment(concgrid,coef(puro2.nls)[1],coef(puro2.nls)[2]), lty=2)
dev.off()
summary(puro1.nls)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm/2, theta2=theta2.lm/2), data=puromycin)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm/10, theta2=theta2.lm/10), data=puromycin)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm/100, theta2=theta2.lm/100), data=puromycin)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm*2, theta2=theta2.lm*2), data=puromycin)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm*10, theta2=theta2.lm*10), data=puromycin)
nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm*100, theta2=theta2.lm*100), data=puromycin)
#
# Figure 11.2
#
par(mfrow=c(1,2))
plot(fitted(puro1.nls), resid(puro1.nls), xlab="Fitted values", ylab="Residuals", main="(a)", pch = 19)
qqnorm(resid(puro1.nls), main="(b)", pch = 19)
dev.off()
puro3.nls = nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm), data=enzyme[-1,])
puro4.nls = nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm), data=enzyme)
summary(puro4.nls)
puro5.nls = nls(Velocity~(theta1+theta3*Treated)*Concentration/((theta2+theta4*Treated)+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm, theta3=0.0, theta4=0.0), data=enzyme)
anova(puro4.nls, puro5.nls)
summary(puro5.nls)
puro6.nls = nls(Velocity~(theta1+theta3*Treated)*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm, theta3=0.0), data=enzyme)
summary(puro6.nls)
anova(puro4.nls, puro6.nls)
anova(puro6.nls, puro5.nls)
plot(fitted(puro6.nls), resid(puro6.nls), xlab="Fitted values", ylab="Residuals", pch = 19)
qqnorm(resid(puro6.nls), pch = 19)
#
# Figure 11.3
#
par(mfrow=c(1,2))
plot(puromycin$Concentration, puromycin$Velocity, xlab="Concentration", ylab="Velocity", pch=19, 
  main="Complete data")
points(enzyme$Concentration[enzyme$Treated==0], enzyme$Velocity[enzyme$Treated==0], pch="x")
lines(concgrid, michment(concgrid,coef(puro5.nls)[1],coef(puro5.nls)[2]))
lines(concgrid, michment(concgrid,coef(puro5.nls)[1]+coef(puro5.nls)[3],coef(puro5.nls)[2]+coef(puro5.nls)[4]))
lines(concgrid, michment(concgrid,coef(puro6.nls)[1],coef(puro6.nls)[2]),lty=2)
lines(concgrid, michment(concgrid,coef(puro6.nls)[1]+coef(puro6.nls)[3],coef(puro6.nls)[2]),lty=2)
puro7.nls = nls(Velocity~theta1*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm), data=enzyme[-c(1,13),])
summary(puro7.nls)
puro8.nls = nls(Velocity~(theta1+theta3*Treated)*Concentration/((theta2+theta4*Treated)+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm, theta3=0.0, theta4=0.0), data=enzyme[-c(1,13),])
anova(puro7.nls, puro8.nls)
summary(puro8.nls)
puro9.nls = nls(Velocity~(theta1+theta3*Treated)*Concentration/(theta2+Concentration), 
  start=list(theta1=theta1.lm, theta2=theta2.lm, theta3=0.0), data=enzyme[-c(1,13),])
summary(puro9.nls)
anova(puro9.nls, puro7.nls)
anova(puro9.nls, puro8.nls)
plot(puromycin$Concentration[-c(1,13)], puromycin$Velocity[-c(1,13)], xlab="Concentration", ylab="Velocity", 
  pch=19, main="Omitting outliers")
points(enzyme$Concentration[enzyme$Treated==0][-1], enzyme$Velocity[enzyme$Treated==0][-1], pch="x")
lines(concgrid, michment(concgrid,coef(puro8.nls)[1],coef(puro8.nls)[2]))
lines(concgrid, michment(concgrid,coef(puro8.nls)[1]+coef(puro8.nls)[3],coef(puro8.nls)[2]+coef(puro8.nls)[4]))
lines(concgrid, michment(concgrid,coef(puro9.nls)[1],coef(puro9.nls)[2]),lty=2)
lines(concgrid, michment(concgrid,coef(puro9.nls)[1]+coef(puro9.nls)[3],coef(puro9.nls)[2]),lty=2)
dev.off()
