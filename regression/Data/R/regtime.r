#
# Ordinary least squares estimation and time series data
#
# This code corresponds to a hypothetical simple regression with target variable y and predictor x, with time variable time
#
plot(x,y)
olstime1 <- lm(y ~ x)
summary(olstime1)
std.res1 <- rstandard(olstime1)
plot(time,std.res1,ylab="Standardized residuals")
#
# Durbin-Watson test
# First, load the lmtest library (it can be obtained from the CRAN library).
# Note that this function provides a p-value for the test as well.
#
library(lmtest)
dwtest(y ~ x)
#
# Note that R has the annoying property that the ACF plot includes
# the correlation for lag zero, which by definition must equal one. This can be avoided by 
# omitting the first displayed lag in the plot. The plot below plots the first 20 lags.
#
acf(std.res1, xlim=c(1,20), ylim=c(-.25, .8))
n <- length(y)
#
# The approximate standard error of the estimated autocorrelations is 1/sqrt(n),so
# these are approximate z-statistics (they're not the same as those in Minitab, but
# they're similar).
#
acf(std.res1,plot=F)$acf[2:21]*sqrt(n)
#
# The following is a runs test function. It was written by Adrian Trapletti as part of
# the R tseries library. The mean and standard deviation for the number of runs given
# in the code doesn't match what I have, so I've changed that part of the code. Large
# sample approximations to the mean and variance are used. The code now also gives as
# output the observed number of runs and the expected number of runs. Missing
# values are not allowed. A continuity correction can be applied to the test, but is not by default.
# Adrian Trapletti holds the copyright for the previously written portion of the code, but has
# allowed distribution of it.
#
runs.test <- function (x, cont.corr = F)
{
    if(any(is.na(x)))
        stop("NAs in x")
    DNAME <- deparse(substitute(x))
    if(any(x == 0.0)) {
        cat("Removed", length(x[x==0.0]), "zero(es)\n")
        x <- x[x != 0.0]
    }
    d <- diff(sign(x))
    f <- factor(d)
    sp <- split(d, f)
    resL <- lapply(sp, length)
    n <- length(x)
    nplus <- sum(sign(x)==1)
    nminus <- sum(sign(x)==-1)
    m <- 2.*nplus*nminus/n+1
    s <- sqrt(2.*nplus*nminus*(2.*nplus*nminus-n)/(n*n*(n-1)))
    R <- 1
    if(!is.null(resL$"-2"))
        R <- R+resL$"-2"
    if(!is.null(resL$"2"))
        R <- R+resL$"2"
# Continuity correction is not used by default
    if (cont.corr) STATISTIC <- sign(R-m)*(abs(R-m)-.5)/s else STATISTIC <- (R-m)/s
    METHOD <- "Runs Test"
    PVAL <- 2 * pnorm(-abs(STATISTIC))
    names(STATISTIC) <- "z-statistic for runs test"
    runstuff <- c(R,m)
    names(runstuff) <- c("Observed number of runs", "Expected number of runs")
    structure(list(estimate = runstuff,
    		   statistic = STATISTIC,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
}
runs.test(std.res1, cont.corr=T)
#
# Lagging the target variable
#
y.lag <- c(NA,y[1:(n-1)])
olstime2 <- lm(y~y.lag,na.action=na.exclude)
summary(olstime2)
std.res2 <- rstandard(olstime2)
plot(c(1:(n-1)),std.res2[2:n],xlab="Observation Order",ylab="Standardized residuals")
#
# The acf function won't work in R if there are missing values; using na.omit removes the problem
#
acf(na.omit(std.res2), xlim=c(1,20), ylim=c(-.25, .25))
acf(na.omit(std.res2),plot=F)$acf[2:20]*sqrt(n-1)
runs.test(na.omit(std.res2), cont.corr=T)
#
# Differencing the target variable
#
y.diff <- y-y.lag
summary(y.diff)
sd(y.diff,na.rm=T)
#
# Cochrane-Orcutt procedure
#
rhohat <- acf(olstime1.diag$std.res,plot=F)$acf[2]
ystar <- y - rhohat*(c(NA,y[1:(n-1)]))
xstar <- x - rhohat*(c(NA,x[1:(n-1)]))
olstime3 <- lm(ystar ~ xstar,na.action=na.exclude)
summary(olstime3)
dwtest(ystar ~ xstar)
std.res3 <- rstandard(olstime3)
plot(c(1:(n-1)),std.res3[2:n],xlab="Observation Order",ylab="Standardized residuals")
acf(na.omit(std.res3), xlim=c(1,19), ylim=c(-.25, .25))
acf(na.omit(std.res3),plot=F)$acf[2:20]*sqrt(n-1)
runs.test(na.omit(std.res3), cont.corr=T)
