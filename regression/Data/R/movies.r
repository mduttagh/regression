#
# Predicting total movie grosses after one week
#
movie13 <- read.csv("c:/class/ascii/movie13.csv")
#
attach(movie13)
hist(Opening.Gross)
hist(Total.Gross)
plot(Opening.Gross,Total.Gross)
Log.domestic <- log10(Total.Gross)
Log.1st.weekend <- log10(Opening.Gross)
plot(Log.1st.weekend,Log.domestic)
movie1.lm <- lm(Log.domestic ~ Log.1st.weekend)
summary(movie1.lm)
#
# This is a way to get the standardized residuals
#
stdres <- rstandard(movie1.lm)
plot(fitted(movie1.lm),stdres,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(stdres)
movie2.lm <- lm(Log.domestic ~ Log.1st.weekend + Rotten.Tomatoes.Audience.Score)
summary(movie2.lm)
stdres <- rstandard(movie2.lm)
plot(fitted(movie2.lm),stdres,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(stdres)
movie14 <- read.csv("c:/class/ascii/movie14.csv")
newmovie <- data.frame(Log.1st.weekend = log10(movie14$Opening.Gross))
pred14 <- predict(movie1.lm,newmovie,interval=c("prediction"))
data.frame(movie14$Name,10^pred14[,2],movie14$Total.Gross,10^pred14[,3])
plot(c(1:22),10^pred14[,2],type="b",pch=1,lty=1,ylim=c(0,500000000),xlab="Index",ylab="Total Gross")
lines(c(1:22),movie14$Total.Gross,type="b",pch=2,lty=2)
lines(c(1:22),10^pred14[,3],type="b",pch=3,lty=3)
