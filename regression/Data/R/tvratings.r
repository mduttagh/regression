#
# Modeling television viewership
#
tvrating <- read.csv("c:/class/ascii/tvratings1112.csv")
attach(tvrating)
boxplot(split(HH.Rating,Network))
boxplot(split(HH.Rating,Type))
#
# Two-way ANOVA. Interaction effect is given by "*" notation
# R fits this model, despite the zero entries in the design table, in the
# way described in the Appendix of the handout. 
#
tvratinga <- lm(HH.Rating ~ Network + Full.type + Network*Full.type)
#
# Cross-classification of network by type counts
#
table(Network,Full.type)
#
# Drop out news shows, drop out CW, and list evening animation shows as comedies
#
tvrating2 <- tvrating[((tvrating$Type!="News")&(tvrating$Network!="CW  ")),]
attach(tvrating2)
tvratingb <- aov(HH.Rating ~ Network + Type + Network*Type)
#
# To get Type III sums of squares in R, use the "car" package and the Anova function. Note that the F-tests for
# the main effects are different from those reported in Minitab in the presence of the interaction, but since
# they are not interpretable this does not affect analysis of the data.
#
library(car)
Anova(tvratingb, type=3)
# 
# Note: the drop1() command can also be used to get the partial F-test for the interaction term, and it has an
# important advantage; it will work in the situation where you have "holes" in your design (cells with no
# observations), while the Anova() command from the car package will not work in that situation. So, for example,
# the original model fit above (tvratinga) could be used to test the interaction for a model that includes CW and news
# shows, using the command
#
#                  drop1(tvratinga, test="F")
#
# To get R^2, s, need to fit using lm
#
summary(lm(HH.Rating ~ Network + Type + Network*Type))
#
# This gives the least squares means. It is not clear that the numbers given in R are correct in 
# general; in particular, it will not work for weighted analyses. 
#
model.tables(tvratingb,"mean")
#
std.resb <- rstandard(tvratingb)
hatb <- hatvalues(tvratingb)
cooksb <- cooks.distance(tvratingb)
plot(fitted(tvratingb),std.resb,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resb, ylab="Standardized residuals")
cbind(std.resb,hatb,cooksb)
lograting <- log10(HH.Rating)
boxplot(split(lograting,Network))
boxplot(split(lograting,Type))
tvratingc <- aov(lograting ~ Network + Type + Network*Type)
Anova(tvratingc, type=3)
summary(lm(lograting ~ Network + Type + Network*Type))
std.resc <- rstandard(tvratingc)
hatc <- hatvalues(tvratingc)
cooksc <- cooks.distance(tvratingc)
plot(fitted(tvratingc),std.resc,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resc, ylab="Standardized residuals")
cbind(std.resc,hatc,cooksc)
#
# Omit outlier shows
#
tvrating3 <- tvrating2[-c(131:134),]
attach(tvrating3)
lograting <- lograting[-c(131:134)]
tvratingd <- aov(lograting ~ Network + Type +Network*Type)
Anova(tvratingd, type=3)
summary(lm(lograting ~ Network + Type +Network*Type))
model.tables(tvratingd, "mean")
#
# Interaction plot. The first line gets the new reduced levels for type correctly.
#
Type <- factor(Type)
interaction.plot(Type,Network,lograting)
std.resd <- rstandard(tvratingd)
hatd <- hatvalues(tvratingd)
cooksd <- cooks.distance(tvratingd)
plot(fitted(tvratingd),std.resd,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resd, ylab="Standardized residuals")
cbind(std.resd,hatd,cooksd)
boxplot(split(std.resd,Network))
boxplot(split(std.resd,Type))
#
# Levene's test
#
levened <- aov(abs(std.resd) ~ Network + Type + Network*Type)
Anova(levened, type=3)
#
# WLS
#
# Getting weights. They are based on two categorical predictors, so it is more complicated. We go back to the data that includes
# the outliers (tvrating2), since they might not be outliers any more. At this point the attaching of new versions of the data
# makes it difficult for R (and us) to keep track of which version of different variables are bing used, so I will refer to the
# effects by their full "names", including the name of the underlying data frame.
#
groupsd <- sapply(split(std.resd,list(tvrating2$Type,tvrating2$Network)),sd)
wt<-rep(NA,dim(tvrating3)[1])
wt[(tvrating2$Type=="Comedy")&(tvrating2$Network=="ABC ")] <- 1 / groupsd["Comedy.ABC "]^2
wt[(tvrating2$Type=="Comedy")&(tvrating2$Network=="NBC ")] <- 1 / groupsd["Comedy.NBC "]^2
wt[(tvrating2$Type=="Comedy")&(tvrating2$Network=="CBS ")] <- 1 / groupsd["Comedy.CBS "]^2
wt[(tvrating2$Type=="Comedy")&(tvrating2$Network=="FOX ")] <- 1 / groupsd["Comedy.FOX "]^2
wt[(tvrating2$Type=="Drama")&(tvrating2$Network=="ABC ")] <- 1 / groupsd["Drama.ABC "]^2
wt[(tvrating2$Type=="Drama")&(tvrating2$Network=="NBC ")] <- 1 / groupsd["Drama.NBC "]^2
wt[(tvrating2$Type=="Drama")&(tvrating2$Network=="CBS ")] <- 1 / groupsd["Drama.CBS "]^2
wt[(tvrating2$Type=="Drama")&(tvrating2$Network=="FOX ")] <- 1 / groupsd["Drama.FOX "]^2
wt[(tvrating2$Type=="Reality/Participatio")&(tvrating2$Network=="ABC ")] <- 1 / groupsd["Reality/Participatio.ABC "]^2
wt[(tvrating2$Type=="Reality/Participatio")&(tvrating2$Network=="NBC ")] <- 1 / groupsd["Reality/Participatio.NBC "]^2
wt[(tvrating2$Type=="Reality/Participatio")&(tvrating2$Network=="CBS ")] <- 1 / groupsd["Reality/Participatio.CBS "]^2
wt[(tvrating2$Type=="Reality/Participatio")&(tvrating2$Network=="FOX ")] <- 1 / groupsd["Reality/Participatio.FOX "]^2
lograting <- log10(tvrating2$HH.Rating)
tvratinge <- aov(lograting ~ tvrating2$Network + tvrating2$Type + tvrating2$Network*tvrating2$Type, weight=wt)
Anova(tvratinge, type=3)
summary(lm(lograting ~ tvrating2$Network + tvrating2$Type + tvrating2$Network*tvrating2$Type, weights=wt))
std.rese <- rstandard(tvratinge)
hate <- hatvalues(tvratinge)
cookse <- cooks.distance(tvratinge)
plot(fitted(tvratinge),std.rese,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.rese, ylab="Standardized residuals")
cbind(std.rese,hate,cookse)
#
# At this point the analysis proceeds as above, omitting the outliers and continuing to use WLS
#
#
# If you have a weighted analysis with two main effects and no interaction effect (which would be based on all of the data)
# you need to get the least squares means from the underlying effect codings linear regression coefficients.
# This is done as follows by creating the effect codings using the contr.sum(K) command, where K is the
# number of levels of the factor. Then those are used in the linear regression fitting.
# The least squares mean for row i is the sum of the intercept and the slope for the coding for row i,
# with a corresponding definition for column j, which are obtained from the summary command below.
# Note that the lm() command isn't quite right here, since the wt vector is based on the data set that omits the
# news and CW shows; a correct weight vector, which I've called wt.full, has been hypothetically constructed here 
#
tvratingnoint <- lm(log10(HH.Rating) ~ Network + Type, data=tvrating, weight=wt.full)
contrasts(tvrating$Network) = contr.sum(5)
contrasts(tvrating$Type) = contr.sum(4)
summary(tvratingnoint)
Anova(tvratingnoint, type=3)
#
# If you were able to do multiple comparisons (because the interaction was not necessary and was removed from the model,
# you would use the multcomp package. Say the model fit was in the object tvratingnoint.
#
library(multcomp)
tvnoint.network.tukey <- glht(tvratingnoint, linfct=mcp(Network="Tukey"))
summary(tvnoint.network.tukey)
print(confint(tvnoint.network.tukey))
plot(print(confint(tvnoint.network.tukey)))
tvnoint.type.tukey <- glht(tvratingnoint, linfct=mcp(Type="Tukey"))
summary(tvnoint.type.tukey)
print(confint(tvnoint.type.tukey))
plot(print(confint(tvnoint.type.tukey)))
#
# As is true for any regression (lm) model, predictions are made using the predict() command. Put the predictor combinations
# you are interested in in a new data frame with the same variable names, put associated weights in a weight variable if doing WLS,
# and then (for predictions based on tvratinge, for example)
#
#       predict(tvratinge, data.new, interval="prediction", weights=data.new$wt)
#
