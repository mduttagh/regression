# Initialize Libraries
packages <-
  c(
    "GGally",
    "corrplot",
    #Corr Scatterplot
    "PerformanceAnalytics",
    #Corr Scatterplot
    "RColorBrewer",
    "car",
    #Corr scatterplot, scatterplotMatrix
    "ggfortify",
    # autoplot
    "ggpubr",
    "gmodels",
    # CrossTable()
    "lda",
    #"plyr",# Mapvalues
    "psych",
    # stats description
    "tidyverse",
    # gather, spread, separate, unite
    "pastecs",
    # stat.desc
    "dplyr",
    # data manipulation - select, filter, group_by, summarise, arrange, join, mutate
    "broom", #tidy(lmfit)
    "corrgram", #corrgram
    "leaps", # regsubsets
    "gvlma" #global validation of linear model assumptions
  )

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      require(x, character.only = TRUE)
    }
  }
)
zagat <- read.csv(file.choose())

describe(zagat, IQR = TRUE)
stat.desc(zagat, norm = TRUE, p = 0.95)
scatterplot(Cost ~ Service, data = zagat)
scatterplotMatrix(zagat)
scatterplotMatrix(~ Cost + ., data=zagat)
scatterplotMatrix(zagat,diagonal = "histogram")
chart.Correlation(zagat[4:7], histogram = TRUE, pch = 19)

ggscatmat(zagat[4:7],columns = c(1:ncol(zagat[4:7])))
ggcorr(zagat[4:7],label = TRUE,
       label_alpha = TRUE)
corrplot(cor(zagat[4:7]), method="number")
corrplot(cor(zagat[4:7]), method="pie")
corrgram(zagat,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,text.panel=panel.txt,diag.panel = panel.density)

shapiro.test(zagat$Food)
shapiro.test(zagat$Decor)
shapiro.test(zagat$Service)
shapiro.test(zagat$Cost)

ggscatter(
  data = zagat,
  x = "Food",
  y = "Cost",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Cost vs Food"
)

ggscatter(
  data = zagat,
  x = "Decor",
  y = "Cost",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Cost vs Food"
)
ggscatter(
  data = zagat,
  x = "Service",
  y = "Cost",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Cost vs Service"
)
zagatlm <- lm(Cost ~ Food + Decor + Service, data = zagat)
zagataov <- aov(zagatlm)
summary(zagatlm)
tidy(zagatlm)
augment(zagatlm)
glance(zagatlm)
summary(zagataov)

autoplot(zagatlm,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()
sprintf("Adj R2 = %0.4f",summary(zagatlm)$r.squared)


outlierTest(zagatlm)
zagat %>% filter(row_number() == 124)
zagata <- zagat %>% filter(!Restaurant.Name == "Peter Luger Steak House")
scatterplot(Cost ~ Service, data = zagata)
scatterplotMatrix(zagata)


require(leaps)
zagatlmb <- regsubsets(Cost ~ Food + Decor + Service, data=zagat,nbest=4)
summary(zagatlmb)
plot(zagatlmb, scale="adjr2")
coef(zagatlmb,1:7)
vcov(zagatlmb,7)

require(car)
subsets(zagatlmb,statistic = "cp")
abline(1,1,lty=2,col="red")

zagatlma <- lm(Cost ~ Decor + Service, data = zagata)
zagataova <- aov(zagatlma)
summary(zagatlma)
vif(zagatlma)
sqrt(vif(zagatlma)) > 2 #If >2 then problem
summary(zagataova)
autoplot(zagatlma,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()
qqPlot(zagatlma,labels=zagata$Restaurant.Name, simulate = TRUE, id.method="identify")

durbinWatsonTest(zagatlma)
crPlots(zagatlma)
ncvTest(zagatlma)
spreadLevelPlot(zagatlma)

require(gvlma)
gvmodel <- gvlma(zagatlma)
summary(gvmodel)

vif(zagatlma)
sqrt(vif(zagatlma)) > 2 # problem?
outlierTest(zagatlma)
avPlots(zagatlma,ask=FALSE,id.method="identify",labels=zagata$Restaurant.Name)
influencePlot(zagatlma,labels=zagata$Restaurant.Name, id.method = "identify",main="Influnce Plot")
confint(zagatlma, levels = 0.95)