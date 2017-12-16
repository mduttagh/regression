#Load MukEnv2.R functions and variables for this script for HW2-Revised
# Linear Regression for Monthly Returns of IBM vs SP500
# Data: https://www.stock-analysis-on.net/NYSE/Company/International-Business-Machines-Corp/DCF/CAPM

cat("\n\nFirst Round\n")
old.par <- par()
ifelse (!exists("mlreg", mode = "function"),
        source("MukEnv2.R"),
        "MukEnv1.R already exists")
df1 <- read_csv(".\\Data\\Homework\\IBM-Rates-of-Return.csv", col_types = "ncnn")
df1$Date <- as.Date(df1$Date,format = "%d-%b-%y")
response <- "IBM.Return"
df0 <- df1
df1 <-
  as.data.frame(df0 %>% select_if(function(x) !all(is.na(x))) %>% na.omit())
rownames(df1) <- df1$Date
response <- "IBM.Return"
fit1 <- lm(IBM.Return ~ SP500.Return, data = df1)
vars1 <- colnames(model.frame(fit1, df1))
ts.stdres1 <- ts(data=rstandard(fit1))
out1 <- NULL
#options(error=recover)
#options(error=NULL)
#par(old.par)
out1 <- mlreg(df = df1[vars1],
              fit = fit1,
              1)
tsmlreg(df = df1[vars1],
        fit = fit1,
        num = 1, ts.stdres = ts.stdres1)
powerT(df = df1,
       fit = fit1,
       1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
out2 <- c("2014-10-31","2015-10-31","2016-03-31")
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
rownames(df2) <- df2$Date
# df2 <-
#   df2 %>% mutate(
#     NET.VALUE.log = log(NET.VALUE),
#     SALE.PRICE.log = log(SALE.PRICE),
#     #SALE.ASSESSMENT.log = log(SALE.ASSESSMENT),
#     ACREAGE.log = log(ACREAGE)
#   )# %>% na.omit()
fit2 <- lm(IBM.Return ~ SP500.Return, data = df2)
vars2 <- colnames(model.frame(fit2, df2))
df2 <-
  as.data.frame(df2 %>% dplyr::select(one_of(vars2), everything()))
rownames(df2) <- df2$Date
ts.stdres2 <- ts(data=rstandard(fit2))
out1 <- NULL
out2 <- NULL
par(old.par)
out2 <- mlreg(df = df2[vars2],
              fit = fit2,
              2)
tsmlreg(df = df2[vars2],
        fit = fit2,
        num = 2, ts.stdres = ts.stdres2)
powerT(df = df2[vars2],
       fit = fit2,
       2)


cat("\n\nComparing Models using Tidy\n")
inner_join(tidy(fit1),
           tidy(fit2),
           by = "term",
           suffix = c(".fit1", ".fit2"))
cat("\n\nComparing Models using screenreg\n")
screenreg(list(fit1, fit2),
          single.row = TRUE,
          include.fstatistic = TRUE)
cat("\n\np-values of models\n")
print(as.data.frame(cbind(
  "p-value",
  glance(fit1)$p.value,
  glance(fit2)$p.value
)))
cat("\n\nComparing Models using plotreg\n")
plotreg(list(fit1, fit2), single.row = TRUE)
cat("\n\nComparing Models using anova\n")
anova(fit1, fit2)

cat("\n\nPrediction with Confidence Interval for the Model with new data \n")
dfnew <- data.frame(0.03)
colnames(dfnew) <- vars2[2]
dfpred2 <- predict(fit2,
                   newdata = dfnew,
                   interval = 'confidence',
                   se.fit = TRUE)
dfpred2
dfpred2.p <- predict(fit2,
                     newdata = dfnew,
                     interval = 'predict',
                     se.fit = TRUE)
dfpred2.p



cat("\n\nThird Round after Best Subset Model\n")
df3 <- df2 %>% na.omit()
#df3 <- filter(df3, df3$IMPROVEMENT.VALUE < 200000)
#df3 <- filter(df3, df3$SALE.ASSESSMENT > 55000)
# df3 <-
#   df3 %>% mutate(
#     SALE.PRICE.sqrt = sqrt(SALE.PRICE),
#     SALE.ASSESSMENT.sqrt = sqrt(SALE.ASSESSMENT)
#     #LAND.VALUE.log = log(LAND.VALUE),
#     #ACREAGE.log = log(ACREAGE),
#     #IMPROVEMENT.VALUE.log = log(IMPROVEMENT.VALUE)
#     #NET.VALUE.log = log(NET.VALUE)
#    )# %>% na.omit()
out3 <- c("2011-04-01","2015-10-01","2016-01-01","2010-01-01","2016-10-01",
          "2014-10-01","2003-07-01")
df3 <- subset(df3, !(row.names(df3) %in% out3)) # For iterations
#rownames(df3) <- df3$Industry.Name
#df3 <- df3 #%>% filter(sign(df3$NET.VALUE.log) == 1)
rownames(df3) <- df3$OBS_DATE
fit3 <-
  lm(
    FEDRATE_PCH ~ RU3000_PCH + UNRATE_PCH + DGORDER_PCH, data = df3
  )
vars3 <- colnames(model.frame(fit3, df3))
df3 <-
  as.data.frame(df3 %>% dplyr::select(one_of(vars3), everything()))
rownames(df3) <- df3$OBS_DATE
out3 <- NULL
out3 <- mlreg(df = df3[vars3],
              fit = fit3,
              3)
powerT(df = df3[vars3],
       fit = fit3,
       3)


cat("\n\nComparing Models using screenreg\n")
screenreg(list(fit1, fit2, fit3),
          single.row = TRUE,
          include.fstatistic = TRUE)
cat("\n\np-values of models\n")
print(as.data.frame(cbind(
  "p-value",
  glance(fit1)$p.value,
  glance(fit2)$p.value,
  glance(fit3)$p.value
)))
cat("\n\nComparing Models using plotreg\n")
plotreg(list(fit1, fit2, fit3), single.row = TRUE)
cat("\n\nComparing Models using anova\n")
anova(fit2, fit3)

cat("\n\nPrediction with Confidence Interval for the Model with new data file\n")
dfnew <- read.csv(file.choose())
dfpred3 <- predict(fit3,
                   newdata = dfnew,
                   interval = 'confidence',
                   se.fit = TRUE)
dfpred3

cat("\n\nPrediction with Prediction Interval for the Model\n")
dfpred31 <- predict(fit3,
                    newdata = dfnew,
                    interval = 'prediction',
                    se.fit = TRUE)
dfpred31

cat("\n\nImpact of predictor variable on the probability of outcome\n")
fitglm <-
  glm(Cost ~ Decor + Service,
      family = poisson(link = "identity"),
      data = df3)
dfpred4 <- predict(fitglm,
                   newdata = dfnew,
                   interval = 'response',
                   se.fit = TRUE)
dfpred4