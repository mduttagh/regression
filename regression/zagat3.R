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
    # data manipulation - select, filter, group_by, summarise, arrange, join, mutate, select_if
    "broom",
    #tidy(lmfit)
    "corrgram",
    #corrgram
    "leaps",
    # regsubsets
    "gvlma",
    #global validation of linear model assumptions
    "bestglm" #best glm including logistic regression, 2 binary factor vars
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

# Relative Importance of Predictor Variables function
relweights <- function(fit, ...) {
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import), 1, drop = FALSE]
  dotchart(
    import$Weights,
    labels = row.names(import),
    xlab = "% of R-Square",
    pch = 19,
    main = "Relative Importance of Predictor Variables",
    sub = paste("Total R-Square=", round(rsquare, digits = 3)),
    ...
  )
  return(import)
}

# Multi-linear regression analysis function
mlreg <- function(df, fit, fr, num) {
  cat("\n\nMulti-linear regression analysis iteration#:", num, "\n")
  cat("\n\nDescribe \n")
  print(describe(df, IQR = TRUE))
  cat("\n\nStatistics Description \n")
  print(stat.desc(df, norm = TRUE, p = 0.95))
  cat("\n\nSelect Numeric Columns\n")
  dfn <- select_if(df, is.numeric)
  cat("\n\nShpiro-Wilk Normality Test \n")
  print(tidy(sapply(dfn, shapiro.test)))
  cat("\n\nScatter Plot \n")
  print(
    ggscatter(
      data = df,
      x = response,
      y = var1,
      add = "reg.line",
      add.params = list(color = "blue", fill = "lightgray"),
      conf.int = TRUE,
      cor.coef = TRUE,
      main = "Scatterplot"
    )
  )
  cat("\n\nScatter Plot Matrix \n")
  scatterplotMatrix(df, id.n = 2, main = "Scatter Plot Matrix")
  cat("\n\nBoxplot for Model \n")
  Boxplot(as.formula(fr), data = df, main = "Boxplot for Model")
  cat("\n\nCorrelation Diagram")
  chart.Correlation(dfn, histogram = TRUE, pch = 19)
  cat("\n\nCorrelation Diagrams \n")
  corrgram(
    df,
    order = TRUE,
    main = "Correlation Diagram",
    lower.panel = panel.shade,
    upper.panel = panel.cor,
    text.panel = panel.txt,
    diag.panel = panel.density
  )
  corrplot(cor(dfn),
           method = "number",
           main = "Correlation Plot",
           order = "hclust")
  print(ggcorr(dfn, label = TRUE,
               label_alpha = TRUE) + ggtitle("Correlation Matrix"))
  print(ggscatmat(dfn) + ggtitle("Correlation Matrix"))
  cat("\n\nSummary Regression Model \n")
  cat("\n\nFormula = ",eval(fit$call[[2]]),"\n")
  print(summary(fit))
  cat("\n\nTidy Regression Model \n")
  print(tidy(fit))
  if (is.null(var2))
    "No VIF"
  else {
    cat("\n\nVIF \n")
    print(vif(fit))
  }
  if (is.null(var2))
    "No VIF"
  else {
    cat("\n\nSQRT OF VIF > 2 Test \n")
    print(sqrt(vif(fit)) > 2)
  }
  cat("\n\nAutoPlot for Regression Model \n")
  print(autoplot(
    fit,
    which = 1:6,
    ncol = 2,
    label.size = 3
  ) + theme_minimal())
  
  cat("\n\nQQ Plot to Test the Normality\n")
  qqPlot(fit, id.n = 4, main = "QQ Plot to Test the Normality")
  cat("\n\nMax Likelyhood estimation of the power lambda if violates normality assumption\n")
  #print(summary(powerTransform(as.formula(fr), data = dfn)))
  cat("\n\nTransforming Variables if violates normality assumption\n")
  #print(boxTidwell(as.formula(fr), data = df))
  cat("\n\nDurbin Watson Test for Independence of Errors Test \n")
  print(durbinWatsonTest(fit))
  cat("\n\nCompnent plus Residual Plots for Linearity Assumption\n")
  crPlots(fit, id.n = 4, main = "Compnent plus Residual Plots for Linearity Assumption")
  cat("\n\nNon Constant Error Plot for Assesing Constant Variane Assumption\n")
  print(ncvTest(fit))
  cat("\n\nSpread Level Plot for Assesing Constant Variane Assumption\n")
  spreadLevelPlot(fit, id.n = 4, main = "Spread Level Plot for Assesing Constant Variane Assumption")
  cat("\n\nGlobal Validation of Linear Model Assumption\n")
  fitgvm <- gvlma(fit)
  print(fitgvm)
  cat("\n\nOutlier Test \n")
  out <- NULL
  out <- outlierTest(fit)
  print(out)
  outt <- NULL
  if (out$bonf.p <= 0.05)
    (outt <- as.numeric(names(out$rstudent)))
  else
    (outt <- NULL)
  print(df[outt, ])
  cat("\n\nAdded Value Plot to asses impact of influence observations \n")
  avPlots(fit,
          ask = FALSE,
          id.n = 4,
          main = "Added Value Plot to asses impact of influence observations")
  cat("\n\nInfluence Plot \n")
  infout <- influencePlot(fit)
  print(influencePlot(fit))
  cat("\n\nInfluence Plot RowNames \n")
  infoutrn <- rownames(infout)
  print(df[infoutrn, ])
  cat("\n\nConfidence Interval for the Model\n")
  print(confint(fit, levels = 0.95))
  if (is.null(var2))
    "No Regsubsets"
  else
    regss <- regsubsets(as.formula(fr), data = df, nbest = 4)
  if (is.null(var2))
    "No Summary Regss"
  else {
    cat("\n\nSummary Subsets Regression\n")
    print(summary(regss))
  }
  
  if (is.null(var2))
    "No Plor Regss adjr2"
  else {
    cat("\n\nBest four models for each subset size based upon Adjusted R-square\n")
    plot(regss, scale = "adjr2", main = "Best four models for each subset size based upon Adjusted R-square")
  }
  if (is.null(var2))
    "No Plot Reggs cp"
  else {
    cat("\n\nBest four models for each subset size based upon Mallows Cp\n")
    plot(regss, scale = "Cp", main = "Best four models for each subset size based upon Mallow Cp")
  }
  
  if (is.null(var2))
    "No COEF"
  else {
    cat("\n\nCoeficient of Regular Subsets\n")
    print(coef(regss, 1:regss$np))
  }
  
  if (is.null(var2))
    "No VCOV"
  else {
    cat("\n\nVCOV of Regular Subsets\n")
    print(vcov(regss, regss$np))
  }
  cat("\n\nVCOV of Regression Mode\n")
  print(vcov(fit))
  relweights(fit, col = "blue")
  fitaov <- aov(fit)
  cat("\n\nAnova Model \n")
  print(summary(fitaov))
  fitaov1 <- Anova(fitaov, type = "III")
  cat("\n\nAnova Model1 for unbalanced design \n")
  print(fitaov1)
  cat("\n\nInfluce Plot Outliers \n")
  print(df[infoutrn, ])
  cat("\n\nOutlierTest Outliers\n")
  print(df[outt, ])
  return(rownames(infout))
}

environment(relweights)
environment(mlreg)

cat("\n\nFirst Round\n")
df1 <- read.csv(file.choose())
response <- "Cost"
df11 <- df1 %>% select_if(colnames(df1)==response)
df12 <- df1 %>% select_if(!colnames(df1)==response)
df1 <- cbind(df11,df12)
var1 <- "Decor"
var2 <- "Food"
var3 <- "Service"
fr1 <- paste(response, "~", var1, "+", var2, "+", var3)
fit1 <- lm(fr1, data = df1)
out1 <- NULL
out1 <- mlreg(df = df1, fit = fit1, fr = fr1, 1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- df1 %>% filter(!row_number() %in% out1)
fr2 <- fr1
fit2 <- lm(fr2, data = df2)
out2 <- NULL
out2 <- mlreg(df = df2, fit = fit2, fr = fr2, 2)

cat("\n\nPrediction with Confidence Interval for the Model with new data \n")
dfnew <- data.frame(var1 = 1.5)
colnames(dfnew) <- var1
dfpred2 <- predict(fit2,
                   newdata = dfnew,
                   interval = 'confidence',
                   se.fit = TRUE)
dfpred2


cat("\n\nThird Round after Best Subset Model\n")
df3 <- df2
response <- "Cost"
var1 <- "Decor"
var2 <- "Service"
var3 <- NULL
fr3 <- paste(response, "~", var1, "+", var2)
fit3 <- lm(fr3, data = df3)
out3 <- NULL
out3 <- mlreg(df = df3, fit = fit3, fr = fr3, 3)


cat("\n\nComparing Models using anova\n")
anova(fit3, fit2)

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
