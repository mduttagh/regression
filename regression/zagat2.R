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

# Multi-linear regression analysis functions
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
  ggscatter(
    data = dfn,
    x = colnames(dfn[1]),
    y = colnames(dfn[2]),
    color = "black",
    shape = 21,
    size = 3,
    add = "reg.line",
    add.params = list(color = "blue", fill = "lightgray"),
    conf.int = TRUE,
    cor.coef = TRUE,
    main = "Scatterplot"
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
  ggcorr(dfn, label = TRUE,
         label_alpha = TRUE) + ggtitle("Correlation Matrix")
  ggscatmat(dfn) + ggtitle("Correlation Matrix")
    cat("\n\nSummary Regression Model \n")
  print(summary(fit))
  cat("\n\nTidy Regression Model \n")
  print(tidy(fit))
  cat("\n\nVIF \n")
  print(vif(fit))
  cat("\n\nSQRT OF VIF > 2 Test \n")
  print(sqrt(vif(fit)) > 2)
  cat("\n\nAutoPlot for Regression Model \n")
  autoplot(fit,
           which = 1:6,
           ncol = 2,
           label.size = 3) + theme_minimal()
  cat("\n\nQQ Plot to Test the Normality\n")
  qqPlot(fit, id.n = 4, main = "QQ Plot to Test the Normality")
  cat("\n\nMax Likelyhood estimation of the power lambda if violates normality assumption\n")
  print(summary(powerTransform(as.formula(fr), data = df)))
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
  print(outlierTest(fit))
  out <- as.numeric(names(outlierTest(fit)$rstudent))
  cat("\n\nAdded Value Plot to asses impact of influence observations \n")
  avPlots(fit,
          ask = FALSE,
          id.n = 4,
          main = "Added Value Plot to asses impact of influence observations")
  cat("\n\nInfluence Plot \n")
  influencePlot(fit)
  cat("\n\nConfidence Interval for the Model\n")
  print(confint(fit, levels = 0.95))
  regss <- regsubsets(as.formula(fr), data = df, nbest = 4)
  cat("\n\nSummary Subsets Regression\n")
  print(summary(regss))
  cat("\n\nBest four models for each subset size based upon Adjusted R-square\n")
  plot(regss, scale = "adjr2", main = "Best four models for each subset size based upon Adjusted R-square")
  cat("\n\nBest four models for each subset size based upon Mallows Cp\n")
  plot(regss, scale = "Cp", main = "Best four models for each subset size based upon Mallow Cp")
  cat("\n\nCoeficient of Regular Subsets\n")
  print(coef(regss, 1:length(df)))
  cat("\n\nVCOV of Regular Subsets\n")
  print(vcov(regss, length(df)))
  cat("\n\nVCOV of Regression Mode\n")
  print(vcov(fit))
  relweights(fit, col = "blue")
  fitaov <- aov(fit)
  cat("\n\nAnova Model \n")
  print(summary(fitaov))
  fitaov1 <- Anova(fitaov,type = "III")
  cat("\n\nAnova Model1 for unbalanced design \n")
  print(fitaov1)
  return(out)
}

cat("\n\nFirst Round\n")
df1 <- read.csv(file.choose())
fr1 <- "Cost ~ Decor + Food + Service"
fit1 <- lm(fr1, data = df1)
out <- NULL
out <- mlreg(df = df1, fit = fit1, fr = fr1, 1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- df1 %>% filter(!row_number() == out)
fr2 <- fr1
fit2 <- lm(fr2, data = df2)
out <- NULL
out <- mlreg(df = df2, fit = fit2, fr = fr2, 2)

cat("\n\nThird Round after Best Subset Model\n")
df3 <- df2
fr3 <- "Cost ~ Decor + Service"
fit3 <- lm(fr3, data = df3)
out <- NULL
out <- mlreg(df = df3, fit = fit3, fr = fr3, 3)

cat("\n\nComparing Models using anova\n")
anova(fit3, fit2)

cat("\n\nPrediction with Confidence Interval for the Model with new data file\n")
dfnew <- read.csv(file.choose())
dfpred1 <- predict(fit3,
        newdata = dfnew,
        interval = 'confidence',
        se.fit = TRUE)
dfpred1

cat("\n\nPrediction with Prediction Interval for the Model\n")
dfpred2 <- predict(fit3,
        newdata = dfnew,
        interval = 'prediction',
        se.fit = TRUE)
dfpred2

cat("\n\nImpact of predictor variable on the probability of outcome\n")
fitglm <-
  glm(Cost ~ Decor + Service,
      family = poisson(link = "identity"),
      data = df3)
dfpred3 <- predict(fitglm,
        newdata = dfnew,
        interval = 'response',
        se.fit = TRUE)
dfpred3
