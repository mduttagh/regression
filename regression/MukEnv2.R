# Initialize Libraries
packages <-
  c(
    "GGally",
    "effects",
    #effect
    "gplots",
    #plotmeans
    "lessR",
    "gridExtra",
    "XML",
    #ReadHTMLTable
    "stringr",
    "corrplot",
    #Corr Scatterplot
    "PerformanceAnalytics",
    #Corr Scatterplot
    "RColorBrewer",
    "gmodels",
    # CrossTable()
    "lda",
    #"plyr",# Mapvalues
    "psych",
    # stats description
    "pastecs",
    # stat.desc
    "corrgram",
    #corrgram
    "leaps",
    # regsubsets
    "mice",
    # for multiple imputation
    "gvlma",
    #global validation of linear model assumptions
    "bestglm",
    #best glm including logistic regression, 2 binary factor vars
    "relaimpo",
    "broom",
    #tidy(lmfit) #Relative importance calc.relimp, boot.relimp, booteval.relimp
    "HH",
    #ancova
    "texreg",
    #Screenreg
    "tseries",
    #runs.test, adf.test, bds.test
    "forecast",
    #auto.arima, Acf, Pacf, forecast, ndiffs, ma, accurace, ma, ets
    "orcutt",
    #cochrane.orcutt
    "ggfortify",
    # autoplot
    "ggpubr",
    "car",
    #Corr scatterplot, scatterplotMatrix
    "dplyr",
    # data manipulation - select, filter, group_by, summarise, arrange, join, mutate, select_if
    "tidyverse"
    # gather, spread, separate, unite
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

# Initialize
ret <- NULL
options(scipen = 10)
options(digits = 3)
dev.off()
old.par <-
  par(no.readonly = TRUE) # all par settings which could be changed.

# Multi-linear regression analysis function
mlreg <- function(df, fit, num) {
  par(old.par)
  fr <- as.formula(fit$call[[2]])
  frdeparse <- deparse(fr, width.cutoff = 500L)
  vars <- colnames(model.frame(fit, df))
  dfrn <-
    as.data.frame(df %>% dplyr::select(one_of(vars)) %>% na.omit())
  response <- vars[1]
  cat("\n\nMulti-linear regression analysis iteration#:", num, "\n")
  cat("\n\nSummary Statistics\n")
  print(summary(df, digits = 4))
  cat("\n\nDescribe \n")
  print(describe(df, IQR = TRUE))
  cat("\n\nSelect Numeric and Factor Columns\n")
  #rm("dfc", "dfn")
  dfn <- NULL
  dfn <- select_if(df, is.numeric)
  dfc <- NULL
  dfc <-
    dfrn %>% dplyr::select(one_of(response), which(sapply(., is.factor)))
  if (sum(count(dfn)) >= 3 &
      sum(count(dfn)) <= 5000) {
    cat("\n\nStatistics Description \n")
    print(stat.desc(dfn, norm = TRUE, p = 0.95))
    cat("\n\nShapiro-Wilk Normality Test \n")
    print(t(sapply(dfn, shapiro.test))[, 1:2])
  }
  cat("\n\nBoxplot for Model Numerics\n")
  i <- NULL
  par(mfrow = c(2, 2))
  for (i in 1:length(dfn)) {
    print(
      Boxplot(
        y = dfn[i],
        ylab = colnames(dfn[i]),
        labels = rownames(dfn),
        col = brewer.pal(8, "Pastel2"),
        main = paste("Boxplot for ", colnames(dfn[i]))
      )
    )
  }
  cat("\n\nBoxplot for Model Factors\n")
  i = NULL
  if (length(dfc) >= 2) {
    for (i in 2:length(dfc))
      print(
        Boxplot(
          y = dfc[[1]],
          g = dfc[[i]],
          glim = 100,
          ylab = colnames(dfc[1]),
          xlab = colnames(dfc[i]),
          labels = rownames(dfc),
          col = brewer.pal(8, "Pastel2"),
          main = paste("Boxplot for ", colnames(dfc[1]), "vs.", colnames(dfc[i]))
        )
      )
  }
  par(old.par)
  cat("\n\nBasic Scatter Plot\n")
  # scatterplot(
  #   fr,
  #   data = df,
  #   smoother = NULL,
  #   reg.line = NULL,
  #   id.n = 5,
  #   main = paste("Scatter Plot to show relationship between\n", frdeparse),
  #   pch = 19,
  #   spread = FALSE,
  #   by.groups = FALSE
  # )
  p <- NULL
  i <- NULL
  for (i in 2:length(dfn)) {
    p <- ggscatter(
      data = dfn,
      x = colnames(dfn[i]),
      y = response,
      conf.int = FALSE,
      cor.coef = FALSE
    ) + theme_pubr()
    print(ggpar(
      p,
      main = paste("Scatterplot\n", response, "vs.", colnames(dfn[i])),
      font.main = c(16, "bold"),
      font.x = c(14, "bold"),
      font.y = c(14, "bold")
    ))
    p <- ggscatter(
      data = dfn,
      x = colnames(dfn[i]),
      y = response,
      add = "reg.line",
      add.params = list(color = "blue", fill = "lightgray"),
      conf.int = TRUE,
      cor.coef = TRUE
    ) + theme_pubr()
    print(ggpar(
      p,
      main = paste("Fitted Line Plot:\n", response, "vs.", colnames(dfn[i])),
      font.main = c(16, "bold"),
      font.x = c(14, "bold"),
      font.y = c(14, "bold")
    ))
    p <- ggscatter(
      data = dfn,
      x = colnames(dfn[i]),
      y = response,
      add = "loess",
      add.params = list(color = "blue", fill = "lightgray"),
      conf.int = TRUE,
      cor.coef = TRUE
    ) + theme_pubr()
    print(ggpar(
      p,
      main = paste(
        "Scatter Plot with Loess Line:\n",
        response,
        "vs.",
        colnames(dfn[i])
      ),
      font.main = c(16, "bold"),
      font.x = c(14, "bold"),
      font.y = c(14, "bold")
    ))
  }
  cat("\n\nScatter Plot Matrix \n")
  # scatterplotMatrix(
  #   df,
  #   id.n = 4,
  #   main = "Scatter Plot Matrix",
  #   use = "pairwise.complete.obs",
  #   spread = FALSE
  # )
  #s3d <-scatterplot3d(df[2],df[3],df[1], pch=16, highlight.3d=TRUE,type="h", main="3D Scatterplot")
  #catterplotMatrix(df, id.n = 4, diagonal = "histogram", main = "Scatter Plot Matrix")
  #catterplotMatrix(df, id.n = 4, diagonal = "qqplot", main = "Scatter Plot Matrix")
  par(mfrow = c(2, 2))
  cat("\n\nCorrelation Diagram")
  chart.Correlation(
    dfn,
    histogram = TRUE,
    pch = 19,
    main = paste("Correlation Chart")
  )
  cat("\n\nCorrelation Diagrams \n")
  corrgram(
    df,
    #order = TRUE,
    main = "Correlation Diagram",
    lower.panel = panel.shade,
    upper.panel = panel.cor,
    text.panel = panel.txt,
    diag.panel = panel.density
  )
  cat("\n\nCorrelation Plot \n")
  corrplot(cor(dfn),
           method = "number",
           main = "Correlation Plot") #order = "hclust")
  print(
    ggcorr(
      df[vars],
      label = TRUE,
      label_alpha = TRUE,
      label_round = 2,
      layout.exp = 1
    ) + ggtitle("Correlation Matrix1")
  )
  print(ggscatmat(df[vars]) + ggtitle("Correlation Matrix2"))
  cat("\n\nScatter Plot of variables\n")
  ggpairs(df[vars],title = "Scatter Plot for most variables",lower = list(continuous = "smooth"))
  invisible(readline(prompt = "Press [enter] to continue"))
  
  cat("\n#------MODEL--------------------------------------------------\n")
  cat("\n\nLinear Regression Model \n")
  # the points
  # the regression line (in green)
  # the smoothed conditional spread (in red dashed line)
  # the non-parametric regression smooth (solid line, red)
  par(old.par)
  if (length(vars) == 2) {
    cat("\n\nFitted Regression Line\n")
    scatterplot(
      fr,
      data = dfrn,
      smoother = loessLine,
      id.n = 5,
      reg.line = lm,
      pch = 19,
      labels = rownames(dfrn),
      main = paste("Fitted Line Plot: ", frdeparse),
      lwd = 2,
      by.groups = FALSE
    )
    #To Show Confidence Interval and Prediction Interval on the fitted regression line
    cat("\n\nCI and PI Fitted Regression Line\n")
    temp_var <- predict(fit, interval = "prediction")
    new_df <- cbind(dfrn, temp_var)
    p <- ggscatter(
      data = new_df,
      x = colnames(new_df[2]),
      y = colnames(new_df[1]),
      add = c("reg.line"),
      add.params = list(color = "blue", fill = "lightgray"),
      conf.int = TRUE
    ) + geom_line(aes(y = lwr),
                  data = new_df,
                  color = "red",
                  linetype = "dashed") + geom_line(aes(y = upr),
                                                   data = new_df,
                                                   color = "red",
                                                   linetype = "dashed") + theme_pubr()
    print(ggpar(
      p,
      main = paste(
        "Fitted Regression Line with 95% CI and 95% PI\n",
        colnames(new_df[1]),
        "vs.",
        colnames(new_df[2]),
        "\nAdj-R2=",
        round(summary(fit)$adj.r.squared, digits = 3),
        "B0=",
        round(fit$coef[[1]], digits = 3),
        " Slope=",
        round(fit$coef[[2]], digits = 3),
        " p=",
        round(summary(fit)$coef[2, 4], digits = 3)
      ),
      font.main = c(14, "bold"),
      font.x = c(14, "bold"),
      font.y = c(14, "bold")
    ))
  }
  cat("\n\nScatter Plot of Residuals vs. Predictors\n")
  print(
    residualPlots(
      fit,
      id.n = 5,
      type = "rstudent",
      linear = TRUE,
      smoother = loessLine,
      quadratic = FALSE,
      label = rownames(dfrn),
      main = "Scatter Plot of Studentized Residuals vs. Predictors",
      lwd = 2
    )
  )
  if (length(fit$residuals) >= 3 &
      length(fit$residuals) <= 5000) {
    cat("\n\nShapiro-Wilk Normality Test of Residuals\n")
    print(shapiro.test(fit$residuals))
  }
  cat("\n\n K-S Normality Test of Residuals\n")
  print(ks.test(fit$residuals, "pnorm"))
  #cat("\n\nLevene's Test for homogenity of variances\n")
  #leveneTest(fr,data = df)
  cat("\n\nSummary Regression Model \n")
  print(summary(fit))
  cat("\n\nAnova Table \n")
  aov1 <- NULL
  aov1 <- aov(fit)
  print(summary(aov1))
  cat("\n\nF-test Critical Value for alpha 5%\n")
  cat("df1 =", df1 <- length(fit$coef) - 1, "\t")
  cat("df2 =", df2 <- nrow(model.matrix(fit)) - df1 - 1, "\t")
  cat("n =", nrow(model.matrix(fit)), "\n")
  alpha = 0.05
  beta = 1 - alpha
  print(F.stat <-
          qf(
            p = alpha,
            df1 = df1,
            df2 = df2,
            lower.tail = FALSE
          )) # P[X > x] or qf(beta, df1=df1, df2=df2)
  par(mfrow = c(1, 2))
  curve(
    df(x = x, df1 = df1, df2 = df2),
    from = 0,
    to = 15,
    col = "chocolate",
    lwd = 2,
    xlab = "x-value",
    ylab = "F value",
    main = paste(
      "F-distributin with df1 =",
      df1,
      ", df2 =",
      df2,
      "\n with vLine at x =",
      round(F.stat, 3)
    )
  )
  abline(v = F.stat, lwd = 2)
  cat("\n\nTwo-tailed t-test Ctitical Value for alpha 5%\n")
  print(t.stat <- qt(alpha / 2, df2)) # P[X ??? x]
  curve(
    dt(x = x, df = df2),
    from = -4.5,
    to = +4.5,
    col = "chocolate",
    lwd = 2,
    xlab = "x-value",
    ylab = "Students t value",
    main = paste(
      "Student's t-distributin with df =",
      df2,
      "\n with vLine at x =",
      round(t.stat, 3)
    )
  )
  abline(v = c(-abs(t.stat), abs(t.stat)), lwd = 2)
  prob.tcut(df2, alpha = alpha) #plot t-distribution with normal curve
  #dev.set(which = dev.prev())
  par(old.par)
  cat(
    "\n#------MULTI-COLLINEARITY------------------------------------------------\n"
  )
  len <- NULL
  len <- length(vars)
  if (len <= 2) {
    "No VIF"
    "No VIF > 2"
  } else {
    cat("\n\nVIF and VIF Test \n")
    v <- car::vif(fit)
    colnames(v) <- c("GVIF", "DF", "VIF")
    v1 <- as.data.frame(sqrt(v) > 2)
    colnames(v1) <- c("1", "2", "SQRT(VIF)>2")
    print(cbind(v, v1[3]))
  }
  cat("\n\nDublin Watson Test for autocorrelation (or Independence of Errors)\n")
  print(durbinWatsonTest(fit))
  cat("\n\nConfidence Intervals for model parameters\n")
  print(confint(fit, levels = 0.95))
  cat("\n\nCovariance Matrix for model parameters\n")
  print(vcov(fit))
  #ancova(fr,data = df,main = frdeparse)
  if (length(dfc) >= 2) {
    cat("\n\nTukey's Honest Significance Test for multiple comparisons \n")
    thsd <- NULL
    thsd <- TukeyHSD(aov1, colnames(dfc[2]))
    print(thsd)
    par(mar = c(5, 8, 4, 2))
    plot(thsd, las = 2)
  }
  par(old.par)
  cat("\n\nAnova Table for unbalanced design \n")
  aov3 <- NULL
  aov3 <- Anova(aov1, type = "III")
  print(aov3)
  cat("\n\nTidy Regression Model \n")
  print(tidy(fit, conf.int = TRUE))
  cat(
    "\n#------INFLUCIAL OUTLIERS-----------------------------------------------\n"
  )
  par(old.par)
  cat("\n\nOutlier Test \n")
  out <- NULL
  out <- outlierTest(fit, digits = 4, labels = rownames(dfrn))
  print(out)
  outt <- NULL
  if (out$signif) {
    outt <- names(out$bonf.p)
  } else
    outt <- NULL
  print(df[outt, ])
  # cat("\n\nInfluce Plot Outliers \n")
  infout <- NULL
  par <- old.par
  infout <-
    influencePlot(
      fit,
      label = rownames(dfrn),
      main = paste("Influence Plot\n", frdeparse),
      sub = "Circle size is proportional to Cook's Distance"
    )
  print(infout)
  par(old.par)
  cat("\n\nInfluce Index Plot \n")
  influenceIndexPlot(
    fit,
    id.n = 3,
    label = rownames(dfrn),
    main = paste("Influence Index Diagnostic Plots\n", frdeparse)
  )
  cat("\n\nInfluence Plot RowNames \n")
  infoutrn <- NULL
  infoutrn <- rownames(infout)
  print(df[infoutrn, ])
  cat("\n\nIntersection of OutlierTest and Influence Plot Outliers\n")
  ret <- intersect(outt, infoutrn)
  print(ret)
  cat("\n\nInfluence Measures from AutoPlot\n")
  par(old.par)
  print(influence.measures(fit))
  cat("\n\nAdded Value Plot to asses impact of influence observations \n")
  avPlots(
    fit,
    ask = FALSE,
    id.n = 2,
    label = rownames(dfrn),
    main = "Added Value Plot to asses impact of influence observations"
  )
  cat("\n#------DIAGNOSTIC PLOTS-------------------------------------------\n")
  cat("\n\nAutoPlot for Regression Model \n")
  par(old.par)
  print(
    autoplot(
      fit,
      which = 1:6,
      ncol = 2,
      label.size = 3,
      labels = rownames(dfrn),
      label.repel = TRUE
    )
  )
  #invisible(readline(prompt = "Press [enter] to continue"))
  cat("\n\nQQ Plot to Test the Normality\n")
  par(old.par)
  print(
    qqPlot(
      fit,
      id.n = 5,
      simulate = TRUE,
      id.location = "ab",
      label = rownames(dfrn),
      main = paste("QQ Plot of Studentized Residuals\n",
                   frdeparse),
      pch = 19
    )
  )
  
  # distribution of studentized residuals
  par(old.par)
  print(
    ggpubr::gghistogram(
      studres(fit),
      xlab  = "Stendentized Residuals",
      y = "..density..",
      rug = TRUE,
      fill = "lightblue",
      add_density = TRUE,
      main = "Distribution of Studentized Residuals"
    )
  )
  
  if (!sum(str_detect(fr, ":")) > 0) {
    cat("\n\nComponent plus Residual Plots for Linearity Assumption\n")
    crPlots(fit,
            id.n = 3,
            label = rownames(dfrn),
            main = "Component + Residual Plots (Partial Residual Plots)")
  }
  cat(
    "\n#------NON-CONSTANT ERROR VARIANCE---------------------------------------\n"
  )
  cat("\n\nNon Constant Error Plot for Assesing Constant Variane Assumption\n")
  print(ncvTest(fit))
  cat("\n\nSpread Level Plot for Assesing Constant Variance Assumption\n")
  print(
    spreadLevelPlot(
      fit,
      id.n = 3,
      label = rownames(dfrn),
      main = "Spread Level Plot for Assesing Constant Variance Assumption"
    )
  )
  cat("\n\nGlobal Validation of Linear Model Assumption\n")
  print(gvlma(fit))
  cat(
    "\n#------VARIABLE SELECTION------------------------------------------------\n"
  )
  par(old.par)
  if (len <= 2) {
    cat("\n\nNo Regsubsets\n")
    cat("No Summary Regss\n")
    cat("No Plor Regss adjr2\n")
    cat("No Plot Reggs Cp\n")
    cat("No COEF\n")
    cat("No VCOV\n")
  } else {
    regss <- regsubsets(fr, data = df, nbest = 8)
    cat("\n\nSummary Subsets Regression\n")
    #cat("\n\nBest models for each subset size based upon Adjusted R-square\n")
    plot(
      regss,
      scale = "adjr2",
      col = brewer.pal(8, "Dark2"),
      main = paste(
        "Best models for each subset size based upon Adjusted R-square\n",
        frdeparse
      )
    )
    #cat("\n\nBest models for each subset size based upon Mallows Cp\n")
    plot(
      regss,
      scale = "Cp",
      col = brewer.pal(8, "Dark2"),
      main = paste(
        "Best models for each subset size based upon Mallow Cp\n",
        frdeparse
      )
    )
    cat("\n\nCoeficient of Regular Subsets\n")
    print(coef(regss, 1:regss$np))
    cat("\n\nVCOV of Regular Subsets\n")
    print(vcov(regss, regss$np))
  }
  if (!len <= 2) {
    cat("\n#------RELATIVE IMPORTANCE OF PREDICTOR VARIABLES----------------------\n")
    cat("\n\nBootstrap Measures of Relative Importance (1000 samples)\n")
    boot <- boot.relimp(
      fit,
      b = 1000,
      type = c("lmg"),
      rank = TRUE,
      diff = TRUE,
      rela = TRUE
    )
    #print(booteval.relimp(boot))
    cat("\n\nPrint Bootstrap Measures\n")
    plot(booteval.relimp(boot))
  }
  par(old.par)
  return(ret)
}

powerT <- function(df, fit, num) {
  par(old.par)
  fr <- as.formula(fit$call[[2]])
  frdeparse <- deparse(fr, width.cutoff = 500L)
  vars <- colnames(model.frame(fit, df))
  response <- vars[1]
  dfn <- select_if(df, is.numeric)
  cat(
    "\n#------VIOLATES NORMALITY-----------------------------------------------\n"
  )
  cat(
    "\n\nMax Likelyhood estimation of the power lambda for Y Variable if violates normality assumption\n"
  )
  for (i in 1:length(dfn)) {
    cat("\n**********Variable# ", i, "\n")
    print(summary(powerTransform(dfn[i])))
  }
  cat("\n\nProfile log-likelihood function for \\??\n")
  print(boxCox(fit, main = "Profile log-likelihood function for ??"))
  if (sum(signif(dfn < 0), na.rm = TRUE) == 0) {
    cat("\n\nBoxTidwell Test: Transforming X Variables if violates normality assumption\n")
    print(boxTidwell(fr, data = df))
  }
  else
    cat("\n\nNegative Sum Not equal to 0\n")
  par(old.par)
}

tsmlreg <- function (df, fit, num, ts.stdres) {
  fr <- as.formula(fit$call[[2]])
  frdeparse <- deparse(fr, width.cutoff = 500L)
  vars <- colnames(model.frame(fit, df))
  dfrn <-
    as.data.frame(df %>% dplyr::select(one_of(vars)) %>% na.omit())
  n <- length(fit$residuals)
  print(summary(fit))
  if (length(vars) <= 2) {
    "No VIF"
    "No VIF > 2"
  } else {
    cat("\n\nVIF and VIF Test \n")
    print(cbind(vif(fit), sqrt(vif(fit)), as.data.frame(sqrt(vif(
      fit
    )) > 2)))
  }
  #std.res <- rstandard(fit)
  #ts.stdres <- ts(rstandard(fit))
  cat(
    "\n\nDublin Watson Test for the Model for autocorrelation (or Independence of Errors)\n"
  )
  print(durbinWatsonTest(fit, alternative = "two.sided"))
  check_ts(ts.stdres,
           fit = fit,
           dfrn = dfrn,
           iteration = "Original")
  #
  # Cochrane-Orcutt procedure
  #
  cat(
    "\n\n**************************Cochrane-Orcutt procedure********************************************\n"
  )
  print(fitco <- cochrane.orcutt(fit))
  print(summary(fitco))
  check_ts(
    ts(residual.orcutt(fitco)),
    fit = fit,
    dfrn = dfrn,
    iteration = "Orcutt"
  )
  cat("\n\nAuto Arima to select a BEST ARIMA model\n")
  print(fitarima <- auto.arima(na.omit(ts.stdres), ic = c("bic")))
  cat("\n\nAuto Arima Plot to select a BEST ARIMA model\n")
  print(autoplot.Arima(fitarima, type = "ar"))
  cat(
    "\n\nAccuracy Test Reports fit (predictive accurace) measures for a time-series model\n"
  )
  print(accuracy(fitarima))
  check_ts(
    ts(fitarima$residuals),
    fit = fit,
    dfrn = dfrn,
    iteration = "AutoArima"
  )
  #cat("\n\nDiagnostic Checking for arima\n")
  #print(tsdiag(fitarima))
  cat("\n\nForecast 5 years ahead: Autoarima\n")
  print(ff <- forecast(fitarima, 5))
  cat("\n\nForecast Plot 5 years ahead: Autoarima\n")
  print(autoplot(
    forecast(ff),
    which = 1:6,
    #ylab = "Standard Residuals",
    xlab = "Time",
    flty = 2
  ))
  return (fitco)
}

check_ts <- function (ts.stdres, fit, dfrn, iteration) {
  #
  # Runs test function given in the file for the "Ordinary least squares estimation and
  # time series data" handout
  #
  cat(
    "\n\n**************************",
    iteration,
    "***********************************************",
    "\n"
  )
  dfn <- NULL
  dfn <- select_if(dfrn, is.numeric)
  std.res <- rstandard(fit)
  cat("\n\nCorrelation Diagram")
  chart.Correlation(
    dfn,
    histogram = TRUE,
    pch = 19,
    main = paste("Correlation Chart")
  )
  
  cat("\n\nAutoPlot for Regression Model \n")
  print(
    autoplot(
      fit,
      which = 1:6,
      ncol = 2,
      label.size = 3,
      labels = rownames(dfrn),
      label.repel = TRUE
    )
  )
  cat("\n\nPlot of fitted values:", iteration, "\n")
  print(
    scatterplot(
      fitted(fit),
      std.res,
      #xlab = "Fitted values",
      #ylab = "Standardized residuals",
      main = paste("Fitted Values Scatter Plot:", iteration),
      labels = rownames(dfrn),
      pch = 19,
      smoother = NULL,
      reg.line = NULL
    )
  )
  cat(
    "\n\nRuns Test to test autocorrlation or test randomness of the dichotomous series:",
    iteration,
    "\n"
  )
  print(runs.test(factor(sign(std.res))))
  cat(
    "\n\nAugmented Dickey-Fuller Test to test the assumption of stationary",
    iteration,
    "\n"
  )
  print(adf.test(ts.stdres))
  cat("\n\nPlot of Time Series:", iteration, "\n")
  p2 <-
    autoplot(
      ts.stdres,
      which = 1:6,
      ylab = "Standardized residuals",
      main = paste("Time Series Plot:", iteration)
    )
  cat("\n\nAutocorrelation function (Acf) Plot:", iteration, "\n")
  print(a <- Acf(ts.stdres, plot = FALSE))
  p3 <-
    autoplot(a, main = paste("Autocorrelation Function Residual:", iteration))
  cat("\n\nPartial autocorrelation (Pacf) Plot:", iteration, "\n")
  print(p <- Pacf(ts.stdres, plot = FALSE))
  p4 <-
    autoplot(p,
             main = paste("Partial Autocorrelation Function Residual:", iteration))
  cat ("\n\nPrint Grid:", iteration, "\n")
  grid.arrange(p2, p3, p4, ncol = 2, nrow = 2)
  cat("\n\nQQPlot of Standardized Residuals:", iteration, "\n")
  print(
    qqPlot(
      std.res,
      id.n = 5,
      simulate = TRUE,
      id.location = "ab",
      labels = rownames(dfrn),
      ylab = "Standardized Residuals",
      main = paste("Normal QQ Plot of Standardized Residuals:",
                   iteration),
      pch = 19
    )
  )
  par(old.par)
  cat(
    "\n\nBox Test to test that the autocorrelations are all zero or that the residuals of a time series are independent:",
    iteration,
    "\n"
  )
  print(Box.test(ts.stdres, type = "Ljung-Box"))
  cat(
    "\n\nBDS test that a series consists of independent, identically distributed random variables:",
    iteration,
    "\n"
  )
  print(bds.test(ts.stdres))
  cat("\n\nndiffs function used to help determine the best value of 'd':",
      iteration,
      "\n")
  print(ndiffs(ts.stdres))
  par(old.par)
}

#######################################################################################################
# Code provided by                                                                                    #
#                                                                                                     #
#    John Fox, Professor                                                                              #
#    Department of Sociology                                                                          #
#    McMaster University                                                                              #
#    Hamilton, Ontario, Canada                                                                        #
#    web: socserv.mcmaster.ca/jfox                                                                    #
#######################################################################################################
cochrane.orcutt.md <- function(mod) {
  # X <- model.matrix(mod)
  # y <- model.response(model.frame(mod))
  # e <- residuals(mod)
  # n <- length(e)
  # names <- colnames(X)
  # rho <- sum(e[1:(n-1)]*e[2:n])/sum(e^2)
  # y <- y[2:n] - rho * y[1:(n-1)]
  # X <- X[2:n,] - rho * X[1:(n-1),]
  # mod <- lm(y ~ X - 1)
  # result <- list()
  # result$coefficients <- coef(mod)
  # names(result$coefficients) <- names
  # summary <- summary(mod, corr = F)
  # result$cov <- (summary$sigma^2) * summary$cov.unscaled
  # dimnames(result$cov) <- list(names, names)
  # result$sigma <- summary$sigma
  # result$rho <- rho
  # class(result) <- 'cochrane.orcutt'
  # result
}

runs.test.md <- function (x, cont.corr = F)
{
  if (any(is.na(x)))
    stop("NAs in x")
  DNAME <- deparse(substitute(x))
  if (any(x == 0.0)) {
    cat("Removed", length(x[x == 0.0]), "zero(es)\n")
    x <- x[x != 0.0]
  }
  d <- diff(sign(x))
  f <- factor(d)
  sp <- split(d, f)
  resL <- lapply(sp, length)
  n <- length(x)
  nplus <- sum(sign(x) == 1)
  nminus <- sum(sign(x) == -1)
  m <- 2. * nplus * nminus / n + 1
  s <-
    sqrt(2. * nplus * nminus * (2. * nplus * nminus - n) / (n * n * (n -
                                                                       1)))
  R <- 1
  if (!is.null(resL$"-2"))
    R <- R + resL$"-2"
  if (!is.null(resL$"2"))
    R <- R + resL$"2"
  # Continuity correction is not used by default
  if (cont.corr)
    STATISTIC <- sign(R - m) * (abs(R - m) - .5) / s
  else
    STATISTIC <- (R - m) / s
  METHOD <- "Runs Test"
  PVAL <- 2 * pnorm(-abs(STATISTIC))
  names(STATISTIC) <- "z-statistic for runs test"
  runstuff <- c(R, m)
  names(runstuff) <-
    c("Observed number of runs", "Expected number of runs")
  structure(
    list(
      estimate = runstuff,
      statistic = STATISTIC,
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
    ),
    class = "htest"
  )
}
