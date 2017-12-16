# Initialize Libraries
packages <-
  c(
    "GGally",
    "XML",
    #ReadHTMLTable
    "stringr",
    "corrplot",
    #Corr Scatterplot
    "PerformanceAnalytics",
    #Corr Scatterplot
    "RColorBrewer",
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
    "corrgram",
    #corrgram
    "leaps",
    # regsubsets
    "gvlma",
    #global validation of linear model assumptions
    "bestglm",
    #best glm including logistic regression, 2 binary factor vars
    "relaimpo",
    "broom",
    #tidy(lmfit)
    #Relative importance calc.relimp, boot.relimp, booteval.relimp
    "HH",
    #ancova
    "car",
    #Corr scatterplot, scatterplotMatrix
    "dplyr",
    # data manipulation - select, filter, group_by, summarise, arrange, join, mutate, select_if
    "texreg" #Screenreg
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
options(digits = 2)

# Multi-linear regression analysis function
mlreg <- function(df, fit, num) {
  par(old.par)
  fr <- as.formula(fit$call[[2]])
  frdeparse <- deparse(fr, width.cutoff = 500L)
  vars <- colnames(model.frame(fit, df))
  response <- vars[1]
  cat("\n\nMulti-linear regression analysis iteration#:", num, "\n")
  cat("\n\nSummary Statistics\n")
  print(summary(df, digits = 4))
  cat("\n\nDescribe \n")
  print(describe(df, IQR = TRUE))
  cat("\n\nSelect Numeric and Factor Columns\n")
  rm("dfc", "dfn")
  dfn <- NULL
  dfn <- select_if(df, is.numeric)
  dfc <- NULL
  dfc <-
    df %>% dplyr::select(one_of(response), which(sapply(., is.factor)))
  if (count(dfn) >= 3 &
      count(dfn) <= 5000) {
    cat("\n\nStatistics Description \n")
    print(stat.desc(dfn, norm = TRUE, p = 0.95))
    cat("\n\nShapiro-Wilk Normality Test \n")
    print(tidy(sapply(dfn, shapiro.test)))
  }
  cat("\n\nBoxplot for Model Numerics\n")
  i <- NULL
  for (i in 1:length(dfn)) {
    if (!is.na(dfn[i]))
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
    for (i in 1:length(dfc))
      if (!is.na(dfc[i]))
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
  scatterplotMatrix(dfn,
                    id.n = 4,
                    main = "Scatter Plot Matrix",
                    spread = FALSE)
  #scatterplotMatrix(df, id.n = 4, diagonal = "histogram", main = "Scatter Plot Matrix")
  #scatterplotMatrix(df, id.n = 4, diagonal = "qqplot", main = "Scatter Plot Matrix")
  cat("\n\nCorrelation Diagram")
  chart.Correlation(
    dfn,
    histogram = TRUE,
    pch = 19,
    main = paste("Correlation Chart Full Data")
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
  invisible(readline(prompt = "Press [enter] to continue"))
  cat("\n#------MODEL--------------------------------------------------\n")
  cat("\n\nLinear Regression Model \n")
  # the points
  # the regression line (in green)
  # the smoothed conditional spread (in red dashed line)
  # the non-parametric regression smooth (solid line, red)
  # scatterplot(
  #   fr,
  #   data = df,
  #   smoother = loessLine,
  #   id.n = 5,
  #   reg.line = lm,
  #   pch = 19,
  #   main = paste("Fitted Line Plot: ", frdeparse),
  #   lwd = 2,
  #   by.groups = FALSE
  # )
  # To Show Confidence Interval and Prediction Interval on the fitted regression line
  # temp_var <- predict(fit, interval = "prediction")
  # new_df <- cbind(df, temp_var)
  # p <- ggscatter(
  #   data = new_df,
  #   x = vars[2],
  #   y = response,
  #   add = c("reg.line"),
  #   add.params = list(color = "blue", fill = "lightgray"),
  #   conf.int = TRUE
  # ) + geom_line(aes(y = lwr), color = "red", linetype = "dashed") + geom_line(aes(y = upr), color = "red", linetype = "dashed") + theme_pubr()
  # print(ggpar(
  #   p,
  #   main = paste(
  #     "Fitted Regression Line with 95% CI and 95% PI\n",
  #     response,
  #     "vs.",
  #     colnames(vars[2]),
  #     "\nAdj R2=",
  #     signif(summary(fit)$adj.r.squared, 5),
  #     "Intercept=",
  #     signif(fit$coef[[1]], 5),
  #     " Slope=",
  #     signif(fit$coef[[2]], 5),
  #     " p=",
  #     signif(summary(fit)$coef[2, 4], 5)
  #   ),
  #   font.main = c(16, "bold"),
  #   font.x = c(14, "bold"),
  #   font.y = c(14, "bold")
  # ))
  cat("\n\nScatter Plot of Residuals vs. Predictors\n")
  print(residualPlots(
    fit,
    id.n = 5,
    main = "Scatter Plot of Residuals vs. Predictors ",
    lwd = 2
  ))
  if (count(dfn) >= 3 &
      count(dfn) <= 5000) {
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
  cat(
    "\n#------MULTI-COLLINEARITY------------------------------------------------\n"
  )
  len <- NULL
  len <- length(vars)
  if (len <= 2) {
    "No VIF"
    "No VIF > 2"
  }
  else {
    cat("\n\nVIF and VIF Test \n")
    print(cbind(vif(fit), sqrt(vif(fit)[3]), as.data.frame(sqrt(vif(
      fit
    )[3]) > 2)))
  }
  cat("\n\nDublin Watson Test for autocorrelation\n")
  print(durbinWatsonTest(fit, alternative = "two.sided"))
  cat("\n\nConfidence Intervals for model parameters\n")
  print(confint(fit, levels = 0.95))
  cat("\n\nCovariance Matrix for model parameters\n")
  print(vcov(fit))
  #ancova(fr,data = df,main = frdeparse)
  #cat("\n\nTukey's Honest Significance Test for multiple comparisons \n")
  #thsd <- NULL
  #thsd <- TukeyHSD(aov1)
  #print(thsd)
  #plot(thsd, las = 1)
  cat("\n\nAnova Table for unbalanced design \n")
  aov3 <- NULL
  aov3 <- Anova(aov1, type = "III")
  print(aov3)
  cat("\n\nTidy Regression Model \n")
  print(tidy(fit, conf.int = TRUE))
  cat(
    "\n#------INFLUCIAL OUTLIERS-----------------------------------------------\n"
  )
  cat("\n\nOutlier Test \n")
  out <- NULL
  out <- outlierTest(fit, digits = 4)
  print(out)
  outt <- NULL
  if (out$signif) {
    outt <- names(out$bonf.p)
  } else
    outt <- NULL
  print(df[outt, ])
  cat("\n\nInfluce Plot Outliers \n")
  infout <- NULL
  infout <-
    influencePlot(fit,
                  main = paste("Influence Plot\n", frdeparse),
                  sub = "Circle size is proportional to Cook's Distance")
  print(infout)
  cat("\n\nInfluce Index Plot \n")
  influenceIndexPlot(
    fit,
    id.n = 3,
    labels = rownames(df),
    main = paste("Influence Index Diagnostic Plots\n", frdeparse)
  )
  cat("\n\nInfluence Plot RowNames \n")
  infoutrn <- NULL
  infoutrn <- rownames(infout)
  print(df[infoutrn, ])
  cat("\n\nIntersection of OutlierTest and Influence Plot Outliers\n")
  ret <- intersect(outt, infoutrn)
  print(ret)
  #cat("\n\nInfluence Measures from AutoPlot\n")
  #print(influence.measures(fit))
  cat("\n\nAdded Value Plot to asses impact of influence observations \n")
  avPlots(fit,
          ask = FALSE,
          id.n = 1,
          main = "Added Value Plot to asses impact of influence observations")
  cat("\n#------DIAGNOSTIC PLOTS-------------------------------------------\n")
  cat("\n\nAutoPlot for Regression Model \n")
  print(autoplot(
    fit,
    which = 1:6,
    ncol = 2,
    label.size = 3,
    label.repel = FALSE
  ) + theme_pubr())
  cat("\n\nQQ Plot to Test the Normality\n")
  print(qqPlot(
    fit,
    id.n = 5,
    simulate = TRUE,
    id.location = "ab",
    main = paste("QQ Plot to Test the Normality\n", frdeparse)
  ))
  if (!sum(str_detect(fr, ":")) > 0) {
    cat("\n\nCompnent plus Residual Plots for Linearity Assumption\n")
    crPlots(fit,
            id.n = 5,
            main = "Compnent + Residual Plots for Linearity Assumption")
  }
  cat("\n\nDurbin Watson Test for Independence of Errors Test \n")
  print(durbinWatsonTest(fit))
  cat(
    "\n#------NON-CONSTANT ERROR VARIANCE---------------------------------------\n"
  )
  cat("\n\nNon Constant Error Plot for Assesing Constant Variane Assumption\n")
  print(ncvTest(fit))
  cat("\n\nSpread Level Plot for Assesing Constant Variance Assumption\n")
  print(
    spreadLevelPlot(fit, id.n = 3, main = "Spread Level Plot for Assesing Constant Variance Assumption")
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
  }
  else {
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
    print(summary(powerTransform(dfn[i])))
  }
  if (sum(signif(dfn < 0), na.rm = TRUE) == 0) {
    cat("\n\nBoxTidwell Test: Transforming X Variables if violates normality assumption\n")
    print(boxTidwell(fr, data = df))
  }
  else
    cat("\n\nNegative Sum Not equal to 0\n")
  par(old.par)
}
