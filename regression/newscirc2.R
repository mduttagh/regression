#Load MukEnv1.R functions and variables for this script
cat("\n\nFirst Round\n")
df1 <- read.delim(file.choose())
response <- "Sunday"
df1 <-
  cbind(df1 %>% select_if(colnames(df1) == response),
        df1 %>% select_if(!colnames(df1) == response))
rownames(df1) <- df1$Newspaper
fit1 <- lm(Sunday ~ Daily, data = df1)
vars <- colnames(model.frame(fit1, df1))
out1 <- NULL
out1 <- mlreg(df = df1,
              fit = fit1,
              fr = as.formula(fit1$call[[2]]),
              1)

cat("\n\nCritical t-value")
qt(c(.025, .975), df = fit1$df.residual)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
rownames(df2) <- df2$Newspaper
fit2 <- lm(Sunday ~ Daily, data = df2)
out1 <- NULL
out2 <- NULL
out2 <- mlreg(df = df2,
              fit = fit2,
              fr = as.formula(fit2$call[[2]]),
              2)

cat("\n\nPrediction with Confidence Interval for the Model with new data \n")
dfnew <- data.frame(c(600L, 2500L))
colnames(dfnew) <- vars[2]
dfpred2 <- predict(fit2,
                   newdata = dfnew,
                   interval = 'confidence',
                   se.fit = TRUE)
dfpred2

cat("\n\nPrediction with Prediction Interval for the Model\n")
dfpred3 <- predict(fit2,
                   newdata = dfnew,
                   interval = 'prediction',
                   se.fit = TRUE)
dfpred3
