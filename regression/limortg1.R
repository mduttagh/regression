#Load MukEnv1.R functions and variables for this script
# Multi Regression for Limortgage

cat("\n\nFirst Round\n")
df1 <- read.csv(file.choose())
response <- "Rate"
df1 <-
  cbind(df1 %>% select_if(colnames(df1) == response),
        df1 %>% select_if(!colnames(df1) == response))
rownames(df1) <- df1$Lender
fit1 <- lm(Rate ~ Fees, data = df1)
vars <- colnames(model.frame(fit1, df1))
out1 <- NULL
out1 <- mlreg(df = df1,
              fit = fit1,
              fr = as.formula(fit1$call[[2]]),
              1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
rownames(df2) <- df2$Lender
fit2 <- lm(Rate ~ Fees, data = df2)
out1 <- NULL
out2 <- NULL
out2 <- mlreg(df = df2,
              fit = fit2,
              fr = as.formula(fit2$call[[2]]),
              2)

cat("\n\nPrediction with Confidence Interval for the Model with new data \n")
dfnew <- data.frame(1.5)
colnames(dfnew) <- vars[2]
dfpred2 <- predict(fit2,
                   newdata = dfnew,
                   interval = 'confidence',
                   se.fit = TRUE)
dfpred2


cat("\n\nThird Round after Best Subset Model\n")
df3 <- df2
response <- "Rate"
rownames(df3) <- df3$Lender
fit3 <- lm(Rate ~ Fees + Jumbo + Fees:Jumbo, data = df3)
vars <- colnames(model.frame(fit3, df3))
out3 <- NULL
out3 <- mlreg(df = df3,
              fit = fit3,
              fr = as.formula(fit3$call[[2]]),
              3)

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
