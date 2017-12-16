#Load MukEnv1.R functions and variables for this script
# Linear Regression for American Housing Survey: Housing Affordability Data System
# Data: The primary data source is the Public Use Files (PUFs) of the American Housing Survey. https://www.huduser.gov/portal/datasets/hads/hads.html "https://www.huduser.gov/portal/datasets/hads/hads2013n_ASCII.zip"

cat("\n\nFirst Round\n")
df1 <- read.csv(file.choose(), stringsAsFactors = FALSE) #thads2013n.csv
response <- "IBM.Close"
df1 <-
  cbind(df1 %>% select_if(colnames(df1) == response),
        df1 %>% select_if(!colnames(df1) == response))
df1$IBM.Date <- as.Date(df1$IBM.Date, "%m/%d/%Y")
df1 <- df2
df1 <-
  df1 %>% select_if(colnames(df1) %in% c("IBM.Close", "SP.Close", "IBM.Date")) %>% filter(IBM.Date >= as.Date("2014-01-01"))
df1 <- arrange(df1, desc(IBM.Date))
df1 <-
  mutate(df1, IBM.Returns = ((IBM.Close - lag(IBM.Close)) / lag(IBM.Close)),
         SP.Returns = ((SP.Close - lag(SP.Close)) / lag(SP.Close))) %>% na.omit()
rownames(df1) <- df1$IBM.Date
response <- "IBM.Returns"
fit1 <- lm(IBM.Returns ~ SP.Returns, data = df1)
vars <- colnames(model.frame(fit1, df1))
out1 <- NULL
out1 <- mlreg(df = df1,
              fit = fit1,
              fr = as.formula(fit1$call[[2]]),
              1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
fit2 <- lm(IBM.Returns ~ SP.Returns, data = df2)
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
df3 <- subset(df3, !(row.names(df3) %in% out3)) # For iterations
fit3 <-
  lm(IBM.Returns ~ SP.Returns, data = df3)
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
