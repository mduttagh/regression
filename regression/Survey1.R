#Load MukEnv1.R functions and variables for this script
# Linear Regression for House Prices and multi variate analysis
# Original Data "IT - TOS CSS Nov 2015 Prod_September 2, 2016_13.09 - Mapped ToLoad - WITH FORMULA.csv"
# Data sdfr1.txt
# R Data Format sdfr1.RDS

cat("\n\nFirst Round\n")
old.par <- par()
ifelse (!exists("mlreg", mode = "function"),
        source("MukEnv2.R"),
        "MukEnv1.R already exists")
df1 <- readRDS(".\\Data\\Homework\\sdfr1.RDS")
df0 <- df1
df1 <-
  df0 %>% select_if(function(x)
    ! all(is.na(x))) # remove columns with all NAs

df1 <- df1 %>% mutate(Recorded_Date = NULL,
                      DistribChnl = NULL,
                      Role = NULL, Others_Score <- NULL, FundR_Score <- NULL, PubSaf_Score <- NULL)
df1$Satisfied <- ifelse(df1$Satis_Score >= 4, TRUE, FALSE)
# Multiple Imputation-----------------------
# require(mice)
# df1 <- dplyr::select(df1, contains("Score"), contains("Satisfied"))
# imp <- mice(df1,seed=1234)
# fitimp <- with(imp, lm(Qual_Score ~  BPA_Score + Fin_Score + FundR_Score + HR_Score + PubSaf_Score + SIS_Score + UDW_Score + Satisfied))
# pooled <- pool(fitimp)
# df1 <- complete(imp,3)
df1 <- df1 %>% select_if(function(x)
  ! all(is.infinite(x)))
fit1 <-
  lm(Qual_Score ~  BPA_Score + Fin_Score + HR_Score + UDW_Score + Satisfied, data = df1)
vars1 <- colnames(model.frame(fit1, df1))
df1 <-
  as.data.frame(df1 %>% dplyr::select(one_of(vars1), everything()))
out1 <- NULL
#options(error=recover)
#options(error=NULL)
#par(old.par)
out1 <- mlreg(df = df1[vars1],
              fit = fit1,
              1)
#df1$Satisfied <- df1$Satisfied + 1
powerT(df = df1[vars1],
       fit = fit1,
       1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
fit2 <-
  lm(Qual_Score ~  BPA_Score + Fin_Score + HR_Score + UDW_Score + Satisfied, data = df2)
vars2 <- colnames(model.frame(fit2, df2))
df2 <-
  as.data.frame(df2 %>% dplyr::select(one_of(vars2), everything()))
out1 <- NULL
out2 <- NULL
par(old.par)
out2 <- mlreg(df = df2[vars2],
              fit = fit2,
              2)
#df2$Satisfied <- df2$Satisfied + 1
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
dfnew <- data.frame(2750)
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
df3 <- df2 #%>% na.omit()
df3 <-
  df3 %>% 
  mutate(
    Qual_Score.sqrt = sqrt(Qual_Score)
    # ,
    # BPA_Score.sqrt = sqrt(BPA_Score),
    # Fin_Score.sqrt = sqrt(Fin_Score),
    # HR_Score.sqrt = sqrt(HR_Score),
    # UDW_Score.sqrt = sqrt(UDW_Score)
  )# %>% na.omit()
                 
df3 <-
  subset(df3, !(row.names(df3) %in% out3)) # For iterations
#rownames(df3) <- df3$Industry.Name
fit3 <-
  lm(Qual_Score ~  BPA_Score + Fin_Score + HR_Score + UDW_Score, data = df3)
vars3 <- colnames(model.frame(fit3, df3))
df3 <-
  as.data.frame(df3 %>% dplyr::select(one_of(vars3), everything()))
out3 <- NULL
out3 <- mlreg(df = df3[vars3],
              fit = fit3,
              3)
#df3$Satisfied <- df3$Satisfied + 1
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
                 