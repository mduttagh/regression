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
                      Role = NULL)
df1 <-
  df1 %>% mutate(DivSch1 = case_when(
    .$DivSch %in% c("Abu-Dhabi", "Shanghai", "Public Safety") ~ "misc",
    TRUE ~ as.character(.$DivSch)
  ))
df1 <-
  df1 %>% mutate(
    DivSch1 = case_when(
      .$DivSch1 == "Student Services" ~ "ss",
      .$DivSch1 == "Finance" ~ "fin",
      .$DivSch1 == "Human Resource" ~ "hr",
      .$DivSch1 == "Information Technology" ~ "it",
      .$DivSch1 == "Payroll" ~ "py",
      .$DivSch1 == "Project Services Office (PSO)" ~ "pso",
      .$DivSch1 == "Budget & Planning" ~ "bp",
      .$DivSch1 == "Others" ~ "oth",
      .$DivSch1 == "School" ~ "sch",
      .$DivSch1 == "UDAR" ~ "udr",
      TRUE ~ as.character(.$DivSch1)
    )
  )
df1$DivSch1 <- as.factor(df1$DivSch1)
df1$Satisfied <- ifelse(df1$Satis_Score >= 4, 1, 0)
df1$QualityScore <- df1$Qual_Score * 20
#df1$Satisfied <- factor(df1$Satisfied,levels=c(0,1),labels=c("No","Yes"))
df1 <- df1 %>% select_if(function(x)
  ! all(is.infinite(x)))
#out1 <- c("1","23","63")
df1 <- subset(df1, !(row.names(df1) %in% out1))
fit1 <-
  lm(Satisfied ~  QualityScore,
     data = df1)
vars1 <- colnames(model.frame(fit1, df1))
df1 <-
  as.data.frame(df1 %>% dplyr::select(one_of(vars1)))
out1 <- NULL
#rownames(df1) <- df1$ObsDate
#ts.stdres1 <- ts(data=rstandard(fit1))
#options(error=recover)
#options(error=NULL)
#par(old.par)
out1 <- mlreg(df = df1[vars1],
              fit = fit1,
              1)
#df1$Satisfied <- df1$Satisfied + 1
tsmlreg(
  df = df1[vars1],
  fit = fit1,
  num = 1,
  ts.stdres = ts.stdres1
)
powerT(df = df1[vars1],
       fit = fit1,
       1)
#--------Logistics Regression -------------------
table(df1$Satisfied)

scatterplotMatrix(
  df1,
  id.n = 4,
  main = "Scatter Plot Matrix",
  use = "pairwise.complete.obs",
  spread = FALSE
)

stat.desc(x = df1)
describe(x = df1)
describeBy(
  x = df1$QualityScore,
  group = df1$Satisfied,
  mat = TRUE,
  digits = 2
)
Boxplot(
  QualityScore ~ Satisfied,
  data = df1,
  glim = 100,
  col = brewer.pal(8, "Pastel2"),
  main = "Boxplot between QualityScore & Satisfied"
)

glm1 <-
  glm(Satisfied ~  QualityScore,
      data = df1,
      family = binomial(link = "logit"))
glm1
summary(glm1)
## exponentiated coefficients (odds ratios) and 95% CI for exponentiated coefficients
exp(cbind(OR = coef(glm1), confint(glm1)))
residuals(glm1, type = "deviance") # residuals
g <-
  ggplot(df1, aes(x = QualityScore, y = Satisfied)) + geom_point() +
  stat_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE, colour = 'red'
  ) + stat_smooth(method = "glm", se = FALSE)
ggpar(
  g,
  main = paste(
    "Fitted Logistic and Linear Regression Line with 95% CI\n",
    "Satisfied ~  QualityScore"
  ),
  font.main = c(14, "bold"),
  font.x = c(14, "bold"),
  font.y = c(14, "bold")
)

scatterplot(
  Satisfied ~  QualityScore,
  data = df1,
  smoother = loessLine,
  id.n = 5,
  reg.line = lm,
  pch = 19,
  labels = rownames(df1),
  main = paste("Fitted Line Plot: ", "Satisfied ~  QualityScore"),
  lwd = 2,
  by.groups = FALSE
)


library(ROCR)
(p <- predict(glm1, type = "response")) # predicted values)
ROCRpred <- prediction(p, df1$Satisfied)
ROCRperf <-
  performance(ROCRpred, measure = "tpr", x.measure = "fpr")
AUCRperf <- performance(ROCRpred, measure = "auc")
plot(ROCRperf,
     colorize = TRUE,
     main = paste("Area under the curve:", round(AUCRperf@y.values[[1]] , 4)))

Anova(glm1, type = "II", test = "Wald") #to compare nested models.
anova(update(glm1, ~ 1), # update here produces null model for comparison
      glm1,
      test = "Chisq")
cdplot(factor(Satisfied) ~ QualityScore, data = df1) #will display the conditional density plot of the binary outcome F on the continuous x variable
# install.packages("Deducer")
# require(Deducer)
# rocplot(glm1,diag=TRUE,pred.prob.labels=FALSE,prob.label.digits=3,AUC=TRUE)


cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
df2 <-
  df2 %>%
  mutate(GrpScore.sqrt = sqrt(GrpScore),
         Qual_Score.sqrt = sqrt(Qual_Score))
fit2 <-
  lm(Qual_Score.sqrt ~  Group + GrpScore.sqrt,
     data = df2)
vars2 <- colnames(model.frame(fit2, df2))
df2 <-
  as.data.frame(df2 %>% dplyr::select(one_of(vars2), everything())) %>% na.omit()
out1 <- NULL
out2 <- NULL
#rownames(df2) <- df2$ObsDate
#ts.stdres2 <- ts(data=rstandard(fit2))
par(old.par)
out2 <- mlreg(df = df2[vars2],
              fit = fit2,
              2)
fitco <- tsmlreg(
  df = df2[vars2],
  fit = fit2,
  num = 2,
  ts.stdres = ts.stdres2
)
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
  mutate(Qual_Score.sqrt = sqrt(Qual_Score))
# ,
# BPA_Score.sqrt = sqrt(BPA_Score),
# Fin_Score.sqrt = sqrt(Fin_Score),
# HR_Score.sqrt = sqrt(HR_Score),
# UDW_Score.sqrt = sqrt(UDW_Score))# %>% na.omit()

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
print(as.data.frame(
  cbind(
    "p-value",
    glance(fit1)$p.value,
    glance(fit2)$p.value,
    glance(fit3)$p.value
  )
))
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
