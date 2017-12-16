#Load MukEnv1.R functions and variables for this script
# Linear Regression for EBIT Growth
# Data 
# https://data.ers.usda.gov/reports.aspx?ID=14843 % of total population in poverty in Florida
# https://data.ers.usda.gov/reports.aspx?ID=18241 % of percent change in population in Florida
# https://data.ers.usda.gov/reports.aspx?ID=18242 Unempl rate and median household income
# https://data.ers.usda.gov/reports.aspx?ID=18243 Education
# https://www.huduser.gov/portal/datasets/hads/hads.html American Housing Survey: Housing Affordability Data System 
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP04&src=pt  SELECTED HOUSING CHARACTERISTICS  more information 2011-2015 American Community Survey 5-Year Estimates  "ACS_15_5YR_DP04_Housing Char.zip"
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP05&src=pt  ACS DEMOGRAPHIC AND HOUSING ESTIMATES  more information 2011-2015 American Community Survey 5-Year Estimates "ACS_15_5YR_DP04_ACS Demo and Housing.zip"
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_DP_DPDP1&src=pt  Profile of General Population and Housing Characteristics: 2010  more information 2010 Demographic Profile Data "DEC_10_DP_DPDP1 Profile General Population and Housing Charc.zip"

cat("\n\nFirst Round\n")
u <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/fundgrEB.html"
df1 <- as.data.frame(readHTMLTable(u, colClasses = c("factor","integer",rep("Percent",3), which=2, trim = TRUE)))
colnames(df1) <- substr(colnames(df1),6,50)
df1$Industry.Name <- as.factor(str_replace(df1$Industry.Name,"[\\r*]",""))
df1$Industry.Name <- as.factor(str_replace(df1$Industry.Name,"[\\t*]",""))
df1$Industry.Name <- as.factor(str_replace(df1$Industry.Name,"[\\t*]",""))
df1 <- df1 %>% filter(!str_detect(Industry.Name,"Total"))
response <- "Expected.Growth.in.EBIT"
df1 <-
  cbind(df1 %>% select_if(colnames(df1) == response),
        df1 %>% select_if(!colnames(df1) == response))
rownames(df1) <- df1$Industry.Name
fit1 <- lm(Expected.Growth.in.EBIT ~ Reinvestment.Rate, data = df1)
vars <- colnames(model.frame(fit1, df1))
out1 <- NULL
out1 <- mlreg(df = df1,
              fit = fit1,
              fr = as.formula(fit1$call[[2]]),
              1)

cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
fit2 <- lm(Expected.Growth.in.EBIT ~ Reinvestment.Rate, data = df2)
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
df3 <- df2 %>% na.omit()
#df3$Reinvestment.Rate <- df3$Reinvestment.Rate^0.40
df3 <- df3 %>% mutate(Expected.Growth.in.EBIT.log=log(Expected.Growth.in.EBIT+5), 
              Reinvestment.Rate.log = log(Reinvestment.Rate+5)) %>% na.omit()
df3 <- subset(df3, !(row.names(df3) %in% out3)) # For iterations
fit3 <- lm(Expected.Growth.in.EBIT.log ~ Reinvestment.Rate.log, data = df3)
vars <- colnames(model.frame(fit3, df3))
out3 <- NULL
out3 <- mlreg(df = df3,
              fit = fit3,
              fr = as.formula(fit3$call[[2]]),
              3)
powerT(df = df3,
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
