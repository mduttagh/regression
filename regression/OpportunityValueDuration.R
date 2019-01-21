cat("\n\nFirst Round\n")
old.par <- par()
ifelse (!exists("mlreg", mode = "function"),
        source("MukEnv2.R"),
        "MukEnv2.R already exists")
df <- read.csv("~/Oppor Value Duration.csv")
View(df)
#CorrCoef <- filter (CorrCoef, CorrCoef$MaxDate != "")
#View(df)
df1 <- na.omit(df)
fit1 <- lm(ProjectDurationLog ~ CurrencyCode + OpportunityValueLog, data = df1)
out1 <- NULL
out1 <- mlreg(df = df1,
              fit = fit1,
              1)
fit1
cat("\n\nTidy Regression Model \n")
print(tidy(fit1, conf.int = TRUE))
df2 <- df1
out2 <- out1
df2 <- subset(df2, !(row.names(df2) %in% out2))
fit2 <- lm(ProjectDurationLog ~ CurrencyCode + OpportunityValueLog, data = df2)
out2 <- NULL
out2 <- mlreg(df = df2,
              fit = fit2,
              2)
fit2
cat("\n\nTidy Regression Model \n")
print(tidy(fit2, conf.int = TRUE))
powerT(df = df2,
       fit = fit2,
       2)


#Auto ML using automl package-----------------------------------


install.packages("automl")
require("automl")
vars1 <- colnames(model.frame(fit1, df1))
dfrn <- df1 %>% mutate_if(is.factor, as.numeric) %>% na.omit()
xmat = as.data.frame (dfrn[,1:2])
ymat =  as.data.frame (dfrn[,"ProjectDuration"])

#1 ---- fit a regression model automatically (easy way, Mix 1)

amlmodel <- automl_train(Xref = xmat, Yref = ymat,
                         autopar = list(psopartpopsize = 15,
                                        numiterations = 5,
                                        auto_layers_max = 1,
                                        nbcores = 4))
amlmodel
res <- cbind(ymat, automl_predict(model = amlmodel, X = xmat))
colnames(res) <- c('actual', 'predict')
head(res)


#2 fit a regression model experimentally (experimental way, Mix 2)
amlmodel <- automl_train_manual(Xref = xmat, Yref = ymat,
                                hpar = list(modexec = 'trainwpso',
                                            numiterations = 30,
                                            psopartpopsize = 50))
amlmodel
res <- cbind(ymat, automl_predict(model = amlmodel, X = xmat))
colnames(res) <- c('actual', 'predict')
head(res)

#Auto ML using H2o automl package-----------------------------------
install.packages("h2o")
require("h2o")
h2o.init()

#Convert to h2o frame
df1_h2oframe <- as.h2o(df1[1:3])

#Split data into Train/Validation/Test Sets
split_h2o <- h2o.splitFrame(df1_h2oframe, c(0.6, 0.2), seed = 1234 )
train_df1_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 60%
valid_df1_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 20%
test_df1_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 20%
#Model
# Set names for h2o
target <- "ProjectDuration"
predictors <- setdiff(names(train_df1_h2o), target)

# Run the automated machine learning 
automl_h2o_models <- h2o.automl(
  x = predictors, 
  y = target,
  training_frame    = train_df1_h2o,
  leaderboard_frame = valid_df1_h2o
)

# Extract leader model
automl_leader <- automl_h2o_models@leader
print(automl_h2o_models@leaderboard,n = nrow(automl_h2o_models@leaderboard))
automl_leader
print(automl_leader, n = nrow(automl_leader))  # Print all rows instead of default (6 rows)


# Predict on hold-out test set
pred_conversion <- h2o.predict(object = automl_leader, newdata = test_df1_h2o)
pred <- as.data.frame(pred_conversion)
pred
#Confusion matrix on test data set
h2o.table(pred_conversion$predict, test_df1_h2o$converted)


#compute performance
perf <- h2o.performance(automl_leader,df1_h2oframe )
h2o.confusionMatrix(perf)
h2o.accuracy(perf)
h2o.tpr(perf)


# Close cluster
h2o.shutdown()
