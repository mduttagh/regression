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
df2 <- df1
out2 <- out1
df2 <- subset(df2, !(row.names(df2) %in% out2))
fit2 <- lm(ProjectDurationLog ~ CurrencyCode + OpportunityValueLog, data = df2)
out2 <- NULL
out2 <- mlreg(df = df2,
              fit = fit2,
              2)
fit2
powerT(df = df2,
       fit = fit2,
       2)
