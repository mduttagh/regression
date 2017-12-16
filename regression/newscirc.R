# Initialize Libraries
packages <-
  c(
    "GGally",
    "corrplot",
    #Corr Scatterplot
    "PerformanceAnalytics",
    #Corr Scatterplot
    "RColorBrewer",
    "car",
    #Corr scatterplot, scatterplotMatrix
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
    "dplyr",
    # data manipulation - select, filter, group_by, summarise, arrange, join, mutate
    "broom" #tidy(lmfit)
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
news <- read.delim(file.choose())

describe(news, IQR = TRUE)
stat.desc(news, norm = TRUE, p = 0.95)
scatterplotMatrix(news)

shapiro.test(news$Daily)
shapiro.test(news$Sunday)

ggscatter(
  data = news,
  x = "Daily",
  y = "Sunday",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Sunday vs Daily"
)

ggscatter(
  data = news,
  x = "Daily",
  y = "Sunday",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Sunday vs Daily",
  label = "Newspaper",
  repel = TRUE
)
newslm <- lm(Sunday ~ Daily, data = news)
newsaov <- aov(newslm)
summary(newslm)
tidy(newslm)
augment(newslm)
glance(newslm)
summary(newsaov)
autoplot(newslm,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()

confint(newslm, levels = 0.95)
predict(
  newslm,
  newdata = data.frame(Daily = 600L),
  interval = 'confidence',
  se.fit = TRUE
)
predict(
  newslm,
  newdata = data.frame(Daily = 600L),
  interval = 'prediction',
  se.fit = TRUE
)
predict(
  newslm,
  newdata = data.frame(Daily = 2500L),
  interval = 'confidence',
  se.fit = TRUE
)
predict(
  newslm,
  newdata = data.frame(Daily = 2500L),
  interval = 'prediction',
  se.fit = TRUE
)