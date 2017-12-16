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
    "dplyr" # data manipulation - select, filter, group_by, summarise, arrange, join, mutate
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
ppp <- read.csv(file.choose())
#pppt <- as_tibble(ppp) #tibble data frame

describe(ppp,IQR=TRUE)
stat.desc(ppp, norm=TRUE,p=0.95)
scatterplotMatrix(ppp)

shapiro.test(ppp$Inflation.difference)
shapiro.test(ppp$Exchange.rate.change)
shapiro.test(ppp$Developed)

ggscatter(
  data = ppp,
  x = "Inflation.difference",
  y = "Exchange.rate.change",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Exchange Rate vs Inflation Difference"
)

ggscatter(
  data = ppp,
  x = "Inflation.difference",
  y = "Exchange.rate.change",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Exchange Rate vs Inflation Difference",
  label = "Country",
  repel = TRUE
)
ppplm <- lm(Exchange.rate.change ~ Inflation.difference, data = ppp)
pppaov <- aov(ppplm)
summary(ppplm)
summary(pppaov)
autoplot(ppplm,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()
ppp %>% filter(row_number() %in% c(31,17))
pppa <- ppp %>% filter(!Country %in% c("Brazil", "Mexico"))
describe(pppa,IQR=TRUE)
stat.desc(pppa, norm=TRUE,p=0.95)
scatterplotMatrix(pppa)
shapiro.test(pppa$Inflation.difference)
shapiro.test(pppa$Exchange.rate.change)
shapiro.test(pppa$Developed)
ggscatter(
  data = pppa,
  x = "Inflation.difference",
  y = "Exchange.rate.change",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE,
  main = "Scatterplot of Exchange Rate vs Inflation Difference"
)
ppplma <- lm(Exchange.rate.change ~ Inflation.difference, data = pppa)
pppaova <- aov(ppplma)
summary(ppplma)
summary(pppaova)
# Autoplot with categories
autoplot(ppplma, data = pppa, colour = "Developed",
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()

autoplot(ppplma,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()

#Split data by Developed Countries
pppa1 <- pppa %>% filter(Developed==1)
pppa0 <- pppa %>% filter(Developed==0)
scatterplotMatrix(pppa1[1:3])
shapiro.test(pppa1$Inflation.difference)
shapiro.test(pppa1$Exchange.rate.change)
ggscatter(
  data = pppa1,
  x = "Inflation.difference",
  y = "Exchange.rate.change",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE, rug=TRUE,label="Country",repel=TRUE,
  main = "Scatterplot of Exchange Rate vs Inflation Difference \nfor Developed Countries"
)
ppplma1 <- lm(Exchange.rate.change ~ Inflation.difference, data = pppa1)
pppaova1 <- aov(ppplma1)
summary(ppplma1)
summary(pppaova1)
autoplot(ppplma1,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()

ggscatter(
  data = pppa0,
  x = "Inflation.difference",
  y = "Exchange.rate.change",
  color = "black",
  shape = 21,
  size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE,
  cor.coef = TRUE, rug=TRUE,label="Country",repel=TRUE,
  main = "Scatterplot of Exchange Rate vs Inflation Difference \nfor Developing Countries"
)

scatterplotMatrix(pppa0[1:3])
shapiro.test(pppa0$Inflation.difference)
shapiro.test(pppa0$Exchange.rate.change)