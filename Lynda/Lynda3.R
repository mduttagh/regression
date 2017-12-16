# Survey Analysis
# Original Data "IT - TOS CSS Nov 2015 Prod_September 2, 2016_13.09 - Mapped ToLoad - WITH FORMULA.csv"

# Initialize Libraries
packages <-
  c(
    "GGally", "corrplot",
    #Corr Scatterplot
    #"Hmisc", # stats description
    "PerformanceAnalytics",
    #Corr Scatterplot
    "RColorBrewer",
    "SnowballC",
    "car",
    #Corr scatterplot, scatterplotMatrix
    "foreign",
    "ggfortify",
    # autoplot
    "ggpubr",
    "gmodels",
    # CrossTable()
    "lda",
    #"plyr",# Mapvalues
    "psych",
    # stats description
    "tidyr",
    # gather, spread, separate, unite
    "tm",
    "topicmodels",
    "wordcloud",
    "tidyverse",
    "pastecs",
    # stat.desc
    "dplyr" # data manipulation - select, filter, group_by, summarise, arrange, join, mutate
    #"devtools","easyGgplot2"
  )
#sapply(packages, install.packages)
#sapply(packages, require, character.only = TRUE)
#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      require(x, character.only = TRUE)
    }
  }
)
#install_github("kassambara/easyGgplot2")
#verify they are loaded
#search()

# Working solution to load Survey Results "Survey.csv"
# First Save Qlattrics data as CSV
survey <- read.csv(file.choose())
str(survey)



# Data Manipulation
require(dplyr)
require(tidyverse)
surveyt <- as_tibble(survey) #convert it to tibble
surveyt

# Describe stats
summary(surveyt$Finished)
View(stat.desc(surveyt, norm = TRUE))
View(describe(surveyt, quant = c(.1, .25, .5, .75, .90), IQR = TRUE))

# Select & Remove survey finished rows
surveyt <- filter(surveyt, Finished == TRUE)
#survey[survey$Finished == TRUE, ]
glimpse(surveyt)
#grep("Others", cn)
#names(survey[, grep("Others", cn)])


# Initialize Variables
#likert_lvl <-
#  c(1:5)
likert_lbl <-
  c("Very Dissatisfied",
    "Dissatisfied",
    "Neutral",
    "Satisfied",
    "Very Satisfied")

#Format Variables

#Format Date columns
surveyt1 <- surveyt #Copy original
glimpse(surveyt1)
#select(surveyt1,everything()) # selects all columns
#dateCols <- grep("Date", names(surveyt1))

surveyt1 <- surveyt1 %>% select(everything()) %>%
  mutate_at(vars(dplyr::contains('Date')), as.Date, format = "%m/%d/%Y")

#survey1[, dateCols] <-
#  lapply(survey1[, dateCols], as.Date, format = "%m/%d/%Y") #Format date in YYYY-MM-DD format

# Format Rate factors columns
rateCols <- grep("Rate", names(surveyt1))
surveyt1[, rateCols] <-
  lapply(surveyt1[, rateCols], factor, likert_lbl, ordered = TRUE)

#surveyt1 <- surveyt1 %>%
#  select(everything()) %>%
#  mutate_at(vars(dplyr::contains("Rate")), factor, likert_lbl, ordered = TRUE)

# Format columns with Text
#textCols <- grep("Text", names(survey1))
#survey1[, textCols] <-
#  lapply(survey1[, textCols], as.character)
surveyt1 <- surveyt1 %>%
  select(everything()) %>%
  mutate_at(vars(dplyr::contains("Text")), as.character)

# Recode Full UDAR level to short UDAR Level
surveyt1$DivSch <-
  recode(surveyt1$DivSch,
         "University Development and Alumni Relations (UDAR)" = "UDAR")

surveyt1 %>% select(dplyr::contains('Date'),
                    dplyr::contains("Rate"),
                    dplyr::contains("Text"),
                    DivSch)

#Split MajorApps field into new columns
View(surveyt1)
surveyt1 <- surveyt1 %>%
  mutate(
    BPA = ifelse(grepl("BPA", MajorApps), TRUE, NA),
    Fin = ifelse(grepl("Financial", MajorApps), TRUE, NA),
    FundR = ifelse(grepl("Fund Raising", MajorApps), TRUE, NA),
    HR = ifelse(grepl("HR", MajorApps), TRUE, NA),
    Others = ifelse(grepl("Others", MajorApps), TRUE, NA),
    PubSaf = ifelse(grepl("Public Safety", MajorApps), TRUE, NA),
    SIS = ifelse(grepl("SIS", MajorApps), TRUE, NA),
    UDW = ifelse(grepl("UDW", MajorApps), TRUE, NA)
  )
tail(surveyt1)
#MajorApps1 <- strsplit(as.character(surveyt1$MajorApps), ",")
# survey1$BPA[grep("BPA", MajorApps1)] <-
#   ifelse(grep("BPA", MajorApps1), TRUE, NA)
# survey1$Fin[grep("Financial", MajorApps1)] <-
#   ifelse(grep("Financial", MajorApps1), TRUE, NA)
# survey1$FundR[grep("Fund Raising", MajorApps1)] <-
#   ifelse(grep("Fund Raising", MajorApps1), TRUE, NA)
# survey1$HR[grep("HR", MajorApps1)] <-
#   ifelse(grep("HR", MajorApps1), TRUE, NA)
# survey1$Others[grep("Others", MajorApps1)] <-
#   ifelse(grep("Others", MajorApps1), TRUE, NA)
# survey1$PubSaf[grep("Public Safety", MajorApps1)] <-
#   ifelse(grep("Public Safety", MajorApps1), TRUE, NA)
# survey1$SIS[grep("SIS", MajorApps1)] <-
#   ifelse(grep("SIS", MajorApps1), TRUE, NA)
# survey1$UDW[grep("UDW", MajorApps1)] <-
#   ifelse(grep("UDW", MajorApps1), TRUE, NA)
str(surveyt1$BPA)

# Select Rate Columns
require(dplyr)
sdfr <-  as_tibble(surveyt1 %>%
  select(Recorded_Date,
         DistribChnl,
         DivSch,
         Role,
         BPA:UDW,
         dplyr::contains("Rate")))

str(sdfr)
tail(sdfr)
describe(sdfr, quant = c(.1, .25, .5, .75, .90), IQR = TRUE)
# Calculate Scores
#Function to convert factor to numeric
facToNum <- function(x) {
  as.numeric(unclass(x))
}

sdfr1 <- sdfr
group.names <- as.array(names(select(surveyt1,BPA:UDW)))
group.names[9] <- "Satis"
View(group.names)
# Convert columns to numbers
for (x in group.names) {
  y <- paste("Rate_",x,sep = "")
  sdfr1 <- sdfr1 %>% mutate_at(vars(dplyr::contains(y)), facToNum)
  print(x)
  print(y)
}
rm(x,y,z,sdfr2)
glimpse(sdfr1)
# Calculate Column Mean Scores
for (x in group.names) { 
  y <- paste("Rate_",x,sep = "")
  z <- paste(x,"_Score",sep = "")
  scoreCols <- grep(y, names(sdfr1))
  sdfr1[[z]] <-
    rowMeans(sdfr1[, scoreCols], na.rm = TRUE)
  print(x)
  print(y)
  print(z)
}
glimpse(sdfr1)
rm(x,y,z,sdfr2)

#sdfr1 <- sdfr1 %>% select(dplyr::contains("Rate_Fin")) %>% mutate(Fin_Score = rowMeans(.,na.rm = TRUE))
quality.names <- c("Confor","Reliab","Assur","Partner","Respons")
for (x in quality.names) { 
  z <- paste(x,"_Score",sep = "")
  scoreCols <- grep(x, names(sdfr1))
  sdfr1[[z]] <-
    round(rowMeans(sdfr1[, scoreCols], na.rm = TRUE),1)
  print(x)
  print(z)
}
rm(x,y,z)
glimpse(sdfr1)
sdfr1 <- sdfr1 %>% 
  mutate(Qual_Score = select(.,one_of("Confor_Score","Reliab_Score","Assur_Score","Partner_Score","Respons_Score")) %>% rowMeans(na.rm = TRUE)) 
glimpse(sdfr1)
# sdfr1 <- sdfr1 %>%
#   mutate(BPA_Score = select(.,dplyr::ends_with("Rate_BPA")) %>% rowMeans(na.rm = TRUE))  %>%
#   mutate(Confor_Score = select(., ends_with("_Confor")) %>% rowMeans(na.rm = TRUE)) %>% 
#   mutate(Qual_Score = select(.,one_of("Confor_Score","Reliab_Score","Assur_Score","Partner_Score","Respons_Score")) %>% rowMeans(na.rm = TRUE)) 
# scoreCols <- grep("Rate_BPA", names(sdfr))
# sdfr$BPA_Score <-
#   rowMeans(data.frame(lapply(sdfr[, scoreCols], facToNum)), na.rm = TRUE)


#Analysis using pysch and dplyr package
# Summarizing explanatory variable
require(psych)
describe(sdfr1, quant = c(.1, .25, .5, .75, .90), IQR = TRUE)
require(pastecs)
stat.desc(sdfr1)

#WIP
#Scatterplot Matrix
require(GGally)

require("PerformanceAnalytics")
NumCols <- sdfr1 %>% select(Satis_Score:Qual_Score)

ggpairs(NumCols)
chart.Correlation(NumCols, histogram = TRUE, pch = 19)

ggscatmat(NumCols,columns = c(1:ncol(NumCols)))
ggcorr(NumCols,label = TRUE,
       label_alpha = TRUE)

require(car)
scatterplot(Satis_Score ~ Qual_Score, data = NumCols)
scatterplotMatrix(NumCols)

#Using Correlogram
M <- cor(NumCols)
corrplot(M, method="pie")
corrplot(M, method="number")

mean.sdfr1 <- sdfr1 %>% 
  summarise_if(is.numeric, funs(count = n(), mean = mean(.,na.rm=TRUE)))
glimpse(mean.sdfr1)

mean.SatisScore.DivSch <- sdfr1 %>%   
  group_by(DivSch) %>% 
  summarise_if(is.numeric, funs(count = n(), mean = mean(.,na.rm=TRUE)))
glimpse(mean.SatisScore.DivSch)

# ?describeBy
# descSatisDivSch <- data.frame(describeBy(
#   Satis_Score,
#   group = DivSch,
#   mat = TRUE,
#   digits = 3,
#   na.rm = TRUE
# ))
# descQualDivSch <- describeBy(
#   Qual_Score,
#   group = DivSch,
#   mat = TRUE,
#   digits = 3,
#   na.rm = TRUE
# )


#Plot charts using ggpubr
require(ggpubr)
glimpse(sdfr1)

p <- gghistogram(data=sdfr1, x="Rate_Satis", add="mean", rug = TRUE, bins = 8)
ggpar(p, main = "Satisfaction Score", xlab = "Satisfaction Rate",ylab =  "Freq") + theme_minimal()
p <- NULL

p <- ggbarplot(data=as.data.frame(table(sdfr$Rate_Satis)), x="Var1", y="Freq", fill = "steelblue")
ggpar(p, main = "Satisfaction Score", xlab = "", ylab =  "Freq") + theme_minimal()

p <- ggplot(sdfr, aes(x=Rate_Satis)) + geom_bar()
p

p <- ggbarplot(data=as.data.frame(sdfr1), x="DivSch", y="Rate_Satis", group = "DivSch")
ggpar(p, main = "Satisfaction Score", xlab = "Satisfaction Rate",ylab = "Count", orientation = "horizontal") + theme_minimal()

p <- ggbarplot(data=as.data.frame(table(sdfr$DivSch)), x="Var1", y="Freq", fill = "steelblue")
ggpar(p, main = "Count by Div/School", xlab = "Division School",ylab = "Count", orientation = "horizontal") + theme_minimal()

p <- gghistogram(data=sdfr1, x="Satis_Score", binwidth = 1, addMeanLine = TRUE, meanLineColor = "blue", meanLineType = "solid", meanLineSize = 2)
ggpar(p, main = "Satisfaction Score") + theme_minimal()

p <- ggboxplot(data=as.data.frame(sdfr1),y="Satis_Score", x=1)
ggpar(p, main = "Satisfaction Score Boxplot", xlab = "") + theme_minimal()

p <- ggboxplot(data=as.data.frame(sdfr1), x = "DivSch", y="Satis_Score", group = "DivSch")
ggpar(p, main = "Satisfaction Score by DivSch", orientation = "horizontal") + theme_minimal()

p <- ggdensity(data=sdfr1, x="Qual_Score")
ggpar(p, main = "Quality Score", xlab = "Qulaity Score",ylab = "Density") + theme_minimal()

p <- ggdensity(data=sdfr1, x="Qual_Score",group = "DivSch")
ggpar(p, main = "Quality Score", xlab = "Qulaity Score",ylab = "Density") 

#Add the 95% confidence region
p <- ggscatter(data = sdfr1, x='Qual_Score',y='Satis_Score')
ggpar(p, main = "Regression Line", xlab = "Qulaity Score",ylab = "Satisfaction Score")  

df1 <- sdfr1 %>%   
  group_by(DivSch) %>% 
  summarise_if(is.numeric, funs(count = n(), mean = mean(.,na.rm=TRUE)))
glimpse(df1) 



g <- scoreCols[1:8]
bxg <-
  boxplot(
    sdfr[, g],
    col = "lavender",
    main = "Satisfaction IT Group Scores",
    col.main = "#3366CC",
    xlab = "TOS Groups",
    ylab = "Score",
    col.lab = "blue",
    cex.lab = 1
  )
bxg
q <- scoreCols[9:15]
bxq <-
  boxplot(
    sdfr[, q],
    col = "lavender",
    main = "Satisfaction Quality Scores",
    col.main = "#3366CC",
    cex.main = 2,
    xlab = "Quality Dimensions",
    ylab = "Score",
    col.lab = "blue",
    cex.lab = 1.5
  )
bxq
bp <-
  barplot(
    table(Rate_Satis),
    col = "lavender",
    main = "Satisfaction Rate",
    col.main = "#3366CC",
    cex.main = 2,
    xlab = "Scale",
    ylab = "Frequency",
    col.lab = "blue",
    cex.lab = 1.5,
    ylim = c(0, 60)
  )
#abline(v=median(4),col="red4",lwd=2) #To draw veritical line
bp
text(
  bp,
  table(Rate_Satis),
  labels = table(Rate_Satis),
  pos = 1,
  cex = 1
)
?barplot
bp1 <-
  barplot(
    summary(DivSch),
    col = "lavender",
    main = "TOS Groups Counts",
    col.main = "#3366CC",
    cex.main = 2,
    xlab = "TOS Groups",
    ylab = "Frequency",
    col.lab = "blue",
    cex.lab = 1.5,
    ylim = c(0, 60)
  )

text(
  bp1,
  summary(DivSch),
  labels = summary(DivSch),
  pos = 3,
  cex = 1
)

#Plots using ggplot2 pacakge
require(ggplot2)
#qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, facets=, xlim=, ylim= xlab=, ylab=, main=, sub=)
colors()[grep("white", colors())] #Find color names of interest
GetColorHexAndDecimal("white")

q <- qplot(
  Rate_Satis,
  data = sdfr,
  main = "Satisfaction Rate",
  xlab = "Scale",
  ylab = "Frequency"
) +  geom_bar(fill = "cornflowerblue") + coord_flip() + theme_minimal()
q

qplot(
  DivSch,
  data = sdfr,
  main = "Div/School Counts",
  xlab = "Division_School",
  ylab = "Frequency"
)  +  geom_bar(fill = "cornflowerblue") + coord_flip() + theme_minimal()

?ggplot
?geom_text
ggplot(data = sdfr, aes(x = Rate_Satis)) +
  geom_bar(fill = "cornflowerblue") +
  scale_x_discrete(drop = FALSE) +
  theme_minimal()

#geom_text
dat <- data.frame(table(sdfr$Rate_Satis))
str(dat)
View(dat)
?ggplot
#Vertical
g <- ggplot(data = dat, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity",
    fill = "cornflowerblue",
    aes(y = Freq,
        ymax = Freq),
    position = "dodge"
  )  +
  geom_text(
    aes(
      x = Var1,
      y = Freq,
      ymin = -0.5,
      ymax = Freq,
      label = Freq,
      vjust = 1.25
    ),
    position = position_dodge(width = 1)
  ) +
  theme_minimal() +
  scale_y_continuous(labels = waiver()) + ggtitle("Satisfaction Score") + xlab("Likert Scale") +   ylab("Frequency") +   theme(text = element_text(size = 16))
g
#Horizontal
g <- ggplot(data = dat, aes(x = Var1, y = Freq)) +
  geom_bar(
    stat = "identity",
    fill = "cornflowerblue",
    aes(y = Freq,
        ymax = Freq),
    position = "dodge"
  )  +
  geom_text(aes(
    #x = Var1,
    y = Freq,
    ymin = -0.25,
    ymax = Freq,
    label = Freq,
    hjust = 1.25
  ),
  position = position_dodge(width = 1)) +
  theme_minimal() +
  scale_y_continuous(labels = waiver()) + ggtitle("Satisfaction Score") + xlab("Likert Scale") +  ylab("Frequency") + coord_flip() +
  theme(text = element_text(size = 16))
g

# Score Bars by Div_School
attach(sdfr)
require(plyr)
sdf_mean <-
  ddply(
    sdfr,
    "DivSch",
    summarize,
    divsch_mean = round(mean(Satis_Score, na.rm = TRUE), 2),
    qual_mean = round(mean(Qual_Score, na.rm = TRUE), 2)
  )
grp1 <-
  ggplot(data = sdf_mean, aes(x = DivSch, y = divsch_mean, fill = DivSch)) +
  geom_bar(stat = "identity",
           fill = "cornflowerblue",
           position = "dodge")  +
  geom_text(aes(label = divsch_mean,
                hjust = 1.25),
            position = position_dodge(width = 1)) +
  theme_minimal() +
  scale_y_continuous(labels = waiver()) + ggtitle("Satisfaction Score by Division School") + xlab("Division-School") +  ylab("Score") + coord_flip() +
  theme(text = element_text(size = 16))
grp1
grp2 <-
  ggplot(data = sdf_mean, aes(x = DivSch, y = qual_mean, fill = DivSch)) +
  geom_bar(stat = "identity",
           fill = "cornflowerblue",
           position = "dodge")  +
  geom_text(aes(label = qual_mean,
                hjust = 1.25),
            position = position_dodge(width = 1)) +
  theme_minimal() +
  scale_y_continuous(labels = waiver()) + ggtitle("Quality Score by Division School") + xlab("Division-School") +  ylab("Score") + coord_flip() +
  theme(text = element_text(size = 16))
grp2

# Checking the normality and hypothesis testing
# Null Hypothesis Mean or Median >=4
# Alternate Hypothesis Mean or Median < 4
m <- c(4)
str(m)
for (i in scoreCols)   {
  print(names(sdfr[i]))
  print(shapiro.test(sdfr[, i]))
  summary(sdfr[, i])
  boxplot(
    sdfr[, i],
    col = "lavender",
    main = paste("Boxplot of", names(sdfr[i])),
    col.main = "#3366CC"
  )
  print("Outlier")
  print(boxplot.stats(i)) #Outliers
  plot(sdfr[, i])
  print(
    wilcox.test(
      sdfr[, i],
      y = NULL,
      alternative = "l",
      mu = m,
      paired = FALSE,
      exact = FALSE,
      correct = FALSE,
      conf.int = TRUE,
      conf.level = 0.95
    )
  )
  print(
    t.test(
      sdfr[, i],
      y = NULL,
      alternative = "l",
      mu = m,
      paired = FALSE,
      exact = FALSE,
      correct = FALSE,
      conf.int = TRUE,
      conf.level = 0.95
    )
  )
}

#Text Mining
libs2 <-
  c("tm", "wordcloud", "topicmodels", "SnowballC", "lda")
#sapply(libs2, install.packages)
sapply(libs2, require, character.only = TRUE)
# load documents and vocabulary
# Select Columns start with Text
sdft <-
  survey1 %>%   select(Recorded_Date, starts_with("Text"))
str(sdft)

# TRANSFORM TEXT TO CORPUS
CorpusText <- Corpus(DataframeSource(sdft))
inspect(CorpusText)
summary(CorpusText)
writeLines(as.character(CorpusText[1:2]))
toSpace <-
  content_transformer(function (x , pattern)
    gsub(pattern, " ", x))
#Transform changing letters to lower case, removing punctuaations etc.
getTransformations()
#create the toSpace content transformer
#toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
# Text transformation function
txttrans = function(text) {
  text = tm_map(text, removeNumbers)
  text = tm_map(text, removePunctuation)
  text = tm_map(text, content_transformer(tolower))
  text = tm_map(text, content_transformer(removeNumbers))
  text = tm_map(text, removeWords, stopwords("english"))
  text = tm_map(text, content_transformer(stripWhitespace))
  text
}
CorpusText <- txttrans(CorpusText)
#CorpusText <- tm_map(CorpusText,toSpace,"/")
#CorpusText <- tm_map(CorpusText,toSpace,"@")
#CorpusText <- tm_map(CorpusText,toSpace,"\\|")
# STEMMING
CorpusTextDict <- CorpusText
CorpusText <- tm_map(CorpusText, stemDocument)
writeLines(as.character(CorpusText))
stemCompletion2 <- function(x, dictionary)
{
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary = dictionary)
  x <- paste(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}

CorpusText1 <-
  lapply(CorpusText, stemCompletion2, dictionary = CorpusTextDict)
CorpusText <- Corpus(VectorSource(CorpusText1))
#CorpusText <- tm_map(CorpusText,PlainTextDocument)
writeLines(as.character(CorpusText1))
str(CorpusText)

#DOCUMENT TERM MATRIX
dtm <- DocumentTermMatrix(CorpusText)
dtm2 <- removeSparseTerms(dtm, 0.95)
inspect(dtm2)
dim(dtm2)

writeLines(as.character(dtm2))
#source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

plot(dtm2,
     terms = findFreqTerms(dtm2, lowfreq = 16),
     corThreshold = 0.30)

#FREQUENT TERMS AND ASSOCIATIONS
findFreqTerms(dtm2, lowfreq = 16)
termFrequency <- colSums(as.matrix(dtm2))
termFrequency <- subset(termFrequency, termFrequency > 10)
View(termFrequency)
str(termFrequency)
df <- data.frame(term = names(termFrequency), freq = termFrequency)
View(df)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
  xlab("Terms") + ylab("Count") + coord_flip() + theme_minimal() +
  geom_text(aes(label = freq,
                hjust = 1.25),
            position = position_dodge(width = 1))

#WORDCLOUD
m <- as.matrix(dtm2)
wordfreq <- sort(colSums(m), decreasing = TRUE)
head(wordfreq, 10)
wordcloud(
  names(wordfreq),
  wordfreq,
  min.freq = 2,
  max.word = Inf,
  random.order = FALSE,
  colors = brewer.pal(9, "PuBuGn"),
  rot.per = 0
)

#CLUSTER
#install.packages("wordnet")
#require(wordnet)

dim(dtm2)
d <- dist(as.matrix(scale(t(dtm2))))
hc <- hclust(d, method = "ward.D")
plot(hc)
groups <- cutree(hc, k = 10)
rect.hclust(hc, k = 10)


# LDA COLLAPSED GIBBS
#require(reshape2)
#install.packages("lda")
#install.packages("topicmodels")
require(lda)
require(topicmodels)

dtm2 <- removeSparseTerms(dtm, 0.95)
inspect(dtm2)
rowTotals <- apply(dtm2, 1, sum)
dtm2 <- dtm2[rowTotals > 0,] # remove rows with rowTotal = 0
inspect(dtm2)

# FIT THE GIBBS MODEL
# tokenize on space and output as list
set.seed(2016)
K <- 25 # topics become more coherent with more iterations

# Rather than use canned stopword list, try doing it by tf-idf
# Median word appears 3 times, mean is 107.2
summary(slam::col_sums(dtm2))
term_tfidf <-
  tapply(dtm2$v / slam::row_sums(dtm2)[dtm2$i], dtm2$j, mean) *  # Don't need this, can use weightTfIdf function)
  log2(nDocs(dtm) / slam::col_sums(dtm2 > 0))
summary(term_tfidf)
dim(dtm) #

dtm2 <-
  dtm2[, term_tfidf >= 0.024] # This is arbitrary, iteratively check
dim(dtm2) #
(text <- dtm2ldaformat(dtm2, omit_empty = FALSE))

system.time(
  result <- lda.collapsed.gibbs.sampler(
    text$documents,
    K,
    text$vocab,
    1000,
    alpha = 50 / K,
    eta = 200 / ncol(dtm2),
    compute.log.likelihood = TRUE
  )
)
topwords <-
  top.topic.words(result$topics, 10, by.score = TRUE) # 10 most likely words for each topic
p_topic <-
  as.vector(result$topic_sums / sum(result$topic_sums)) # Distribution of topics across Corpus
topdocs <-
  top.topic.documents(result$document_sums) # Most likely documents for each topic

names(result)
dim(result$assignments) # For each document, vector of integers with topic assignment per word
dim(result$topics) # Per topic word assignments --
dim(result$topic_sums) # Number of times words assigned to each topic
result$topic_sums / sum(result$topic_sums) # distribution of topic assignments
dim(result$document_sums) # topic x document matrix -- number of times words in each document assigned to a topic

# Lots of great matrix manipulation you can do with these to create frequency distributions, incorporate metadata, etc.
# For instance, topics by date or by author

rowslist <- row.names(dtm2)
Rec_Date <- sdft[rowslist,]$Recorded_Date
Rec_Data <- na.exclude(Rec_Date)

# Attach dates to doc x topic matrix
doc_sums_t_date <- data.frame(Rec_Date, t(result$document_sums))
colnames(doc_sums_t_date) <-
  c("date", paste("topic", seq(1:K), sep = ""))
articles_by_date <- ddply(doc_sums_t_date, .(date), nrow)
tail(articles_by_date)

qplot(x = articles_by_date$date,
      y = articles_by_date$V1,
      geom = "smooth") + xlab("Date") + ylab("Number of articles")

qplot(x = articles_by_date$date,
      y = articles_by_date$V1,
      geom = "smooth")
+ xlab("Date") + ylab("Number of articles")

qplot(x = articles_by_date$date,
      y = articles_by_date$V1,
      geom = "smooth")
+ xlab("Date") + ylab("Number of articles")


qplot(x = date,
      y = topic1,
      data = doc_sums_t_date,
      geom = "smooth")

# TOPIC MODELING

k <- 10 #find 10 topics
lda <- LDA(dtm2, control = list(alpha = 0.2, nstart = 100), k)
topics(lda)
str(topics(lda))
term <- terms(lda, 6) # first 6 terms of every topics
View(term)
term <- apply(term, MARGIN = 1, paste, collapse = ", ")
View(term)
topic <- topics(lda, 1) # first topic identified for every document
str(topic)
names(topic)

rowslist <- row.names(dtm2)
Rec_Date <- sdft[rowslist,]$Recorded_Date
Rec_Data <- na.exclude(Rec_Date)

topicsm <- data.frame(date = Rec_Date, topic)
ggplot(topicsm,
       aes(x = date, y = topic, fill = topic),
       geom_density(alpha = 0.4))
qplot(
  date,
  ..count..,
  data = topicsm,
  geom = "density",
  fill = term[topic],
  position = position_stack()
) + theme_minimal()


topicsm <- na.exclude(data.frame(t = term[topic], topic))
topicsm <- arrange(topicsm, desc(topic))
View(topicsm)

g <-
  ggplot(data = topicsm, aes(x = t, y = topic)) + geom_bar(stat = "identity",
                                                           position = position_dodge(),
                                                           fill = "cornflowerblue") + theme_minimal() + coord_flip()
g

# Linear Regression
require(ggplot2)
fit = lm(Sales ~ Adv, data = salesadv)
summary(fit)

equation = function(x) {
  lm_coef <- list(
    a = round(coef(x)[1], digits = 2),
    b = round(coef(x)[2], digits = 2),
    r2 = round(summary(x)$r.squared, digits = 2)
  )
  lm_eq <-
    substitute(italic(y) == a + b %.% italic(x) * "," ~  ~ italic(R) ^ 2 ~
                 "=" ~ r2,
               lm_coef)
  as.character(as.expression(lm_eq))
}

p1 <- ggplot(data = salesadv, mapping = aes(x = Adv, y = Sales))
p1 + geom_point() + geom_smooth(method = "lm") + ggtitle("Sales Vs Adv Linear Regression Model") +
  annotate(
    "rect",
    xmin = 175,
    xmax = 250,
    ymin = 700,
    ymax = 750,
    fill = "white",
    colour = "red"
  ) +
  annotate(
    "text",
    x = 210,
    y = 725,
    label = equation(model1),
    parse = TRUE
  ) + theme_minimal()

library(ggfortify)
autoplot(fit,
         which = 1:6,
         ncol = 2,
         label.size = 3) + theme_minimal()

mod <- lm(mpg ~ wt, data = mtcars)
head(fortify(mod))
head(fortify(mod, mtcars))
autoplot(mod,which=1:6,ncol =2,label.size = 3) + theme_minimal()
ggscatter(data=mtcars,"wt","mpg",add=c("reg.line"))

#OutlierTest
require(car)
outlierTest(mod)
influencePlot(mod,id.method = "identify",main="Influence Plot",sub="Circle size is proporational to Cook's distance")
#End of Analysis=========================================


#Use Likert package
require(likert)
str(survey)
lik_df <- likert(data.frame(survey1[, 88]))
plot(lik_df)
likert.density.plot(lik_df)
likert.bar.plot(lik_df)
?likert.bar.plot

demo(graphics)
demo(persp)
library(lattice)
demo(lattice)


# CLEAN UP
packlibs <- paste("package", c(libs, libs2), sep = ":")
sapply(packlibs,
       detach,
       unload = TRUE,
       character.only = TRUE)
#remove.packages(libs)
#remove.packages(libs2)
rm(list = ls())


# recursively search for the conflicted files and delete them
#find . -type f -name "* conflicted copy*" -exec rm -f {} \;
# remove any "conflicted" references from git's packed-refs file
#awk '!/conflicted/' .git/packed-refs > temp && mv temp .git/packed-refs
# Remove dangling objects
# git gc --prune="0 days"

#Summary http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
require(plyr)
cdata <-
  ddply(
    data,
    .(sex, condition),
    summarise,
    N = sum(!is.na(change)),
    mean = mean(change, na.rm = TRUE),
    sd = sd(change, na.rm = TRUE),
    se = sd / sqrt(N)
  )
cdata

#Converting wide to long format http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
require(tidyr)
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
data_long <-
  gather(olddata_wide,
         condition,
         measurement,
         control:cond2,
         factor_key = TRUE)
data_long

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values
data_wide <- spread(olddata_long, condition, measurement)
data_wide
