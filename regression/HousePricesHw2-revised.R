#Load MukEnv1.R functions and variables for this script
# Linear Regression for House Prices and multi variate analysis
# Data http://www.state.nj.us/treasury/taxation/lpt/TaxListSearchPublicWebpage.shtml
# Data: http://www.realtor.com/realestateandhomes-detail/3
# Data: https://www.zillow.com/homes/for_sale/
# Data: http://taxlookup.njtown.net/pmod4search.aspx?cc=12
# File Name "Middlesex16.txt"
# Layout "MODIVLayout2016.pdf"
# ORG: 122500392  0200026               2000021413179940918000 2 169 BLOOMFIELD AVE       2S AL L        50 X 133            000001527                                              76DUTTA, MUKESH & PARUL              169 BLOOMFIELD AVE       ISELIN  NJ               0883022250002000000 00660          0558300320A10200500046900000002130007                  01       0000000000000000000                                16   2005 000021300000089300000110600F09          00000000 00000000 00000000 00000000000000000000000000        00392   B00026                                     N2015001119383000110600     00111938300000000{00039603B00041831F00015429G00015073H00039837I00016240A0         000110600000000000

# 1225 00392  02 00026               2000021413179940918000 2 169 BLOOMFIELD AVE       2S AL L        50 X 133            000001527                                              76DUTTA, MUKESH & PARUL              169 BLOOMFIELD AVE       ISELIN  NJ               0883022250002000000 00660          0558300320A10200500046900000002130007                  01       0000000000000000000                                16   2005 000021300000089300000110600F09          00000000 00000000 00000000 00000000000000000000000000        00392   B00026                                     N2015001119383000110600     00111938300000000{00039603B00041831F00015429G00015073H00039837I00016240A0         000110600000000000


cat("\n\nFirst Round\n")
old.par <- par()
ifelse (!exists("mlreg", mode = "function"),
        source("MukEnv2.R"),
        "MukEnv1.R already exists")
df1 <- read_fwf(
  ".//Data//Homework//Middlesex16.txt",
  fwf_positions(
    c(
      1,
      5,
      14,
      23,
      34,
      36,
      38,
      44,
      48,
      56,
      59,
      84,
      99,
      119,
      128,
      148,
      168,
      172,
      176,
      211,
      236,
      261,
      270,
      274,
      280,
      281,
      286,
      296,
      301,
      306,
      307,
      313,
      322,
      331,
      333,
      342,
      351,
      353,
      355,
      356,
      357,
      359,
      360,
      362,
      364,
      367,
      373,
      379,
      391,
      411,
      416,
      420,
      421,
      430,
      439,
      448,
      451,
      454,
      457,
      460,
      461,
      469,
      470,
      478,
      479,
      487,
      488,
      496,
      500,
      504,
      508,
      511,
      514,
      518,
      522,
      551,
      556,
      560,
      563,
      573,
      574,
      578,
      587,
      596,
      601,
      610,
      619,
      628,
      637,
      646,
      655,
      664,
      673,
      674,
      683,
      692
    ),
    c(
      4,
      13,
      22,
      33,
      35,
      37,
      43,
      47,
      55,
      58,
      83,
      98,
      118,
      127,
      147,
      167,
      171,
      175,
      210,
      235,
      260,
      269,
      273,
      279,
      280,
      285,
      295,
      300,
      305,
      306,
      312,
      321,
      330,
      332,
      341,
      350,
      352,
      354,
      355,
      356,
      358,
      359,
      361,
      363,
      366,
      372,
      378,
      390,
      410,
      415,
      419,
      420,
      429,
      438,
      447,
      450,
      453,
      456,
      459,
      460,
      468,
      469,
      477,
      478,
      486,
      487,
      495,
      499,
      503,
      507,
      510,
      513,
      517,
      521,
      550,
      555,
      559,
      562,
      572,
      573,
      577,
      586,
      595,
      600,
      609,
      618,
      627,
      636,
      645,
      654,
      663,
      672,
      673,
      682,
      691,
      700
    ),
    c(
      "COUNTY.DISTRICT",
      "BLOCK",
      "LOT",
      "QUALIFIER",
      "RECORD.ID",
      "FILLER1",
      "TRANSACTION.DATE.MMDDYY",
      "TRANSACTION.UPDATE.NO",
      "TAX.ACCOUNT.NUMBER",
      "PROPERTY.CLASS",
      "PROPERTY.LOCATION",
      "BUILDING.DESCRIPTION",
      "LAND.DESCRIPTION",
      "ACREAGE",
      "ADDITION.LOTS1",
      "ADDITION.LOTS2",
      "ZONING",
      "TAX.MAP.PAGE.NUMBER",
      "OWNER.NAME",
      "STREET.ADDRESS",
      "CITY.STATE",
      "ZIP.CODE",
      "NUMBER.OF.OWNERS",
      "DEDUCTION.AMOUNT",
      "FILLER",
      "BANK.CODE",
      "MORTGAGE.ACCOUNT.NUMBER",
      "DEED.BOOK",
      "DEED.PAGE",
      "SALES.PRICE.CODE",
      "DEED.DATE.MMDDYY",
      "SALE.PRICE",
      "SALE.ASSESSMENT",
      "SALE.SR1A.UN.CODE",
      "SOCIAL.SECURITY.NO",
      "SCHOOL.TAX.OVERAGE",
      "NO.OF.DWELLINGS",
      "NO.OF.COMMERCIAL.DW",
      "MULTIPLE.OCCUPANCY",
      "PERCENT.OWNED.CODE",
      "REBATE.CODE",
      "DELINQUENT.CODE",
      "EPL.OWN",
      "EPL.USE",
      "EPL.DESC",
      "INITIAL.DATE.MMDDYY",
      "FURTHER.DATE.MMDDYY",
      "STATUTE.NUMBER",
      "FACILITY.NAME",
      "BUILDING.CLASS.CODE",
      "YEAR.CONSTRUCTED",
      "ASSESSMENT.CODE",
      "LAND.VALUE",
      "IMPROVEMENT.VALUE",
      "NET.VALUE",
      "SPECIAL.TAX.CODE1",
      "SPECIAL.TAX.CODE2",
      "SPECIAL.TAX.CODE3",
      "SPECIAL.TAX.CODE4",
      "EXEMPTION.CODE1",
      "EXEMPTION.AMT1",
      "EXEMPTION.CODE2",
      "EXEMPTION.AMT2",
      "EXEMPTION.CODE3",
      "EXEMPTION.AMT3",
      "EXEMPTION.CODE4",
      "EXEMPTION.AMT4",
      "SENIOR.CITIZENS.CNT",
      "VETERANS.CNT",
      "WIDOWS.CNT",
      "SURV.SPOUSE.CNT",
      "DISABLED.CNT",
      "USER.FIELD1",
      "USER.FIELD2",
      "OLD.PROPERTY.ID",
      "CENSUS.TRACT",
      "CENSUS.BLOCK",
      "PROPERTY.USE.CODE",
      "PROPERTY.FLAGS",
      "REBATE.RESPONSE.FLG",
      "REBATE.BASE.YEAR",
      "REBATE.BASE.YR.TAX",
      "REBATE.BASE.YR.NET.VAL",
      "FILLER2",
      "LAST.YEAR.TAX",
      "CURRENT.YEAR.TAX",
      "NON.MUNICIPAL.HALF1",
      "NON.MUNICIPAL.HALF2",
      "MUNICIPAL.HALF1",
      "MUNICIPAL.HALF2",
      "NON.MUNICIPAL.HALF3",
      "MUNICIPAL.HALF3",
      "BILL.STATUS.FLAG",
      "ESTIMATED.QTR3.TAX",
      "PRIOR.YR.NET.VALUE",
      "STATEMENT.OF.STATE.AID.AMT"
    )
  ),
  na = c("", "NA"),
  comment = "",
  progress = interactive()
)
df1$COUNTY.DISTRICT <- as.factor(df1$COUNTY.DISTRICT)
df1$BLOCK <- as.factor(df1$BLOCK)
df1$LOT <- as.factor(df1$LOT)
df1$RECORD.ID <- as.factor(df1$RECORD.ID)
df1$TRANSACTION.UPDATE.NO <- as.factor(df1$TRANSACTION.UPDATE.NO)
df1$BUILDING.DESCRIPTION <- as.factor(df1$BUILDING.DESCRIPTION)
df1$ACREAGE <- as.numeric(df1$ACREAGE)
df1$QUALIFIER <- as.factor(df1$QUALIFIER)
df1$TRANSACTION.DATE.MMDDYY <-
  as.Date(df1$TRANSACTION.DATE.MMDDYY, "%m%d%y")
df1$DEED.DATE.MMDDYY <- as.Date(df1$DEED.DATE.MMDDYY, "%m%d%y")
df1$FURTHER.DATE.MMDDYY <-
  as.Date(df1$FURTHER.DATE.MMDDYY, "%m%d%y")
df1$INITIAL.DATE.MMDDYY <-
  as.Date(df1$INITIAL.DATE.MMDDYY, "%m%d%y")
df1$PROPERTY.CLASS <- as.factor(df1$PROPERTY.CLASS)
df1$BUILDING.CLASS.CODE <- as.factor(df1$BUILDING.CLASS.CODE)
df1$PROPERTY.USE.CODE <- as.factor(df1$PROPERTY.USE.CODE)
df1$BILL.STATUS.FLAG <- as.factor(df1$BILL.STATUS.FLAG)
df1$NO.OF.DWELLINGS <- as.factor(df1$NO.OF.DWELLINGS)
df1$REBATE.BASE.YEAR <- NULL
df1$YEAR.CONSTRUCTED <- as.numeric(df1$YEAR.CONSTRUCTED)
df1$ASSESSMENT.CODE <- as.factor(df1$ASSESSMENT.CODE)
df1$SALE.PRICE <- as.numeric(df1$SALE.PRICE)
df1$NUMBER.OF.OWNERS <- as.numeric(df1$NUMBER.OF.OWNERS)
df1$SALE.ASSESSMENT <- as.numeric(df1$SALE.ASSESSMENT)
df1$SALES.PRICE.CODE <- as.factor(df1$SALES.PRICE.CODE)
df1$LAND.VALUE <- as.numeric(df1$LAND.VALUE)
df1$IMPROVEMENT.VALUE <- as.numeric(df1$IMPROVEMENT.VALUE)
df1$NET.VALUE <- as.numeric(df1$NET.VALUE)
df1$LAST.YEAR.TAX <- as.numeric(df1$LAST.YEAR.TAX)
df1$CURRENT.YEAR.TAX <- as.numeric(df1$CURRENT.YEAR.TAX)
df1$ESTIMATED.QTR3.TAX <- as.numeric(df1$ESTIMATED.QTR3.TAX)
df1$PRIOR.YR.NET.VALUE <- as.numeric(df1$PRIOR.YR.NET.VALUE)
df1$MULTIPLE.OCCUPANCY <- as.factor(df1$MULTIPLE.OCCUPANCY)
df1$PERCENT.OWNED.CODE <- as.factor(df1$PERCENT.OWNED.CODE)
df1$ZIP.CODE <- as.factor(df1$ZIP.CODE)
# To convert currency and % to number in the data frame use "lapply(df1,parse_number)"
df0 <- df1
# Select Residential Propoerties with sale price or Last year tax >= 1000 in last few years and Actual
df1 <-
  df0 %>% filter(
    PROPERTY.CLASS %in% c("2"),
    SALE.PRICE > 10000,
    NET.VALUE > 10000,
    ACREAGE > 100,
    LAND.VALUE > 10000,
    SALE.ASSESSMENT > 10000,
    IMPROVEMENT.VALUE > 10000,
    SALES.PRICE.CODE == "A",
    COUNTY.DISTRICT == "1225",
    #BUILDING.DESCRIPTION == "2S AL L",
    #TRANSACTION.DATE.MMDDYY >= "2010-01-01",
    SALE.ASSESSMENT > NET.VALUE,
    YEAR.CONSTRUCTED >= 1980
  ) %>% select_if(function(x)
    ! all(is.na(x))) # remove columns with all NAs
#write.table(df1,file="Neighborhood.csv",row.names = FALSE, sep = "|")
#df1 <-
#  df1 %>% mutate(
#    BEDS = 0,
#    BATHS = 0,
#    FLOOR.AREA = 0
#  )
#addr1 <- df1 %>% dplyr::select(STREET.ADDRESS,CITY.STATE,ZIP.CODE,BEDS,BATHS,FLOOR.AREA)
#write.table(addr1,file="realtorSqFt.csv",row.names = FALSE, sep = "|")
# sqft <-
#   read_csv(file = ".\\Data\\Homework\\realtorSqFt.WithSqFt.csv")
# df1$BEDS <- sqft$BEDS
# df1$BATHS <- sqft$BATHS
# df1$FLOOR.AREA <- sqft$FLOOR.AREA
df1 <- df1 %>% mutate(
  FILLER1 = NULL,
  TRANSACTION.UPDATE.NO = NULL,
  TAX.ACCOUNT.NUMBER = NULL,
  FILLER2 = NULL,
  PROPERTY.CLASS = NULL,
  ADDITION.LOTS1 = NULL,
  TAX.MAP.PAGE.NUMBER = NULL,
  NUMBER.OF.OWNERS = NULL,
  DEDUCTION.AMOUNT = NULL,
  BANK.CODE = NULL,
  SALES.PRICE.CODE = NULL,
  ASSESSMENT.CODE = NULL,
  COUNTY.DISTRICT = NULL
  #LAND.VALUE = NULL,
  #IMPROVEMENT.VALUE = NULL
)
df1 <-
  as.data.frame(df1  %>% dplyr::select(
    -(SALE.SR1A.UN.CODE:STATUTE.NUMBER),
    -(SPECIAL.TAX.CODE1:STATEMENT.OF.STATE.AID.AMT)
  ) %>% select_if(function(x)
    ! all(is.infinite(x)))) 
#rownames(df1) <- paste(df1$BLOCK,df1$LOT,df1$QUALIFIER,df1$RECORD.ID)
#df1 <- sample_n(df1,500)
df1 <- df1 %>% filter(LAND.VALUE <= 40000) %>% filter(SALE.ASSESSMENT <= 300000)
out1 <- c("11","82")
df1 <- subset(df1, !(row.names(df1) %in% out1))
df1$NEW.HOME <-
  if_else(df1$YEAR.CONSTRUCTED<=median(df1$YEAR.CONSTRUCTED),FALSE,TRUE)
fit1 <-
  lm(NET.VALUE ~ ACREAGE,data = df1)
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
powerT(df = df1[vars1],
       fit = fit1,
       1)

#out1 <- c("11","82")
cat("\n\nSecond Round after Filtering Outlier\n")
df2 <- subset(df1, !(row.names(df1) %in% out1))
out2 <- c("134","47","67","142")
df2 <- subset(df2, !(row.names(df2) %in% out2)) # For iterations
df2 <-
   df2 %>% mutate(
#     NET.VALUE.log = log(NET.VALUE),
#     SALE.PRICE.log = log(SALE.PRICE),
#     #SALE.ASSESSMENT.log = log(SALE.ASSESSMENT),
#     ACREAGE.log = log(ACREAGE)
      ACREAGE.inv = 1/(ACREAGE)
   )# %>% na.omit()
#df2$SALE.ASSESSMENT.GROUP <-cut(df2$SALE.ASSESSMENT,3,labels=c("Low","Medium","Large"))
fit2 <-
  lm(NET.VALUE ~ ACREAGE.inv, data = df2)
vars2 <- colnames(model.frame(fit2, df2))
df2 <-
  as.data.frame(df2 %>% dplyr::select(one_of(vars2), everything()))
out1 <- NULL
out2 <- NULL
par(old.par)
out2 <- mlreg(df = df2[vars2],
              fit = fit2,
              2)
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


out3 <- c("46","143","12","141","80")
cat("\n\nThird Round after Best Subset Model\n")
df3 <- df2 #%>% na.omit()
#df3 <- filter(df3, df3$IMPROVEMENT.VALUE < 200000)
#df3 <- filter(df3, df3$SALE.ASSESSMENT > 55000)
df3 <-
  df3 %>% mutate(
    #SALE.PRICE.sqrt = sqrt(SALE.PRICE),
    #SALE.ASSESSMENT.sqrt = sqrt(SALE.ASSESSMENT)
    #LAND.VALUE.log = log(LAND.VALUE),
    ACREAGE.inv = 1/(ACREAGE)
    #IMPROVEMENT.VALUE.log = log(IMPROVEMENT.VALUE)
    #NET.VALUE.sqr = NET.VALUE^2
   )# %>% na.omit()

df3 <- subset(df3, !(row.names(df3) %in% out3)) # For iterations
#rownames(df3) <- df3$Industry.Name
#df3 <- df3 #%>% filter(sign(df3$NET.VALUE.log) == 1)
fit3 <-
  lm(
    NET.VALUE ~ ACREAGE.inv,
    data = df3
  )
vars3 <- colnames(model.frame(fit3, df3))
df3 <-
  as.data.frame(df3 %>% dplyr::select(one_of(vars3), everything()))
out3 <- NULL
out3 <- mlreg(df = df3[vars3],
              fit = fit3,
              3)
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
