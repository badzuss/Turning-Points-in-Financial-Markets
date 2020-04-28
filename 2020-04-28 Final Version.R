# 1. Packages --------------------------------------------------------------
install.packages("pacman")

pacman::p_load(pacman,ggplot2,tibble,purrr,dplyr,tidyr,stringr,readr,
               forcats,readxl,TTR,quantmod,corrplot,rpart,rpart.plot,
               lubridate,caret,e1071,xgboost,pROC,ROCR, grDevices)
#profvis,pls


# 2. Pulling dataset from a .xlsx ------------------------------------------
dataset <- data.frame(read_excel("DJIA4519dataPT.xlsx"))
indexsp <- data.frame(read_excel("spx index last price.xlsx"))

indexcpi = read_excel("cpi_yoy_(inflation_rate).xlsx")
colnames(indexcpi) = c('date', 'cpi')

indexintrate = read_excel("fdtrmid_(interest_rate).xlsx")
colnames(indexintrate) = c('date', 'intrate')

indexindpro = read_excel("ip_chng_mom_(industrial_production_sa).xlsx")
colnames(indexindpro) = c('date', 'indpro')

indexunemp = read_excel("usertotn_(unemployment_rate).xlsx")
colnames(indexunemp) = c('date', 'unemp')

indexriskfree = read_excel("usgg10yr_(risk_free_rate).xlsx")
colnames(indexriskfree) = c('date', 'riskfree')

dividends = read_excel("dividends.xlsx")
colnames(dividends) = c('date', 'div_net', 'div_gross')

# 3. Dataset appendix -----------------------------------------------------

# Computing Return ROC()
dataset$Return <- round(ROC(dataset$Price, type = "discrete")*100, 2)

# Labelling Returns
dataset$ReturnTrend <- ifelse(dataset$Return >= 0, "Up", "Down")

# Computing RSI:
dataset$RSI_14 <- round(RSI(dataset$Price, n = 14, maType="WMA"),2)

# Computing EMA:
dataset$EMA_12 <- round(EMA(dataset$Price, n = 12),2)
dataset$EMA_26 <- round(EMA(dataset$Price, n = 26),2)

# Computing MACD:
dataset$MACD <- round((dataset$EMA_26 - dataset$EMA_12),2)

# Computing SMA:
dataset$SMA_20 <- round(SMA(dataset$Price, n = 20), 2)

# S&P 500 Benchmark merge:
dataset <- dataset %>% merge(indexsp, by.x = "Date", by.y = "date", all.x = TRUE)
dataset$spx.index[is.na(dataset$spx.index)] <- lag(dataset$spx.index,1)
dataset$spx.index[is.na(dataset$spx.index)] <- lead(dataset$spx.index,1)

# CPI index merge:
indexcpi = indexcpi %>%
  mutate(Day = weekdays(date), 
         date = case_when(Day == "Saturday" ~ date + 2*86400, 
                          Day == "Sunday" ~ date + 86400, 
                          TRUE ~ date)) %>%
  select(-Day)

dataset = merge(dataset, indexcpi, by.x = "Date", by.y = "date", all.x=TRUE)
for(i in 2:length(dataset[,1])){
  if(is.na(dataset$cpi[i])){
    dataset$cpi[i] = dataset$cpi[i - 1]
  }
}
dataset$cpi[is.na(dataset$cpi)] = 2.3
rm(indexcpi)

# FDTRMID index merge:
indexintrate = indexintrate %>%
  mutate(Day = weekdays(date), 
         date = case_when(Day == "Saturday" ~ date + 2*86400, 
                          Day == "Sunday" ~ date + 86400, 
                          TRUE ~ date)) %>%
  select(-Day)
dataset = merge(dataset, indexintrate, by.x = "Date", by.y = "date", all.x=TRUE)
for(i in 2:length(dataset[,1])){
  if(is.na(dataset$intrate[i])){
    dataset$intrate[i] = dataset$intrate[i - 1]
  }
}
dataset$intrate[is.na(dataset$intrate)] = 4.75
rm(indexintrate)

# IP_CHNG_MOM index merge:
indexindpro = indexindpro %>%
  mutate(Day = weekdays(date), 
         date = case_when(Day == "Saturday" ~ date + 2*86400, 
                          Day == "Sunday" ~ date + 86400, 
                          TRUE ~ date)) %>%
  select(-Day)
dataset = merge(dataset, indexindpro, by.x = "Date", by.y = "date", all.x=TRUE)
for(i in 2:length(dataset[,1])){
  if(is.na(dataset$indpro[i])){
    dataset$indpro[i] = dataset$indpro[i - 1]
  }
}
dataset$indpro[is.na(dataset$indpro)] = -8.94
rm(indexindpro)

# USERTOTN index merge:
indexunemp = indexunemp %>%
  mutate(Day = weekdays(date), 
         date = case_when(Day == "Saturday" ~ date + 2*86400, 
                          Day == "Sunday" ~ date + 86400, 
                          TRUE ~ date)) %>%
  select(-Day)
dataset = merge(dataset, indexunemp, by.x = "Date", by.y = "date", all.x=TRUE)
for(i in 2:length(dataset[,1])){
  if(is.na(dataset$unemp[i])){
    dataset$unemp[i] = dataset$unemp[i - 1]
  }
}
dataset$unemp[is.na(dataset$unemp)] = 54.9
rm(indexunemp)

# USGG10YR index merge:
indexriskfree = indexriskfree %>%
  mutate(Day = weekdays(date), 
         date = case_when(Day == "Saturday" ~ date + 2*86400, 
                          Day == "Sunday" ~ date + 86400, 
                          TRUE ~ date)) %>%
  select(-Day)
dataset = merge(dataset, indexriskfree, by.x = "Date", by.y = "date", all.x=TRUE)
for(i in 2:length(dataset[,1])){
  if(is.na(dataset$riskfree[i])){
    dataset$riskfree[i] = dataset$riskfree[i - 1]
  }
}
dataset$riskfree[is.na(dataset$riskfree)] = 4.06
rm(indexriskfree)

# dividends merge:
dataset = merge(dataset, dividends, by.x = "Date", by.y = "date", all.x=TRUE)
rm(dividends)

# Meric's variables
dataset$cpiDiff = dataset$cpi - lag(dataset$cpi,20,0)
dataset$intrateDiff = dataset$intrate - lag(dataset$intrate,20,0)
dataset$indproDiff = dataset$indpro - lag(dataset$indpro,20,0)
dataset$unempDiff = dataset$unemp - lag(dataset$unemp,20,0)
dataset$riskfreeDiff = dataset$riskfree - lag(dataset$riskfree,20,0)

dataset = dataset %>%
  mutate(SPDailyRet = c(0, diff(sp_price, 1)))

dataset$WeeklySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 20, 0))
dataset$WeeklySPLogRet[1:20] = 0

dataset$MonthlySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 20, 0))
dataset$MonthlySPLogRet[1:20] = 0

dataset$ThreeMonthlySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 60, 0))
dataset$ThreeMonthlySPLogRet[1:60] = 0

dataset$SixMonthlySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 120, 0))
dataset$SixMonthlySPLogRet[1:120] = 0

dataset$YearlySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 240, 0))
dataset$YearlySPLogRet[1:240] = 0

dataset$FiveYearlySPLogRet = log(dataset$sp_price/lag(dataset$sp_price, 1200, 0))
dataset$FiveYearlySPLogRet[1:1200] = 0

dataset$TPDummy[dataset$L3T == 1 & dataset$TPcand == 1] = 0
dataset$TPDummy[dataset$L3P == 1 & dataset$TPcand == 1] = 1

dataset$WeeklyLogRet = log(dataset$Price/lag(dataset$Price, 5, 0))
dataset$WeeklyLogRet[1:5] = 0

dataset$MonthlyLogRet = log(dataset$Price/lag(dataset$Price, 20, 0))
dataset$MonthlyLogRet[1:20] = 0

#subtracting previous date from current date
dataset$DayDiff = as.numeric(dataset$Date - lag(dataset$Date, 1, 0))
dataset$DayDiff[1] = 0

#Days since last Peak (L3P)
TrendL3P = dataset %>% 
  group_by(cumsum(c(FALSE, diff(L3P) != 0))) %>% 
  mutate(TrendL3P = row_number())
dataset$TrendL3P = TrendL3P$TrendL3P

#Days since last Through (L3T)
TrendL3T = dataset %>% 
  group_by(cumsum(c(FALSE, diff(L3T) != 0))) %>% 
  mutate(TrendL3T = row_number())
dataset$TrendL3T = TrendL3T$TrendL3T

#Highest Point Dummy
dataset$HighPoint[1] = dataset$Price[1]

for(i in 2:length(dataset[,1])){
  if(dataset$Price[i] > dataset$HighPoint[i-1]){
    dataset$HighPoint[i] = dataset$Price[i]
  }else{
    dataset$HighPoint[i] = dataset$HighPoint[i-1]
  }
}

dataset$HighPointDummy = dataset$HighPoint / dataset$Price
dataset$HighPointDummy[dataset$HighPointDummy != 1] = 0

#ncol(dataset)
dataset = dataset[,-"HighPoint"]

# 4. Creating Subset (TPcand only) -------------------------------------

small <- subset(data.frame(c(dataset[,1:3], dataset[,10:12])), dataset$TPcand == 1)

# Adjusting row names
for(i in 1:nrow(small)){ rownames(small)[i] <- i}

#Identifier's Price:
small$IdentifierPrice <- c(dataset[small$IDObs, "Price"])
small <- small[ , c('Date','TPreal','ObsNo','Price','IDObs','IDPrice')] # Columns reordering

small$TPcand <- NULL
rm(i)


# 5. Small Subset appendix ------------------------------------------------

# * 5.1 Ups & Downs counter -----
# Counting Ups (positive daily return) & Downs (negative daily return) between TPcand
x <- 1
y <- 0

for(i in small$ObsNo){
  y <- y + 1
  UpsCounter <- 0
  DownsCounter <- 0
  for(j in x:i){
    if(is.na(dataset$ReturnTrend[j]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[j] == "Up"){
      UpsCounter <- UpsCounter + 1
    }
    else if(dataset$ReturnTrend[j] == "Down"){
      DownsCounter <- DownsCounter + 1
    }
  }
  x <- i + 1    
  small$UpsCounter[y] <- c(UpsCounter)
  small$DownsCounter[y] <- c(DownsCounter)
}
rm(i,j,x,y, DownsCounter, UpsCounter)

# * 5.2 Trend shifts counter -----
# Trend shift counter (No. transitions from positive to negative daily returns) between TPcand

x <- 1
y <- 0

for(w in small$ObsNo){
  y <- y + 1
  ShiftCounter <- 0
  for(i in x:w){
    if(is.na(dataset$ReturnTrend[i]) == TRUE){
      next
    }
    else if(is.na(dataset$ReturnTrend[i+1]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[i] != dataset$ReturnTrend[i+1]){
      ShiftCounter <- ShiftCounter + 1
    }
    x <- w + 1
  }
  small$ShiftCounter[y] <- c(ShiftCounter)
}
rm(i,w,x,y,ShiftCounter)
# * 5.3 LU chain of positive returns TP -----
# The longest uninterrupted chain of positive logarithmic returns between TPcand

x <- 1
y <- 0

for(i in small$ObsNo){
  
  y <- y + 1
  UpsCounter <- 0
  UpsTemp <- 1
  
  for(j in x:i){
    if(is.na(dataset$ReturnTrend[j]) == TRUE){
      next
    }
    else if(is.na(dataset$ReturnTrend[j+1]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[j] == "Up" && dataset$ReturnTrend[j+1] == "Up"){
      UpsTemp <- UpsTemp + 1
      if(UpsTemp > UpsCounter){
        UpsCounter <- UpsTemp
      } 
    }
    else{
      UpsTemp <- 1
    }
    x <- i + 1
  }
  small$LUCP[y] <- c(UpsCounter)
}
rm(UpsCounter,UpsTemp,i,j,x,y)
# * 5.4 LU logreturn gain between TP -----
# The largest uninterrupted logarithmic return gain between TPcand

x <- 1
y <- 0

for(i in small$ObsNo){
  
  y <- y + 1
  Magnitude <- 0
  MagTemp <- 0
  
  for(j in x:i){
    if(is.na(dataset$ReturnTrend[j]) == TRUE){
      next
    }
    else if(is.na(dataset$ReturnTrend[j+1]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[j] == "Up"){
      MagTemp <- MagTemp + dataset$Return[j]
    }
    else if(dataset$ReturnTrend[j] != "Up"){
      if(Magnitude < MagTemp){
        Magnitude <- MagTemp                                
      }
      MagTemp <- 0
    }
    x <- i + 1
  }
  small$LUG[y] <- c(Magnitude)
}
rm(i,j,Magnitude, MagTemp,x,y)
# * 5.5 LU chain of negative returns TP ----
# The longest uninterrupted chain of negative logarithmic returns between TPcand

x <- 1
y <- 0

for(i in small$ObsNo){
  
  y <- y + 1
  DownsCounter <- 0
  DownsTemp <- 1
  
  for(j in x:i){
    if(is.na(dataset$ReturnTrend[j]) == TRUE){
      next
    }
    else if(is.na(dataset$ReturnTrend[j+1]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[j] == "Down" && dataset$ReturnTrend[j+1] == "Down"){
      DownsTemp <- DownsTemp + 1
      if(DownsTemp > DownsCounter){
        DownsCounter <- DownsTemp
      } 
    }
    else{
      DownsTemp <- 1
    }
    x <- i + 1
  }
  small$LUCN[y] <- c(DownsCounter)
} 
rm(DownsCounter,DownsTemp,i,j,x,y)
# * 5.6 LU logreturn loss between TP -----
# The largest uninterrupted logarithmic return loss between TPcand

x <- 1
y <- 0

for(i in small$ObsNo){
  
  y <- y + 1
  Magnitude <- 0
  MagTemp <- 0
  
  for(j in x:i){
    if(is.na(dataset$ReturnTrend[j]) == TRUE){
      next
    }
    else if(is.na(dataset$ReturnTrend[j+1]) == TRUE){
      next
    }
    else if(dataset$ReturnTrend[j] == "Down"){
      MagTemp <- MagTemp + dataset$Return[j]
    }
    else if(dataset$ReturnTrend[j] != "Down"){
      if(Magnitude > MagTemp){
        Magnitude <- MagTemp                                
      }
      MagTemp <- 0
    }
    x <- i + 1
  }
  small$LUL[y] <- c(Magnitude)
}

rm(i,j,Magnitude,MagTemp,x,y)

# * 5.7 "Between TPcand pairs" features ----

##Returns between TPcand pair
small$ReturnBetTP <- round((log(small$Price) - log(lag(small$Price,1)))*100,2)
small$ReturnBetTP[1] <- round((log(small$Price[1]) - log(dataset$Price[1]))*100,2)

## Days between TPcand pair
small$DaysBetTP <- as.numeric(small$Date - lag(small$Date,1))
small$DaysBetTP[1] <- as.numeric(small$Date[1] - dataset$Date[1])

## No. sessions between TPcand pair
small$SessionsBetTP <- small$ObsNo - lag(small$ObsNo,1)
small$SessionsBetTP[1] <- small$ObsNo[1]

## Slope between TPcand pair (days)
small$SlopeDayTP <- round((small$ReturnBetTP / as.numeric(small$DaysBetTP))*100,2)

## Slope between TPcand pair (sessions)
small$SlopeSesTP <- round((small$ReturnBetTP / small$SessionsBetTP)*100,2)

# * 5.8 "Between TPcand and its identifier" features ----

## Returns between TPcand and its identifier
small$ReturnTP_ID <- round((log(small$IdentifierPrice) - log(small$Price))*100,2)

## Days between TPcand and its identifier
small$DaysTP_ID <- as.numeric(dataset$Date[small$IDObs] - small$Date)

## No. sessions between TPcand and its identifier
small$SessionsTP_ID <- (small$IDObs - small$ObsNo)

## Slope between TPcand and its identifier (days)
small$SlopeDayTP_ID <- round((small$ReturnTP_ID / as.numeric(small$DaysTP_ID))*100,2)

## Slope between TPcand and its identifier (sessions)
small$SlopeSesTP_ID <- round((small$ReturnTP_ID / small$SessionsTP_ID)*100,2)



# * 5.9 "Between Identifiers" features ----

##Returns between Identifiers
small$ReturnBetIDs <- round((log(small$IdentifierPrice) - log(lag(small$IdentifierPrice,1)))*100,2)
small$ReturnBetIDs[1] <- round((log(small$IdentifierPrice[1]) - log(dataset$Price[1]))*100,2)

## Days between Identifiers 
small$DaysBetIDs <- as.numeric(dataset$Date[small$IDObs] - lag(dataset$Date[small$IDObs],1))
small$DaysBetIDs[1] <- as.numeric(dataset$Date[small$IDObs[1]] - dataset$Date[1])

## No. sessions between Identifiers
small$SessionsBetID <- small$IDObs - lag(small$IDObs,1)
small$SessionsBetID[1] <- small$IDObs[1]

## Slope between Identifiers pair (days)
small$SlopeDayID <- round((small$ReturnBetIDs / as.numeric(small$DaysBetIDs))*100,2)

## Slope between Identifiers pair (sessions)
small$SlopeSesID <- round((small$ReturnBetIDs / small$SessionsBetID)*100,2)



# * 5.10 Benchmark computations ------

## Appending small dataset with benchmark prices
#small$sp500PriceTP <- dataset$spx.index[small$ObsNo]
#small$sp500PriceID <- dataset$spx.index[small$IDObs]

## Benchmark returns between TPcand pair
#small$sp500ReturnTP <- round((log(small$sp500PriceTP) - log(lag(small$sp500PriceTP,1)))*100,2)
#small$sp500ReturnTP[1] <- round((log(small$sp500PriceTP[1]) - log(dataset$spx.index[1]))*100,2)

## Benchmark returns between TPcand and its identifier
#small$sp500ReturnTP_ID <- round((log(small$sp500PriceID) - log(small$sp500PriceTP))*100,2)

## Benchmark returns between identifiers
#small$sp500ReturnID <- round((log(small$sp500PriceID) - log(lag(small$sp500PriceID,1)))*100,2)
#small$sp500ReturnID[1] <- round((log(small$sp500PriceID[1]) - log(dataset$spx.index[1]))*100,2)

## Benchmark vs DJI
#small$ReturnDJIvsSP500TP <- small$ReturnBetTP - small$sp500PriceTP
#small$ReturnDJIvsSP500ID <- small$ReturnBetIDs - small$sp500ReturnID
#small$ReturnDJIvsSP500TP_ID <- small$ReturnTP_ID - small$sp500ReturnTP_ID



# * 5.11 Volatility computations (volatility function) ------

# Stack Overflow question (using '$' in atomic vector)
#dataset$SessionsInYear <- sapply(match_vec, function(match_table) match_table[match_table$Var1 == match_vec,2])

# Counting No. sessions in the year the observation took place
match_vec <- as.integer(year(dataset$Date))
match_table <- as.data.frame(table(year(dataset$Date)))

dataset$SessionsInYear <- vector("numeric", nrow(dataset))
for(i in seq_along(dataset$Date)){
  dataset$SessionsInYear[i] <- match_table[match_table$Var1 == match_vec[i],2]
}

small$SessInYear <- dataset$SessionsInYear[small$ObsNo]
AvgSessionNo <- round(small$SessInYear %>% mean())
rm(i,match_table,match_vec)

# Volatility
dataset$volWeekly <- volatility(dataset$Price, 5, calc = "close", N = AvgSessionNo)
dataset$volMonthly <- volatility(dataset$Price, 21, calc = "close", N = AvgSessionNo)
dataset$volQuarterly <- volatility(dataset$Price, 63, calc = "close", N = AvgSessionNo)
dataset$volYearly <- volatility(dataset$Price, n = AvgSessionNo, calc = "close", N = AvgSessionNo)

small$volWeeklyTP <- dataset$volWeekly[small$ObsNo]
small$volMonthlyTP <- dataset$volMonthly[small$ObsNo]
small$volQuarterlyTP <- dataset$volQuarterly[small$ObsNo]
#mall$volYearlyTP <- dataset$volYearly[small$ObsNo]

small$volWeeklyID <- dataset$volWeekly[small$IDObs]
small$volMonthlyID <- dataset$volMonthly[small$IDObs]
small$volQuarterlyID <- dataset$volQuarterly[small$IDObs]
#small$volYearlyID <- dataset$volYearly[small$IDObs]


# * 5.12 Weekday/ Month Identification --------

# Names extraction
dataset$Weekdayid <- weekdays(as.Date(dataset$Date))
dataset$Monthid <- months(dataset$Date)

# Factorizing & levels re-ordering
dataset$Weekdayid <- factor(dataset$Weekdayid,
                            levels(factor(dataset$Weekdayid))[c(2,4,5,3,1)])
dataset$Monthid <- factor(dataset$Monthid,
                          levels(factor(dataset$Monthid))[c(5,4,8,1,9,7,6,2,12,11,10,3)])

# Factor to numeric conversion
dataset$Weekdayid <- factor(as.numeric(dataset$Weekdayid))
dataset$Monthid <- factor(as.numeric(dataset$Monthid))

dataset$Weekdayid <- as.numeric(levels(dataset$Weekdayid))[dataset$Weekdayid]
dataset$Monthid <- as.numeric(levels(dataset$Monthid))[dataset$Monthid]

# Copying IDs to the 'small' df
small$WeekdayTP <- dataset$Weekdayid[small$ObsNo]
small$MonthTP <- dataset$Monthid[small$ObsNo]

small$WeekdayID <- dataset$Weekdayid[small$IDObs]
small$MonthID <- dataset$Monthid[small$IDObs]

# Copying Bloomberg data to the 'small' df
small$cpi <- dataset$cpi[small$ObsNo]
small$intrate <- dataset$intrate[small$ObsNo]
small$indpro <- dataset$indpro[small$ObsNo]
small$unemp <- dataset$unemp[small$ObsNo]
small$riskfree <- dataset$riskfree[small$ObsNo]
small$dividends <- dataset$dividends[small$ObsNo]

#Meric's variables
small$cpiDiff = dataset$cpiDiff[small$ObsNo]
small$intrateDiff = dataset$intrateDiff[small$ObsNo]
small$indproDiff = dataset$indproDiff[small$ObsNo]
small$unempDiff = dataset$unempDiff[small$ObsNo]
small$riskfreeDiff = dataset$riskfreeDiff[small$ObsNo]

small$SPDailyRet = dataset$SPDailyRet[small$ObsNo]
small$WeeklySPLogRet = dataset$WeeklySPLogRet[small$ObsNo]
small$MonthlySPLogRet = dataset$MonthlySPLogRet[small$ObsNo]
small$ThreeMonthlySPLogRet = dataset$ThreeMonthlySPLogRet[small$ObsNo]
small$SixMonthlySPLogRet = dataset$SixMonthlySPLogRet[small$ObsNo]
small$YearlySPLogRet = dataset$YearlySPLogRet[small$ObsNo]
small$FiveYearlySPLogRet = dataset$FiveYearlySPLogRet[small$ObsNo]
small$WeeklyLogRet = dataset$WeeklyLogRet[small$ObsNo]
small$MonthlyLogRet = dataset$MonthlyLogRet[small$ObsNo]
small$DayDiff = dataset$DayDiff[small$ObsNo]
small$TrendL3P = dataset$TrendL3P[small$ObsNo]
small$TrendL3T = dataset$TrendL3T[small$ObsNo]
small$HighPointDummy = dataset$HighPointDummy[small$ObsNo]

# * 5.13 Trading pauses distribution --------

#small$breaksTP_ID <- small$DaysTP_ID - small$SessionsTP_ID
#small$breaksTP_TP <- small$DaysBetTP - small$SessionsBetTP
#small$breaksID_ID <- small$DaysBetIDs - small$SessionsBetID


# 6. Output print ---------------

#str(small)
#head(small,8)
#tail(small,8)
#(dataset)
#head(dataset,8)
#tail(dataset,8)



# 7. Corelation Matrix ------

dev.new()
small_cor <- cor(subset(small, select = -c(Date,ObsNo,IDObs)))
res1 <- cor.mtest(subset(small,select = -c(Date,ObsNo,IDObs)), conf.level = .90)

dev.new()
corrplot(small_cor, type = "lower", method = "circle", tl.srt = 45,
         tl.col = "black")
dev.new()
corrplot(small_cor,type = "lower",tl.srt = 15,
         p.mat = res1$p, insig.level = .1, tl.col = "black")

# 8. Cleaning Data  -----------------

small_clean <- small %>% 
  select(-c(1,2,4)) %>% 
  mutate(TPreal = factor(TPreal, levels = c(0,1), labels = c("False", "True"))) %>%
  na.omit()


# 9. Simple Decision Tree---------------------------------------------------------------

# Pre-processing:
# Zero-variance and Near Zero-Variance Predictors unlikely to cause issues
# in tree-based model

# Correlated Predictors reduction:

# Evaluation
summary(small_cor[upper.tri(small_cor)])

# Highly correlated descriptors identification (absolute correlaton > 0.75)
highlyCor <- findCorrelation(small_cor, cutoff = .75)

# Flagged predictors removal
smallSDT <- small[,-c(1,2,4)]
smallSDT <- smallSDT[,-highlyCor]

# Re-evaluation
smallSDTcor <- cor(smallSDT)
summary(smallSDTcor[upper.tri(smallSDTcor)])

# Data Splitting
set.seed(101)
trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

# Split with highly correlated predictors
trainingSDT <- small_clean[trainIndex, ]
testingSDT <- small_clean[-trainIndex, ]

# SPlit with highly correlated descriptors REMOVED
trainingSDT2 <- smallSDT[trainIndex, ]
testingSDT2 <- smallSDT[-trainIndex, ]

# Model Training and Tuning

# Subsets validation
prop.table(table(trainingSDT$TPreal))
prop.table(table(testingSDT$TPreal))

# With highly correlated predictors
set.seed(112)
rPartfit <- rpart(TPreal~., data = trainingSDT, method = 'class')
dev.new()
rpart.plot(rPartfit, extra = 106)

# With highly correlated descriptors REMOVED
rPartfit2 <- rpart(TPreal~., data = trainingSDT2, method = 'class')
dev.new()
rpart.plot(rPartfit2, extra = 106)

# Extracting Predictions

# With highly correlated predictors
predict_unseen_Test <-predict(rPartfit, testingSDT, type = 'class')
confusionMatrix(data = predict_unseen_Test, testingSDT$TPreal)

pred  <- prediction(as.numeric(testingSDT$TPreal), as.numeric(predict_unseen_Test))
roc   <- performance(pred, measure="tpr", x.measure="fpr")
dev.new()
plot(roc, col="orange", lwd=2)
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
gauge_SDT <- slot(auc, 'y.values')
print(paste("Area under the curve:", gauge_SDT))

# With highly correlated descriptors REMOVED
predict_unseen_Test2 <-predict(rPartfit2, testingSDT2, type = 'class')
confusionMatrix(data = predict_unseen_Test2, as.factor(testingSDT2$TPreal))

pred2  <- prediction(as.numeric(testingSDT2$TPreal), as.numeric(predict_unseen_Test2))
roc2   <- performance(pred2, measure="tpr", x.measure="fpr")
dev.new()
plot(roc2, col="orange", lwd=2)
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc2 = performance(pred2, 'auc')
gauge_SDT2 <- slot(auc2, 'y.values')
print(paste("Area under the curve:", gauge_SDT2))

#
rm(predict_unseen_Test,predict_unseen_Test2, pred,pred2, roc,roc2, auc, auc2)

# 10. Partial least squares discriminant analysis (PLS) ---------------------------------------

# Pre-processing:Zero-variance and Near Zero-Variance Predictors evaluation
nzv <- nearZeroVar(small_clean)
str(nzv)
if(length(nzv) != 0){small_clean <- small_clean_temp[, -nzv]}

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingPLS <- small_clean[trainIndex, ]
testingPLS <- small_clean[-trainIndex, ]

# Model Training and Tuning
ctrlPLS <- trainControl(method = "cv",
                        number = 10,
                        returnResamp = "all",
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)


set.seed(112)
plsFit <- train(TPreal~.,
                data = trainingPLS, 
                method = "pls", 
                preProc = c("center", "scale"),
                tuneLength = 15, 
                trControl = ctrlPLS, 
                metric = "ROC")

plsFit
dev.new()
ggplot(plsFit)

# Extracting Predictions
plsTPreal <- predict.train(plsFit, 
                           newdata = testingPLS)

plsTPreal.probs <- predict.train(plsFit,
                                 newdata = testingPLS, 
                                 type = "prob" )

plsTPreal.ROC <- roc(plsTPreal.probs$False, 
                     response = testingPLS$TPreal,
                     levels = rev(levels(testingPLS$TPreal)))

dev.new()
plot(plsTPreal.ROC, main = "PLS ROC resampling: cv")

confusionMatrix(data = plsTPreal, testingPLS$TPreal)
gauge_PLS <- plsTPreal.ROC$auc
gauge_PLS

rm(plsTPreal)
# 11. XGB Tree -------------------------------------------------------------------

# Tuning Grid
xgbGrid <- expand.grid(nrounds = c(1, 10),
                       max_depth = c(1, 4),
                       eta = c(.1, .4),
                       gamma = 0,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = c(.8, 1))


# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingXGB <- small_clean[trainIndex, ]
testingXGB <- small_clean[-trainIndex, ]

# Model Training and Tuning
ctrlXGB_cv <- trainControl(method = "cv",
                           number = 10,
                           returnResamp = "all",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

ctrlXGB_loocv <- trainControl(method = "LOOCV",
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary)

ctrlXGB_none <- trainControl(method = "none",
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)


set.seed(112)
xgbFit_cv <- train(TPreal~.,
                   data = trainingXGB,
                   method = "xgbTree",
                   trControl = ctrlXGB_cv,
                   metric = "ROC",
                   preProc = c("center", "scale"),
                   tuneGrid = xgbGrid)

xgbFit_loocv <- train(TPreal~.,
                      data = trainingXGB,
                      method = "xgbTree",
                      trControl = ctrlXGB_loocv,
                      metric = "ROC",
                      preProc = c("center", "scale"),
                      tuneGrid = xgbGrid)

xgbFit_none <- train(TPreal~.,
                     data = trainingXGB,
                     method = "xgbTree", 
                     trControl = ctrlXGB_none,
                     tuneGrid = xgbGrid[nrow(xgbGrid),],
                     metric = "ROC", 
                     preProc = c("center", "scale"))


xgbFit_cv
dev.new()
ggplot(xgbFit_cv)
xgbFit_loocv
dev.new()
ggplot(xgbFit_loocv)
xgbFit_none

# Extracting Predictions
xgbTPreal_cv <- predict.train(xgbFit_cv, newdata = testingXGB)
xgbTPreal_loocv <- predict.train(xgbFit_loocv, newdata = testingXGB)
xgbTPreal_none <- predict.train(xgbFit_none, newdata = testingXGB)

confusionMatrix(data = xgbTPreal_cv, testingXGB$TPreal)
confusionMatrix(data = xgbTPreal_loocv, testingXGB$TPreal)
confusionMatrix(data = xgbTPreal_none, testingXGB$TPreal)

xgbTPreal_cv.probs <- predict.train(xgbFit_cv, newdata = testingXGB, type = "prob")
xgbTPreal_loocv.probs <- predict.train(xgbFit_loocv, newdata = testingXGB, type = "prob")
xgbTPreal_none.probs <- predict.train(xgbFit_none, newdata = testingXGB, type = "prob")
####
xgbTPreal_loocvtrain.probs <- predict.train(xgbFit_loocv, newdata = trainingXGB, type = "prob")
xgbTPreal_loocvtrain.ROC <- roc(xgbTPreal_loocvtrain.probs$False,
                                response = trainingXGB$TPreal,
                                levels=rev(levels(trainingXGB$TPreal)))
###
xgbTPreal_cv.ROC <- roc(xgbTPreal_cv.probs$False,
                        response = testingXGB$TPreal,
                        levels=rev(levels(testingXGB$TPreal)))

xgbTPreal_loocv.ROC <- roc(xgbTPreal_loocv.probs$False,
                           response = testingXGB$TPreal,
                           levels=rev(levels(testingXGB$TPreal)))

xgbTPreal_none.ROC <- roc(xgbTPreal_none.probs$False,
                          response = testingXGB$TPreal,
                          levels=rev(levels(testingXGB$TPreal)))


gauge_XGB_cv <- xgbTPreal_cv.ROC$auc
gauge_XGB_cv
dev.new()
plot(xgbTPreal_cv.ROC, main = "xgboost ROC method: cv")

gauge_XGB_loocv <- xgbTPreal_loocv.ROC$auc
gauge_XGB_loocv
dev.new()
plot(xgbTPreal_loocv.ROC, main = "xgboost ROC method: LOOCV")

gauge_XGB_none <- xgbTPreal_none.ROC$auc
gauge_XGB_none
dev.new()
plot(xgbTPreal_none.ROC, main = "xgboost ROC method: none")
##
xgbTPreal_loocvtrain.ROC
xgbTPreal_loocv.ROC

# Feature Importances
cv_imp <- varImp(xgbFit_cv)
cv_imp
dev.new()
plot(cv_imp, main = 'xgbFit_cv Feature Importance')

loocv_imp <- varImp(xgbFit_loocv)
loocv_imp
dev.new()
plot(loocv_imp, main = 'xgbFit_loocv Feature Importance')

none_imp <- varImp(xgbFit_none)
none_imp
dev.new()
plot(none_imp, main = 'xgbFit_none Feature Importance')

# 12. Support Vector Machines with Radial Basis Function Kernel-------------

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingSVM <- small_clean[trainIndex, ]
testingSVM <- small_clean[-trainIndex, ]

# TrainControl definitions
# ctrlSVM1 <- trainControl(method = "repeatedcv",
#                         number = 10,
#                         repeats = 10,
#                         classProbs = TRUE,
#                         summaryFunction = twoClassSummary)

ctrlSVM2 <- trainControl(method = "cv",
                         number = 10,
                         returnResamp = "all",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

# ctrlSVM3 <- trainControl(method = "boot",
#                          number = 10,
#                          returnResamp = "all",
#                          classProbs = TRUE,
#                          summaryFunction = twoClassSummary)

# ctrlSVM4 <- trainControl(method = "LOOCV",
#                          classProbs = TRUE, 
#                          summaryFunction = twoClassSummary)

# Training
set.seed(112)
# svmFit1 <- train(TPreal~.,
#                    data = trainingSVM,
#                    method = "svmRadial",
#                    trControl = ctrlSVM1,
#                    metric = "ROC",
#                    preProc = c("center", "scale"),
#                    tuneLength = 8)

svmFit2 <- train(TPreal~.,
                 data = trainingSVM,
                 method = "svmRadial",
                 trControl = ctrlSVM2,
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneLength = 8)

# svmFit3 <- train(TPreal~.,
#                  data = trainingSVM,
#                  method = "svmRadial",
#                  trControl = ctrlSVM3,
#                  metric = "ROC",
#                  preProc = c("center", "scale"),
#                  tuneLength = 8)
# 
# svmFit4 <- train(TPreal~.,
#                  data = trainingSVM,
#                  method = "svmRadial",
#                  trControl = ctrlSVM4,
#                  metric = "ROC",
#                  preProc = c("center", "scale"),
#                  tuneLength = 8)


# Summary
# svmFit1
# dev.new()
# ggplot(svmFit1)
svmFit2
dev.new()
ggplot(svmFit2)
# svmFit3
# dev.new()
# ggplot(svmFit3)
# svmFit4
# dev.new()
# ggplot(svmFit4)


# Extracting Predictions
# svmFit1.predict <- predict.train(svmFit1, newdata = testingSVM)
svmFit2.predict <- predict.train(svmFit2, newdata = testingSVM)
# svmFit3.predict <- predict.train(svmFit3, newdata = testingSVM)
# svmFit4.predict <- predict.train(svmFit4, newdata = testingSVM)

# svmFit1.probs <- predict.train(svmFit1, newdata = testingSVM, type = "prob")
svmFit2.probs <- predict.train(svmFit2, newdata = testingSVM, type = "prob")
# svmFit3.probs <- predict.train(svmFit3, newdata = testingSVM, type = "prob")
# svmFit4.probs <- predict.train(svmFit4, newdata = testingSVM, type = "prob")

# svmFit1.roc <- roc(svmFit1.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
svmFit2.roc <- roc(svmFit2.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
# svmFit3.roc <- roc(svmFit3.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))
# svmFit4.roc <- roc(svmFit4.probs$False, response = testingSVM$TPreal,levels=rev(levels(testingSVM$TPreal)))

# Confusion Matrix
# confusionMatrix(data = svmFit1.predict, testingSVM$TPreal)
confusionMatrix(data = svmFit2.predict, testingSVM$TPreal)
# confusionMatrix(data = svmFit3.predict, testingSVM$TPreal)
# confusionMatrix(data = svmFit4.predict, testingSVM$TPreal)

# Plotting
# gauge_SVM1 <- svmFit1.roc$auc
gauge_SVM2 <- svmFit2.roc$auc
# gauge_SVM3 <- svmFit3.roc$auc
# gauge_SVM4 <- svmFit4.roc$auc

# dev.new()
# plot(svmFit1.roc, main = "SVM ROC resampling: repeatedcv")
dev.new()
plot(svmFit2.roc, main = "SVM ROC resampling: cv")
# dev.new()
# plot(svmFit3.roc, main = "SVM ROC resampling: boot")
# dev.new()
# plot(svmFit4.roc, main = "SVM ROC resampling: LOOCV")

# 13. Logistic Regression-----

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)


trainingGLM <- small_clean[trainIndex, ]
testingGLM <- small_clean[-trainIndex, ]

# TrainControl Definition
ctrlGLM1 <- trainControl(method = "cv",
                         number = 10,
                         returnResamp = "all",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

ctrlGLM2 <- trainControl(method = "repeatedcv",
                         number = 10,
                         repeats = 10,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

# Training
set.seed(112)
glmFit1 <- train(TPreal~.,
                 data = trainingGLM,
                 method = "glm",
                 trControl = ctrlGLM1,
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 family = "binomial")

glmFit2 <- train(TPreal~.,
                 data = trainingGLM,
                 method = "glm",
                 trControl = ctrlGLM2,
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 family = "binomial")

# Summary
glmFit1
glmFit2

# Extracting Predictions
glmFit1.predict <- predict.train(glmFit1, newdata = testingGLM)
glmFit2.predict <- predict.train(glmFit2, newdata = testingGLM)

glmFit1.probs <- predict.train(glmFit1, newdata = testingGLM, type = "prob")
glmFit2.probs <- predict.train(glmFit2, newdata = testingGLM, type = "prob")

glmFit1.roc <- roc(glmFit1.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))
glmFit2.roc <- roc(glmFit2.probs$False, response = testingGLM$TPreal,levels=rev(levels(testingGLM$TPreal)))

# Confusion Matrix
confusionMatrix(data = glmFit1.predict, testingGLM$TPreal)
confusionMatrix(data = glmFit2.predict, testingGLM$TPreal)

# Plotting
gauge_GLM1 <- glmFit1.roc$auc
dev.new()
plot(glmFit1.roc, main = "GLM ROC resampling: cv")

gauge_GLM2 <- glmFit2.roc$auc
dev.new()
plot(glmFit2.roc, main = "GLM ROC resampling: repeatedcv")

# 14. Elastic Net Regression----------------

# Data Splitting
# set.seed(101)
# trainIndex <- createDataPartition(small_clean$TPreal, p = 0.75, list = FALSE)

trainingENR <- small_clean[trainIndex, ]
testingENR <- small_clean[-trainIndex, ]

# TrainControl Definition
ctrlENR1 <- trainControl(method = "cv",
                         number = 10,
                         returnResamp = "all",
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)

# Training
set.seed(112)
enrFit1 <- train(TPreal~.,
                 data = trainingENR,
                 method = "glmnet",
                 trControl = ctrlENR1,
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneLength = 10)

# Summary
enrFit1
dev.new()
ggplot(enrFit1)


# Extracting Predictions
enrFit1.predict <- predict.train(enrFit1, newdata = testingENR)
enrFit1.probs <- predict.train(enrFit1, newdata = testingENR, type = "prob")
enrFit1.roc <- roc(enrFit1.probs$False, response = testingENR$TPreal,levels=rev(levels(testingENR$TPreal)))

# Confusion Matrix
confusionMatrix(data = enrFit1.predict, testingENR$TPreal)


# Plotting
gauge_ENR1 <- enrFit1.roc$auc
dev.new()
plot(enrFit1.roc, main = "Elastic Net Reg ROC resampling: cv")


# SUMMARY -------------------------------------------------------------------
print(paste(deparse(substitute(gauge_SDT)), gauge_SDT))
print(paste(deparse(substitute(gauge_SDT2)), gauge_SDT2))
print(paste(deparse(substitute(gauge_PLS)), gauge_PLS))
print(paste(deparse(substitute(gauge_XGB_cv)), gauge_XGB_cv))
print(paste(deparse(substitute(gauge_XGB_loocv)), gauge_XGB_loocv))
print(paste(deparse(substitute(gauge_XGB_none)), gauge_XGB_none))
# print(paste(deparse(substitute(gauge_SVM1)), gauge_SVM1))
print(paste(deparse(substitute(gauge_SVM2)), gauge_SVM2))
# print(paste(deparse(substitute(gauge_SVM3)), gauge_SVM3))
# print(paste(deparse(substitute(gauge_SVM4)), gauge_SVM4))
print(paste(deparse(substitute(gauge_GLM1)), gauge_GLM1))
print(paste(deparse(substitute(gauge_GLM2)), gauge_GLM2))
print(paste(deparse(substitute(gauge_ENR1)), gauge_ENR1))

# Plotting
dev.new()
plot(svmFit2.roc, main = "ROC curves for SVM, XGB & PLS")
plot(xgbTPreal_cv.ROC, add = TRUE, col = "blue")
plot(xgbTPreal_loocv.ROC, add = TRUE, col = "red")
plot(xgbTPreal_none.ROC, add = TRUE, col = "pink")
plot(plsTPreal.ROC, add = TRUE, col = "green")
plot(glmFit1.roc, add = TRUE, col = "yellow")
plot(glmFit2.roc, add = TRUE, col = "grey")
plot(enrFit1.roc, add = TRUE, col = "violet")
legend("right", legend = c("SVM", "XGBcv", "XGBloocv", "XGBnone", "PLS", "GLMcv", "GLMrcv", "ENR"),
       bty = "n", cex = 1, lty = 1,
       col = c("black", "blue", "red", "pink", "green", "yellow", "grey", "violet"))

# Comparing Resampling Distributions
resamps <- resamples(list(XGB = xgbFit_cv,
                          PLS = plsFit,
                          SVM = svmFit2,
                          GLM = glmFit1,
                          ENR = enrFit1))
resamps
summary(resamps)

# Differences
difValues <- diff(resamps)
difValues
summary(difValues)

# Density plot
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))
