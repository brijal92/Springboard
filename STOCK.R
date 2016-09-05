library('quantmod')
# high cap stock
getSymbols("AAPL") # APPLE
chartSeries(AAPL, subset='last 3 months')
chartSeries(AAPL,TA=c(addVo(),addBBands()))
getSymbols("AMZN")#AMZN
chartSeries(AMZN, subset='last 3 months')
chartSeries(AMZN,TA=c(addVo(),addBBands()))
getSymbols("XOM")#EXXON
chartSeries(XOM, subset='last 3 months')
chartSeries(XOM,TA=c(addVo(),addBBands()))
getSymbols("BAC") # BANK OF AMERICA
chartSeries(BAC, subset='last 3 months')
getSymbols("MSFT")
chartSeries(MSFT, subset='last 3 months')
chartSeries(MSFT,TA=c(addVo(),addBBands()))
# MID CAP STOCK
getSymbols("TRIP") #TRIP ADVISOR
chartSeries(TRIP, subset='last 3 months')
chartSeries(TRIP,TA=c(addVo(),addBBands()))
getSymbols("AEO") # AEROPOSTALE
chartSeries(AEO, subset='last 3 months')
chartSeries(AEO,TA=c(addVo(),addBBands()))
getSymbols("STE")# STERIS
chartSeries(STE, subset='last 3 months')
chartSeries(STE,TA=c(addVo(),addBBands()))
getSymbols("RJF") # RAYMOND JAMES
chartSeries(RJF, subset='last 3 months')
chartSeries(RJF,TA=c(addVo(),addBBands()))
getSymbols("PAG") # PENSKE
chartSeries(PAG, subset='last 3 months')
chartSeries(PAG,TA=c(addVo(),addBBands()))
# PUBLICLY TRADED COMPANY
getSymbols("GOOGL") # GOOGLE
chartSeries(GOOGL, subset='last 3 months')
chartSeries(GOOGL,TA=c(addVo(),addBBands()))
getSymbols("CVX")   # CHEVERON
chartSeries(CVX, subset='last 3 months')
chartSeries(CVX,TA=c(addVo(),addBBands()))
getSymbols("NSRGY") # NESTLE
chartSeries(NSRGY, subset='last 3 months')
chartSeries(NSRGY,TA=c(addVo(),addBBands()))
getSymbols("FFIV") # F5 NETWORKS INC
chartSeries(FFIV, subset='last 3 months')
chartSeries(FFIV,TA=c(addVo(),addBBands()))
getSymbols("PG") # PROCTOR GAMBLE
chartSeries(PG, subset='last 3 months')
chartSeries(PG,TA=c(addVo(),addBBands()))
# VECTOR TO PREDICT DROP OR RAISE IN NEXT DAY
high_cap_1 <- c("AAPL","AMZN","XOM","BAC","MSFT")
mid_cap_1 <-  c("TRIP","AEO","STE","RJF","PAG")
pub_trad_1 <- c("GOOGL","CVX","NSRGY","FFIV","PG")
getSymbols(high_cap)
getSymbols(mid_cap)
getSymbols(pub_trad)
library('xts')
high_Cap <- data.frame(as.xts(merge(AAPL,AMZN,XOM,BAC,MSFT)))
mid_cap <-  data.frame(as.xts(merge(TRIP,AEO,STE,RJF,PAG)))
pub_trad <- data.frame(as.xts(merge(GOOGL,CVX,NSRGY,FFIV,PG)))
head(high_Cap[,1:12],2)
head(mid_cap[,1:12],2)
head(pub_trad[,1:12],2)
# set outcome variable
outcomeSymbol <- 'FISV.Volume'
# shift outcome value to be on same line as predictors
library(xts)
# high_cap
high_Cap100 <- xts(high_Cap,order.by=as.Date(rownames(high_Cap)))
high_Cap100 <- as.data.frame(merge(high_Cap, lm1=lag(high_Cap[,outcomeSymbol],-1)))
high_Cap100$outcome <- ifelse(high_Cap[,paste0(outcomeSymbol,'.1')] > high_cap[,outcomeSymbol], 1, 0)
# mid_cap
mid_Cap100 <- xts(mid_cap,order.by=as.Date(rownames(mid_cap)))
mid_Cap100 <- as.data.frame(merge(mid_cap, lm1=lag(mid_cap[,outcomeSymbol],-1)))
mid_Cap100$outcome <- ifelse(mid_Cap[,paste0(outcomeSymbol,'.1')] > mid_cap[,outcomeSymbol], 1, 0)
# pub_trad
pud_trad100 <- xts(pub_trad,order.by=as.Date(rownames(pub_trad)))
pub_trad100 <- as.data.frame(merge(pub_trad, lm1=lag(pub_trad[,outcomeSymbol],-1)))
pub_trad100$outcome <- ifelse(pub_trad[,paste0(outcomeSymbol,'.1')] > pub_trad[,outcomeSymbol], 1, 0)
# remove shifted down value
high_Cap100 <- high_Cap100[,!names(high_Cap100) %in% c(paste0(outcomeSymbol,'.1'))]
mid_Cap100 <- mid_Cap100[,!names(mid_Cap100) %in% c(paste0(outcomeSymbol,'.1'))]
pub_trad100 <- pub_trad100[,!names(pub_trad100) %in% c(paste0(outcomeSymbol,'.1'))]
