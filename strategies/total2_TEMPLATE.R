####################################################################################
# STRATEGY CODE
#
# getOrders is the interface between the backtester and your strategies
####################################################################################

# It's arguments and return list should stay the same.

####################################################################################
# ARGUMENTS 
####################################################################################

# store: see explanation under returned list below

# newRowList: this is a list of single-row OHLCV xts objects, one for each series

# currentPos: this is the current position (so if one wants to close all
# positions one could set marketOrders as -currentPos)

# params: these are the strategy params

####################################################################################
# RETURNED LIST
####################################################################################

# This function must return a list with the the following named memebers

# store
# marketOrders
# limitOrders1
# limitPrices1
# limitOrders2
# limitPrices2

# store contains data required by the strategy is in the variable store
# store is returned by the function and passed back to it the next time 
# it is called by the backetester.

# marketOrders is a vector containing the number of lots to be traded at the 
# open for each series that the
# strategy will hold on the current day
#
# The i'th entry of the integer vector pos (of length nseries) represents 
# the number of contracts you want to be long (short) on the next trading day 
# if the entry is positive (negative).

# Entries of marketOrders, limitOrders1, and limitOrders2 should be integers 
################################################################################

# This strategy uses only market orders

# The strategy will go short if RSI is > 50 + threshold 
#                      long  if RSI is < 50 - threshold
# and will be flat otherwie

# This is a contrarian (a.k.a. mean-reversion) type of strategy
################################################################################

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- rep(0,length(newRowList)) # used for initializing vectors

  if (is.null(store)) store <- initStore(newRowList,params$series)
  store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- -currentPos; pos <- allzero
  pos1 <- allzero; pos2 <- allzero; pos3 <- allzero
  
  
  
  if (store$iter > 20) {
    for (i in 1:length(params$series)) {
      if (i==1||i==2||i==3||i==10){
      #RSI
        pos1[params$series[i]] <- lgStFtrsi(store$cl,params$series[i],store$iter)*params$posSizes[params$series[i]]

      }
      if (i==4||i==5||i==7||i==8||i==9){
        #KDJ
        pos2[params$series[i]] <- lgStFtkdj(store$cl,store$high,store$low,store$index,params$series[i],store$iter)*params$posSizes[params$series[i]]

      }
      if (i==6){
        #CCI
        pos3[params$series[i]] <- lgStFtcci(store$cl,store$high,store$low,store$index,params$series[i],store$iter)*params$posSizes[params$series[i]]
        
      }
      
    }
  }
  #想法：当store$iter==300和store$iter==650的时候，检查PnL的正负，当：1前PNL为正后为负；2前负后负这两种情况时，在650时停止交易
  #想法：当损失超0下一丢丢时，停止交易
  
  # exit positions from yesterday
  marketOrders <- marketOrders + pos1 +pos2+pos3+ pos

  #cat(formatC(store$count,2),'\t',formatC(pos,2),'\n')
  #cat(formatC(pos,2),'\n')
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}

# functions for managing the store

initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
  for (i in 1:length(series))
    clStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Close)
  return(clStore)
}
initHighStore  <- function(newRowList,series) {
  highStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(highStore)
}
updateHighStore <- function(highStore, newRowList, series, iter) {
  for (i in 1:length(series))
    highStore[iter,i] <- as.numeric(newRowList[[series[i]]]$High)
  return(highStore)
}
initLowStore  <- function(newRowList,series) {
  lowStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(lowStore)
}
updateLowStore <- function(lowStore, newRowList, series, iter) {
  for (i in 1:length(series))
    lowStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Low)
  return(lowStore)
}
initIndexStore  <- function(newRowList,series) {
  indexStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(indexStore)
}
updateIndexStore <- function(indexStore, newRowList, series, iter) {
  for (i in 1:length(series))
    indexStore[iter,i] <- as.character(index(newRowList[[series[i]]]))
  return(indexStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              index=initIndexStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$index <- updateIndexStore(store$index,newRowList,series,store$iter)
  return(store)
}
###############################################################################
# main strategy logic

lgStFtcci <-	function(clStore,highStore,lowStore,indexStore,column,iter) {
  
  startIndex <- iter - 20 - 1
  
  closeP<-clStore[startIndex:iter,column]
  
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
 
  pricess<-cbind(closeP,highP,lowP)
  
  cci<-last(CCI(pricess))
  
  if (cci < -100)
    return(-1) # short
  if (cci > 100)
    return(1)  # long
  return(0)
}
lgStFtkdj <-	function(clStore,highStore,lowStore,indexStore,column,iter) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  #print(iter)
  startIndex <- iter - 20 - 1
  
  #	Close<-clStore[startIndex:iter,column]
  closeP<-clStore[startIndex:iter,column]
  
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  #print(ndate)
  #periodHigh<-xts(rep(0,ndate-8),order.by=as.Date(date[-(1:8)]))
  #periodLow<-xts(rep(0,ndate-8),order.by=as.Date(date[-(1:8)]))
  #RSV<-xts(rep(0,ndate-8),order.by=as.Date(date[-(1:8)]))
  
  #for(j in (9:ndate)){
  # for (i in (1:(ndate-8))) {
  #  period<-date[(j-8):j]
  #    periodHigh[i]<-max(highP[period])
  #   periodLow[i]<-min(lowP[period])
  #  RSV[i]<-100*(closeP[i]-periodLow[i])/(periodHigh[i]-periodLow[i])
  # names(periodHigh)<-"periodHigh"
  #    names(periodLow)<-"periodLow"
  #    names(RSV)<-"RSV"
  #  }
  #}
  perHigh<-max(highP)
  perLow<-min(lowP)
  RSVV<-100*(closeP-perHigh)/(perHigh-perLow)  #rsv<-c(50,50,as.numeric(RSVV))  #print(length(rsv))  #print(head(rsv))
  RSV1<-na.omit(xts(RSVV,order.by = as.Date(date[1:ndate])))
  KValue<-EMA(RSV1,n=2,ratio = 1/3)
  KValue[1]<-50
  DValue<-na.omit(EMA(KValue,n=2,ratio = 1/3))
  KValue<-KValue[-(1:2)]
  DValue<-DValue[-(1:2)]
  #print(head(KValue))
  
  k<-ifelse(KValue<20,1,0)
  KSignal<-ifelse(KValue>80,-1,k)
  d<-ifelse(DValue<20,1,0)
  DSignal<-ifelse(DValue>80,-1,d)
  
  KDSignal<-apply(merge(KSignal,DSignal),1,sum)
  KDSignal<-na.omit(KDSignal)
  KDSignal<-last(KDSignal)
  #print(KDSignal)  #KDSignal<-xts(KDSignal,order.by = index(KSignal))
  
  if (KDSignal<=-1)
    return(-1) # short
  if (KDSignal>=1)
    return(1)  # long
  return(0)
}
lgStFtrsi <-	function(clStore,column,iter) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  startIndexS <- iter - 6 - 1
  rsiS <- last(RSI(clStore[startIndexS:iter,column],n=6))
  
  startIndexL <- iter - 20 - 1
  rsiL <- last(RSI(clStore[startIndexL:iter,column],n=20))
  
  rsiSlag<-lag(rsiS,1)
  rsiLlag<-lag(rsiL,1)
  
  longs<-ifelse((rsiS>rsiL)&(rsiSlag<rsiLlag),1,0)
  shorts<-ifelse((rsiS<rsiL)&(rsiSlag>rsiLlag),-1,0)
  
  if ((rsiS > (50 + 25))||(shorts=-1))
    return(-1) # short
  if ((rsiS < (50 - 25))||(longs=1))
    return(1)  # long
  return(0)
}
