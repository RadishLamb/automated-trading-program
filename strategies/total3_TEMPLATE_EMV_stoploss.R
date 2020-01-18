

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- stopOuts <- rep(0,length(newRowList)) # used for initializing vectors
  
  marketOrders <- -currentPos; pos <- allzero
  pos1 <- allzero; pos2 <- allzero; pos3 <- allzero
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  ppos<-rep(0,length(params$series))


  
  
  if (store$iter > 10) {

    for (i in 1:length(params$series)) {
    #  if (i==1||i==2||i==3||i==10){
      #RSI
        #pos1[params$series[i]] <- lgStFtrsi(store$cl,params$series[i],store$iter)*params$posSizes[params$series[i]]

     # }
     # if (i==4||i==5||i==7||i==8||i==9){
    #    #KDJ
    #    pos2[params$series[i]] <- lgStFtkdj(store$cl,store$high,store$low,store$index,params$series[i],store$iter)*params$posSizes[params$series[i]]

     # }
      #if (i==6){
        #CCI
      ppos[i]<-lgStFtEMV(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo)
      pos[params$series[i]] <- ppos[i]*params$posSizes[params$series[i]]
        
      #}
      
    }
    
  }
  #想法：当store$iter==300和store$iter==650的时候，检查PnL的正负，当：1前PNL为正后为负；2前负后负这两种情况时，在650时停止交易
  #想法：当损失超0下一丢丢时，停止交易
  print(ppos)
  store <- updateStore(store, newRowList, params$series,ppos)
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
###pos##
initPosStore  <- function(newRowList,series) {
  posStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(posStore)
}
updatePosStore <- function(posStore, pos, series, iter) {
  #print(pos)
  posStore[iter,] <- pos
  return(posStore)
}
initVoStore  <- function(newRowList,series) {
  voStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(voStore)
}
updateVoStore <- function(voStore, newRowList, series, iter) {
  for (i in 1:length(series))
    voStore[iter,i] <- as.numeric(newRowList[[series[i]]]$Volume)
  return(voStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              index=initIndexStore(newRowList,series),
              ppooss=initPosStore(newRowList,series),
              vo=initVoStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series,psos) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$index <- updateIndexStore(store$index,newRowList,series,store$iter)
  store$ppooss<-updatePosStore(store$ppooss,psos,series,store$iter)
  store$vo <- updateClStore(store$vo,newRowList,series,store$iter)
  return(store)
}
###############################################################################
# main strategy logic
getTradeReturn <- function(prices,entry,exit,short=FALSE){
  prices<-as.numeric(prices)
  #print(short)
  #print(length(prices))
  #print(exit)
  #print(entry)
  #print(prices[exit])
  #print(prices[entry])
  
  if(short)
    return(prices[entry]/prices[exit]-1)
  else
    return(prices[exit]/prices[entry]-1)
}


lgStFtEMV <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore) {
  

  startIndex <- iter - 10 - 1
  #print(head(clStore[startIndex:iter,column]))
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  #print(head(date))
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  highP<-xts(highStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  lowP<-xts(lowStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  price<-cbind(highP,lowP)
  volume<-xts(voStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  
  #print(head(close))
  emv<-EMV(price,volume)
  emv1 <- as.numeric(last(emv[,1]))
  maEMV <- as.numeric(last(emv[,2]))
  emv1lag<-as.numeric(emv[iter-1,1])
  maEMVlag<-as.numeric(emv[iter-1,2])
  #print(emv1)
  #print(maEMV)
  posP<-ppooss[1:iter,column]
#  print(posP)
#  print(posP[iter-1])
#  print(column)
  #print(emv1lag)
  #print(maEMVlag)
  
    longs<-ifelse((emv1>maEMV)&(emv1lag<maEMVlag),1,0)
    shorts<-ifelse((emv1<maEMV)&(emv1lag>maEMVlag),-1,0)
   
    return(longs+shorts)
    
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
  startIndexS <- iter - params$lookbackS - 1
  rsiS <- last(RSI(clStore[startIndexS:iter,column],n=params$lookbackS))
  
  startIndexL <- iter - params$lookbackL - 1
  rsiL <- last(RSI(clStore[startIndexL:iter,column],n=params$lookbackL))
  
  rsiSlag<-lag(rsiS,1)
  rsiLlag<-lag(rsiL,1)
  
  longs<-ifelse((rsiS>rsiL)&(rsiSlag<rsiLlag),1,0)
  shorts<-ifelse((rsiS<rsiL)&(rsiSlag>rsiLlag),-1,0)
  
  if ((rsiS > (50 + params$threshold))||(shorts=-1))
    return(-1) # short
  if ((rsiS < (50 - params$threshold))||(longs=1))
    return(1)  # long
  return(0)
}