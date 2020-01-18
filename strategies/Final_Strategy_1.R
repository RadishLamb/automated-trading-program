
maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- stopOuts <- rep(0,length(newRowList)) # used for initializing vectors
  
  marketOrders <- -currentPos; pos <- allzero
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  ppos<-rep(0,length(params$series))
  possizes<-rep(0,length(params$series))
  
  entryAndExit1<-rep(0,length(params$series))
  #exitIter<-rep(0,length(params$series))
  #entryAndExit<-rbind(entryIter,exitIter)
  
  if (store$iter > 33) {

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
      ppos[i]<-lgStFt(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]
      possizes[i]<-calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
      pos[params$series[i]] <- ppos[i]*possizes[i]
      entryAndExit1[i]<-lgStFt(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      #entryAndExit[1,i]<-entryIter
      #entryAndExit[2,i]<-exitIter
      
      #}
      
    }
    #print(entryAndExit)
  }
  #想法：当store$iter==300和store$iter==650的时候，检查PnL的正负，当：1前PNL为正后为负；2前负后负这两种情况时，在650时停止交易
  #想法：当损失超0下一丢丢时，停止交易
#  print(ppos)
#  print("-")
#  print(entryAndExit1)
  
  
  store <- updateStore(store, newRowList, params$series,ppos,entryAndExit1)
  # exit positions from yesterday
  marketOrders <- marketOrders + pos

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
#entryexit
initEntryExitStore  <- function(newRowList,series) {
  entryIterStore <- matrix(0,nrow=1,ncol=length(series))
  return(entryIterStore)
}
updateEntryExitStore <- function(entryIterStore, poss2, series, iter) {
#  print(poss2)
#  print(entryIterStore)
  for(i in 1:length(series)){
    if((poss2[i]>0)&&(entryIterStore[1,i]>0)){
      
      entryIterStore[1,i] <- poss2[i]
    
    }else if(poss2[i]<0){
      entryIterStore[1,i] <-0
    }else{
      entryIterStore[1,i] <- poss2[i]+entryIterStore[1,i]
    }
  }
  print(entryIterStore)
  return(entryIterStore)
  
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
              vo=initVoStore(newRowList,series),
              entryiter=initEntryExitStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series,psos,entryAndExit) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$index <- updateIndexStore(store$index,newRowList,series,store$iter)
  store$ppooss<-updatePosStore(store$ppooss,psos,series,store$iter)
  store$vo <- updateVoStore(store$vo,newRowList,series,store$iter)
  store$entryiter <- updateEntryExitStore(store$entryiter,entryAndExit,series,store$iter)
  return(store)
}
###############################################################################
# main strategy logic
calculatePosSizes<-function(clStore,highStore,lowStore,column,iter){
  startIndex <- iter - params$lookback - 1
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  priceHLC<-cbind(highP,lowP,closeP)
  atrA <- ATR(priceHLC,n=14)
  
  #atr_ave1<-mean(atrA[15:1000,2])
  lastAtr<-last(atrA[,"atr"])
  posSizes<-round(10000/lastAtr)
  return(posSizes)
}
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


lgStFt <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  #print(entryAndExit)
#1
  startIndex <- iter - 33 - 1

  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

  MACD1<-MACD(close,12,26,9,maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)))
  macd <- last(MACD1[,1])
  signal <- last(MACD1[,2])
  
#2
  startIndex <- iter - params$lookback - 1
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  pricess<-cbind(closeP,highP,lowP)
  priceSS<-clStore[,column]
  
  cci<-last(CCI(pricess,n=params$lookback))
  
#3
  startIndexS <- iter - params$lookbackS - 1
  rsiS <- last(RSI(clStore[startIndexS:iter,column],n=params$lookbackS))
  
  startIndexL <- iter - params$lookbackL - 1
  rsiL <- last(RSI(clStore[startIndexL:iter,column],n=params$lookbackL))
  
  rsiSlag<-lag(rsiS,1)
  rsiLlag<-lag(rsiL,1)
  
  longs<-ifelse((rsiS>rsiL)&(rsiSlag<rsiLlag),1,0)
  shorts<-ifelse((rsiS<rsiL)&(rsiSlag>rsiLlag),-1,0)
  
#4
  highP<-xts(highStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  lowP<-xts(lowStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  price<-cbind(highP,lowP)
  volume<-xts(voStore[1:iter,column],order.by = as.Date(date[1:ndate]))
  
  emv<-EMV(price,volume)
  emv1 <- last(emv[,1])
  maEMV <- last(emv[,2])
  emv1lag<-lag(emv[,1])
  maEMVlag<-lag(emv[,2])

  posP<-ppooss[1:iter,column]

    long<-ifelse(macd > signal,1,0)
    short<-ifelse(macd < -signal,-1,0)
    long1<-ifelse(cci > params$threshold,1,0)
    short1<-ifelse(cci < -params$threshold,-1,0)
    long2<-ifelse((rsiS < (50 - params$threshold1))||(longs=1),1,0)
    short2<-ifelse((rsiS > (50 + params$threshold1))||(shorts=-1),-1,0)
    long3<-ifelse((emv1>maEMV)&(emv1lag<maEMVlag),1,0)
    short3<-ifelse((emv1<maEMV)&(emv1lag>maEMVlag),-1,0)
    
    longsignals<-ifelse((long1)> 0,1,0)
    short11<-ifelse((short1)< 0,-1,0)
    #print(c(longsignals,short11))
    poss<-rep(0,2)
    poss[1]<-longsignals+short11
    #print(poss)
    #print(entryAndExit[column])
    if(entryAndExit[column]==0){
      if(poss[1]==0){
        return(poss)
      }
      if(poss[1]!=0){
        poss[2]<-iter
        return(poss)
      }
    }
    else if(entryAndExit[column]!=0){
      rett<-getTradeReturn(close,entryAndExit[column],exit = iter,isTRUE(posP[entryAndExit[column]]<0))
     # print(isTRUE(rett>0.1))
      if ((rett > -params$stoploss)&&(rett < params$stoploss)){
        poss[2]<- entryAndExit[column]
        return(poss)
      }else{
        poss[1]<- 0
        poss[2]<- -entryAndExit[column]
        return(poss)
      }
      
      #print(entry)
    }

}