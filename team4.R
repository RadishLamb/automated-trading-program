

#####################################################################
# Notes:
#   1. There are 2 limit orders and 1 market order
#   2. Estimated run time for 1000 days is about ¡¾10 minutes¡¿
#   3. Need 'xts', 'zoo', 'TTR', 'quantmod' ...

params <- list(spreadPercentage=0.6,inventoryLimits=rep(30000,10),
               stoploss=0.5,targetProfit=0.6,series=1:10)


maxRows <- 3100 

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- stopOuts <- rep(0,length(newRowList)) 
  
  marketOrders <- -currentPos
  pos1 <- allzero; pos2 <- allzero; pos3 <- allzero 
  pos4 <- allzero; pos5 <- allzero; pos <- allzero
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  ppos<-rep(0,length(params$series))           #ppos: unit position vector
  entryAndExit1<-rep(0,length(params$series))  
  # entryAndExit1: record entry iter for calculating return rate
  # Stop loss and target profit based on entryAndExit1

  limitOrders1<-limitOrders2<-limitPrices1<-limitPrices2<-allzero
  
  spread <- sapply(1:length(newRowList),function(i)
    params$spreadPercentage * (newRowList[[i]]$High -
                                 newRowList[[i]]$Low))
  
  if (store$iter > 80) {

    for (i in 1:length(params$series)) {
      
      #######################  Market Order #################
      
      if (i==5||i==8){
      # GROUP 5 - OBV (params:11,30) &&  ADX
        pos5[params$series[i]] <- lgStFtmarketorderGroup5(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]*calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
        entryAndExit1[i]<-lgStFtmarketorderGroup5(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      }
      if (i==4||i==10){
      # GROUP 4 - Bias
        pos4[params$series[i]] <- lgStFtmarketorderGroup4(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]*calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
        entryAndExit1[i]<-lgStFtmarketorderGroup4(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      }
      if (i==2||i==3){
      # GROUP 3 - OBV (params:38,50) && TRIX
        pos3[params$series[i]] <- lgStFtmarketorderGroup3(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]*calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
        entryAndExit1[i]<-lgStFtmarketorderGroup3(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      }
      if (i==7){
      # GROUP 2 - VWAP; Bias
        pos2[params$series[i]] <- lgStFtmarketorderGroup2(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]*calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
        entryAndExit1[i]<-lgStFtmarketorderGroup2(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      }
     
      if (i==1||i==9||i==6){
      # GROUP 1 - ADX; MACD
      ppos[i]<-lgStFtmarketorderGroup1(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[1]
      pos[params$series[i]] <- ppos[i]*calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)
      entryAndExit1[i]<-lgStFtmarketorderGroup1(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo,store$entryiter)[2]
      }
     
      #######################  Limit Orders #################
      #i==1||i==2||i==4||i==5||i==6||i==7||i==8||i==9||i==10
      
      if(i==4||i==8||i==9||i==10){
        limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS
        limitPrices1  <- sapply(1:length(newRowList),function(i)
          (abs(newRowList[[i]]$Close + newRowList[[i]]$Open))/2 + calculateDirection(store$cl,params$series[i],store$iter,store$vo)*
            calculatePercentage(store$cl,params$series[i],store$iter,store$op)*(abs(newRowList[[i]]$Close + newRowList[[i]]$Open))/2 - spread[i]/2)
        
        limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS
        limitPrices2  <- sapply(1:length(newRowList),function(i)
          (abs(newRowList[[i]]$Close + newRowList[[i]]$Open))/2 + calculateDirection(store$cl,params$series[i],store$iter,store$vo)*
            calculatePercentage(store$cl,params$series[i],store$iter,store$op)*(abs(newRowList[[i]]$Close + newRowList[[i]]$Open))/2 + spread[i]/2)
        
      } 
    }
    
  }
# print(store$iter) Used to see the progress made

  store <- updateStore(store, newRowList, params$series,ppos,entryAndExit1)

  marketOrders <- marketOrders + pos1 +pos2+pos3+pos4+pos5+ pos


  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,limitPrices2=limitPrices2))
}


########################################################################
#                   functions for managing the store                   #
########################################################################

initOpStore  <- function(newRowList,series) {
  OpStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(OpStore)
}
updateOpStore <- function(OpStore, newRowList, series, iter) {
  #print(series)
  for (i in 1:length(series))
    OpStore[iter,series[i]] <- as.numeric(newRowList[[series[i]]]$Open)
  
  #print(clStore[1:iter,])
  return(OpStore)
}
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
#  print(entryIterStore)
  return(entryIterStore)
  
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              index=initIndexStore(newRowList,series),
              ppooss=initPosStore(newRowList,series),
              vo=initVoStore(newRowList,series),
              entryiter=initEntryExitStore(newRowList,series),
              op=initClStore(newRowList,series)))
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
  store$op <- updateOpStore(store$op,newRowList,series,store$iter)
  return(store)
}

###############################################################################
#                          main strategy logic                                #
#                                                                             #
###############################################################################


  ######################################################
  # 1/9 .  Calculate Position Sizes
  ######################################################
calculatePosSizes<-function(clStore,highStore,lowStore,column,iter){
  startIndex <- iter - 80 - 1
  
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
  ######################################################
  # 2/9 .  Calculate Trade Return - for Stop Loss
  ######################################################

getTradeReturn <- function(prices,entry,exit,short=FALSE){
  prices<-as.numeric(prices)

  if(short)
    return(prices[entry]/prices[exit]-1)
  else
    return(prices[exit]/prices[entry]-1)
}

  ##########################################################
  # 3/9 . Main Logic of Market Order - Group 1 - ADX,MACD
  ##########################################################

lgStFtmarketorderGroup1 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  
  long<-0
  short<-0
  
  ## get prices

  startIndex <- iter - 80 - 1

  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  close<-xts(clStore[startIndex:iter,column],order.by = as.Date(date[1:ndate]))


  ## Group1.1 calculate MACD

  MACD1<-MACD(close,15,35,6,maType="EMA")
  macd <- last(MACD1[,1])
  signal <- last(MACD1[,2])

  if(macd>signal){
    long <- long +1
  }
  else if(macd<signal){
    short <- short +1
  }

  
  ## Group1.2 calculate ADX
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  pricess<-cbind(highP,lowP,closeP)
  
  adx <-na.omit(ADX(pricess,n=7))
  pdi<-last(na.omit(adx[,1]))
  ndi<-last(na.omit(adx[,2]))
  aadx<-last(na.omit(adx[,4]))
  
  longs<-ifelse(pdi>ndi,1,0)
  shorts<-ifelse(pdi<ndi,-1,0)
  
  
  if ((aadx < 36)&&(shorts=-1)){
    long<-long+2
  }else if ((aadx > 36)&&(longs=1)){
    short<-short+2
  }

  
  ######  

  posP<-ppooss[1:iter,column]
  poss<-rep(0,2)
  poss[1]<-ifelse(long>=short,1,-1)
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

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

    if ((rett > -params$stoploss)&&(rett < params$targetProfit)){
      poss[2]<- entryAndExit[column]
      return(poss)
    }else{
      poss[1]<- 0
      poss[2]<- -entryAndExit[column]
      return(poss)
    }
    

  }
  
}

  ##################################################################
  # 4/9 . Main Logic of Market Order - Group 2 - VWAP + BIAS
  ##################################################################

lgStFtmarketorderGroup2 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  
  long<-0
  short<-0
  
  ## get prices
  
  startIndex <- iter - 80 - 1
  
  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  
  close<-xts(clStore[startIndex:iter,column],order.by = as.Date(date[1:ndate]))
  
  ## Group2.1 calculate VWAP
  fastVwap<-VWAP(clStore[startIndex:iter,column],voStore[startIndex:iter,column],18)
  lastfastVwap<-last(fastVwap)
  slowVwap<-VWAP(clStore[startIndex:iter,column],voStore[startIndex:iter,column],42)
  lastslowVwap<-last(slowVwap)
  
  if(lastfastVwap>lastslowVwap){
    long<-long+3
  }else if(lastfastVwap<lastslowVwap){
    short<-short+3
  }
    
  ## Group2.2 calculate BIAS
  msma<-last(SMA(clStore[startIndex:iter,column],29))
  mema<-last(EMA(clStore[startIndex:iter,column],10))
  
  mbias<- ((mema-msma)/msma)*100	
  
  if(mbias>0 && mbias<16){
    long<-long+1
  }else if(mbias<0 && mbias>-16){
    short<-short+1
  }
  

  ######  

  posP<-ppooss[1:iter,column]
  poss<-rep(0,2)
  poss[1]<-ifelse(long>=short,1,-1)
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

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

    if ((rett > -params$stoploss)&&(rett < params$targetProfit)){
      poss[2]<- entryAndExit[column]
      return(poss)
    }else{
      poss[1]<- 0
      poss[2]<- -entryAndExit[column]
      return(poss)
    }

  }
}
  
  ##########################################################################
  # 5/9 . Main Logic of Market Order - Group 3 - OBV(params:38,50) && TRIX
  ##########################################################################

lgStFtmarketorderGroup3 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  
  long<-0
  short<-0
  ## get prices
  
  startIndex <- iter - 80 - 1
  
  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  
  close<-clStore[startIndex:iter,column]
  volume <- voStore[startIndex:iter,column]

  ## Group3.1 calculate OBV
  OBV_value<-OBV(clStore[startIndex:iter,column],volume)
  
  sma<-last(SMA(close, 50))
  shortsma<-last(SMA(close, 38))
  tendcl<-ifelse(shortsma>sma,1,-1)
  
  smaOBV<-last(SMA(OBV_value,50))
  shortsmaOBV<-last(SMA(OBV_value,38))
  tendOBV<-ifelse(shortsmaOBV>smaOBV,1,-1)
  
  if ((tendOBV< 0)&&(tendcl<0))
    short<-short+1    # trend confirmation, go short
  if ((tendOBV>0)&&(tendcl>0))
    long<-long+1    # trend confirmation, go long
  if ((tendOBV>0)&&(tendcl<0))
    long<-long+1   # trend divergence, go long
  if ((tendOBV<0)&&(tendcl>0))
    short<-short+1   # trend devergence, go short


  ## Group3.2 calculate TRIX
  TRIX<-TRIX(close,n=21,nSig = 15,maType=list(list(SMA), list(SMA), list(SMA), list(SMA)))
  trix<-last(TRIX[,1])
  ssignal<-last(TRIX[,2])
  
  direction<-ifelse(trix>ssignal,1,-1)
  if(trix>ssignal){
    long<-long+2
  }
  if(trix<ssignal){
    short<-short+2 
  }

  ######  

  posP<-ppooss[1:iter,column]
  poss<-rep(0,2)
  poss[1]<-ifelse(long>=short,1,-1)
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

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
    if ((rett > -params$stoploss)&&(rett < params$targetProfit)){
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
  ##############################################################
  # 6/9 . Main Logic of Market Order - Group 4 - Bias
  ##############################################################

lgStFtmarketorderGroup4 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  long<-0
  short<-0
  

  ## get prices
  
  startIndex <- iter - 80 - 1
  
  
  ## Group4.1 calculate BIAS
  msma<-last(SMA(clStore[startIndex:iter,column],40))
  mema<-last(EMA(clStore[startIndex:iter,column],9))
  
  mbias<- ((mema-msma)/msma)*100	
  
  if(mbias>0 && mbias<22){
    short<-short+1
  }else if(mbias<0 && mbias>-22){
    long<-long+1
  }
  
  
  ######  

  posP<-ppooss[1:iter,column]
  poss<-rep(0,2)
  poss[1]<-ifelse(long>=short,1,-1)
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

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

    if ((rett > -params$stoploss)&&(rett < params$targetProfit)){
      poss[2]<- entryAndExit[column]
      return(poss)
    }else{
      poss[1]<- 0
      poss[2]<- -entryAndExit[column]
      return(poss)
    }
    

  }
}
  #########################################################################
  # 7/9 . Main Logic of Market Order - Group 5 - OBV(params:11,30) && ADX
  #########################################################################

lgStFtmarketorderGroup5 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore,entryAndExit) {
  
  OBV<-0
  ADXmark<-0
  

  ## get prices
  
  startIndex <- iter - 80 - 1

  
  close<-clStore[startIndex:iter,column]
  volume <- voStore[startIndex:iter,column]
  

  ## Group5.1 calculate OBV
  
  OBV_value<-OBV(clStore[startIndex:iter,column],volume)
  
  sma<-last(SMA(close, 44))
  shortsma<-last(SMA(close, 34))
  tendcl<-ifelse(shortsma>sma,1,-1)
  
  smaOBV<-last(SMA(OBV_value,44))
  shortsmaOBV<-last(SMA(OBV_value,34))
  tendOBV<-ifelse(shortsmaOBV>smaOBV,1,-1)
  
  if ((tendOBV< 0)&&(tendcl<0))
    OBV<- -1    # trend confirmation, go short
  if ((tendOBV>0)&&(tendcl>0))
    OBV<- 1     # trend confirmation, go long
  if ((tendOBV>0)&&(tendcl<0))
    OBV<- 1     # trend divergence, go long
  if ((tendOBV<0)&&(tendcl>0))
    OBV<- -1     # trend devergence, go short

  

  ## Group5.2 calculate ADX
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  pricess<-cbind(highP,lowP,closeP)
  
  adx <-na.omit(ADX(pricess,n=7))
  pdi<-last(na.omit(adx[,1]))
  ndi<-last(na.omit(adx[,2]))
  aadx<-last(na.omit(adx[,4]))
  
  longs<-ifelse(pdi>ndi,1,0)
  shorts<-ifelse(pdi<ndi,-1,0)
  
  
  if ((aadx < 36)&&(shorts=-1)){
    ADXmark<-1
  }else if ((aadx > 36)&&(longs=1)){
    ADXmark<- -1
  }


  ######  

  posP<-ppooss[1:iter,column]
  poss<-rep(0,2)

  if(OBV==1&&ADXmark==1){
    poss[1]<-1
  }else if(OBV==-1&&ADXmark==-1){
    poss[1]<- -1
  }else{
    poss[1]<-0
  }
  
  date<-indexStore[1:iter,column]
  ndate<-length(date)
  close<-xts(clStore[1:iter,column],order.by = as.Date(date[1:ndate]))

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

    if ((rett > -params$stoploss)&&(rett < params$targetProfit)){
      poss[2]<- entryAndExit[column]
      return(poss)
    }else{
      poss[1]<- 0
      poss[2]<- -entryAndExit[column]
      return(poss)
    }

  }
}
##############################################################################
#                           Main Logic of Limit Order                        #
##############################################################################


###############################################
# 8/9 . calculate Percentage of Price's Change
###############################################
calculatePercentage<-function(clStore,column,iter,opStore){
  startIndex <- iter - 5 - 1
  percentage<-rep(0,5)
  

  for(i in 1:4){
    priceNextDay <- (clStore[startIndex+i+1,column]+opStore[startIndex+i+1,column])/2
    priceToday <- (clStore[startIndex+i,column]+opStore[startIndex+i,column])/2
    percentage[i] <- priceNextDay/priceToday-1
  }

  averagePercentage<-mean(percentage)
  return(averagePercentage)
}
###############################################
# 9/9 . calculate Direction of Price's Change
###############################################
calculateDirection<-function(clStore,column,iter,voStore){
  startIndex <- iter - 55 - 1
  
  volume <- voStore[startIndex:iter,column]
  
  OBV_value<-OBV(clStore[startIndex:iter,column],volume)
  close<-clStore[startIndex:iter,column]
  
  sma<-last(SMA(close, 21))
  shortsma<-last(SMA(close, 15))
  tendcl<-ifelse(shortsma>sma,1,-1)
  
  smaOBV<-last(SMA(OBV_value,21))
  shortsmaOBV<-last(SMA(OBV_value,15))
  tendOBV<-ifelse(shortsmaOBV>smaOBV,1,-1)
  
  if ((tendOBV< 0)&&(tendcl<0))
    return(-1) # trend confirmation, go short
  if ((tendOBV>0)&&(tendcl>0))
    return(1)  # trend confirmation, go long
  if ((tendOBV>0)&&(tendcl<0))
    return(1)  # trend divergence, go long
  if ((tendOBV<0)&&(tendcl>0))
    return(-1)  # trend devergence, go short
  return(0)
  
}