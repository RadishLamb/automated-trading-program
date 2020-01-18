
maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  allzero  <- stopOuts <- rep(0,length(newRowList)) # used for initializing vectors
  
  marketOrders <- -currentPos; pos <- allzero
  pos1 <- allzero; pos2 <- allzero; pos3 <- allzero
  
  if (is.null(store)) store <- initStore(newRowList,params$series)
  
  ppos<-rep(0,length(params$series))


  
  
  if (store$iter > 55) {

    for (i in 1:length(params$series)) {
    #  if (i==1||i==2||i==3||i==10){
      #RSI
        #pos1[params$series[i]] <- lgStFtrsi(store$cl,params$series[i],store$iter)*params$posSizes[params$series[i]]

     # }
      if (i==1||i==3||i==10||i==7){
        #KDJ
        pos2[params$series[i]] <- lgStFtmarketorderGroup2(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo)*params$posSizes[params$series[i]]

      }
      if (i==2||i==5||i==6||i==8||i==9||i==4){
     #   print(i)
      ppos[i]<-lgStFtmarketorderGroup1(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$vo)
      pos[params$series[i]] <- ppos[i]*params$posSizes[params$series[i]]
        
      }
      
    }
    
  }
 print(store$iter)
#  print(ppos)
  store <- updateStore(store, newRowList, params$series,ppos)
  # exit positions from yesterday
  marketOrders <- marketOrders + pos1 +pos2+pos3+ pos

  #cat(formatC(store$count,2),'\t',formatC(pos,2),'\n')
  #cat(formatC(pos,2),'\n')
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=allzero,limitPrices1=allzero,
              limitOrders2=allzero,limitPrices2=allzero))
}
#######################################
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
  store$vo <- updateVoStore(store$vo,newRowList,series,store$iter)
  return(store)
}
###############################################################################
#                          main strategy logic
###############################################################################

  #####
  # 1 # Calculate Trade Return - for Stop Loss
  ##################################################

getTradeReturn <- function(prices,entry,exit,short=FALSE){
  prices<-as.numeric(prices)

  if(short)
    return(prices[entry]/prices[exit]-1)
  else
    return(prices[exit]/prices[entry]-1)
}

  #####
  # 2 # Main Logic of Market Order - Group 1 
  ##################################################

lgStFtmarketorderGroup1 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore) {
  
  long<-0
  short<-0
  
  ### 
  ## get prices

  startIndex <- iter - 55 - 1

  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  close<-xts(clStore[startIndex:iter,column],order.by = as.Date(date[1:ndate]))

  ###
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
  
  ###
  ## Group1.2 calculate MFI
  
  volume<-voStore[startIndex:iter,column]
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  hlc<-cbind(highP,lowP,closeP)
  
  mfi<-last(MFI(hlc,volume,n=14))
  
  if (mfi > 85){
    short<-short+1
  }else if (mfi < 15){
    long<-long+1
  }
  
  ###
  ## Group1.3 calculate RSI

  rsiS <- last(RSI(clStore[1:iter,column],n=10))
  if (rsiS > 50 + 29){
    short<-short+1
  }else if (rsiS < 50 - 29){
    long<-long+1
  }
  
  ###
  ## Group1.4 calculate ADX
  
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
#  print(long)
#  print(short)
#  print("---")
  ######
  #####
  return(ifelse(long>=short,1,-1))
  
}


  #####
  # 3 # Main Logic of Market Order - Group 2 
  ##################################################

lgStFtmarketorderGroup2 <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,voStore) {
  
  long<-0
  short<-0
  
  ###
  ## get prices
  
  startIndex <- iter - 55 - 1
  
  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  
  close<-xts(clStore[startIndex:iter,column],order.by = as.Date(date[1:ndate]))
  
  ###
  ## Group2.1 calculate VWAP
  fastVwap<-VWAP(clStore[startIndex:iter,column],voStore[startIndex:iter,column],16)
  lastfastVwap<-last(fastVwap)
  slowVwap<-VWAP(clStore[startIndex:iter,column],voStore[startIndex:iter,column],42)
  lastslowVwap<-last(slowVwap)
  
  if(lastfastVwap>lastslowVwap){
    long<-long+3
  }else if(lastfastVwap<lastslowVwap){
    short<-short+3
  }
    
  ###
  ## Group2.2 calculate BIAS
  msma<-last(SMA(clStore[startIndex:iter,column],29))
  mema<-last(EMA(clStore[startIndex:iter,column],10))
  
  mbias<- ((mema-msma)/msma)*100	
  
  if(mbias>0 && mbias<16){
    long<-long+1
  }else if(mbias<0 && mbias>-16){
    short<-short+1
  }
  
  
  ###
  ## Group2.3 calculate KST
  kksstt <- KST(clStore[1:iter,column],n=c(10,10,10,15),nROC = c(10,15,20,30),nSig = 9)
  kst <- last(kksstt[,1])
  signal <- last(kksstt[,2])
  
  if(kst>0&&kst>signal){
    long<-long+1
  }else if(kst<0&&kst<signal){
    short<-short+1
  }
  
  ######
  return(ifelse(long>=short,1,-1))
}

  #####
  # 4 # Main Logic of Limit Order
  ##################################################

