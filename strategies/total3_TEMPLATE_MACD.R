

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
     # if (i==4||i==5||i==7||i==8||i==9){
    #    #KDJ
    #    pos2[params$series[i]] <- lgStFtkdj(store$cl,store$high,store$low,store$index,params$series[i],store$iter)*params$posSizes[params$series[i]]

     # }
      if (i==6){
        #CCI
      ppos[i]<-lgStFtMACD(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss)
      pos[params$series[i]] <- ppos[i]*params$posSizes[params$series[i]]
        
      }
      
    }
    
  }
  #想法：当store$iter==300和store$iter==650的时候，检查PnL的正负，当：1前PNL为正后为负；2前负后负这两种情况时，在650时停止交易
  #想法：当损失超0下一丢丢时，停止交易
  #print(ppos)
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
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              index=initIndexStore(newRowList,series),
              ppooss=initPosStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series,psos) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$index <- updateIndexStore(store$index,newRowList,series,store$iter)
  store$ppooss<-updatePosStore(store$ppooss,psos,series,store$iter)
  return(store)
}
###############################################################################
# main strategy logic
getTradeReturn <- function(prices,entry,exit,short=FALSE){
  prices<-as.numeric(prices)
#  print(short)
#  print(length(prices))
#  print(exit)
#  print(entry)
#  print(prices[exit])
#  print(prices[entry])
  
  if(short)
    return(prices[entry]/prices[exit]-1)
  else
    return(prices[exit]/prices[entry]-1)
}


lgStFtMACD <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss) {
  

  startIndex <- iter - 55 - 1
  #print(head(clStore[startIndex:iter,column]))
  date<-indexStore[startIndex:iter,column]
  ndate<-length(date)
  #print(head(date))
  close<-xts(clStore[startIndex:iter,column],order.by = as.Date(date[1:ndate]))
  #print(head(close))
  MACD1<-MACD(close,params$lookback12,params$lookback26,params$lookback9,maType="EMA")
  macd <- last(MACD1[,1])
  signal <- last(MACD1[,2])
  #print(MACD1)
#  posP<-ppooss[1:iter,column]
#  print(posP)
#  print(posP[iter-1])
#  print(column)
  if(macd>signal)
    return(1)
  if(macd<signal)
    return(-1)
  return(0)
}













