# FOR A GENERAL EXPLANATION OF REQUIREMENTS ON getOrders see rsi_contrarian.R 

# Marketmaking strategy
# Places buy and sell limit orders around close price
# Spread is determined by daily range
# Unit position sizes for limit orders
# Uses market order to clear inventory when it becomes too large

# Note: limit orders are automatically cancelled at the end of the day
maxRows <- 3100
getOrders <- function(store, newRowList, currentPos, params) {

    #cat("currentPos", formatC(currentPos,3),"\n")

    # check if current inventory is above a limit and if so exit completely
    # with a market order

    marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)

    # use the range (High-Low) as a indicator for a reasonable "spread" for
    # this pseudo market making strategy
    spread <- sapply(1:length(newRowList),function(i)
                     params$spreadPercentage * (newRowList[[i]]$High -
                                                   newRowList[[i]]$Low))
    
    if (is.null(store)) store <- initStore(newRowList,params$series)
    order1<-rep(0,length(params$series))
    order2<-rep(0,length(params$series))
    ppos<-rep(0,length(params$series))
    entryAndExit1<-rep(0,length(params$series))
    
    if (store$iter > params$lookback) {
      
      for (i in 1:length(params$series)) {
        
        order1[params$series[i]]<-lgStFtcci(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$entryiter)[1]
        order2[params$series[i]]<-lgStFtcci(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$entryiter)[2]
        ppos[params$series[i]]<-lgStFtcci(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$entryiter)[4]
      
        entryAndExit1[params$series[i]]<-lgStFtcci(store$cl,store$high,store$low,store$index,params$series[i],store$iter,store$ppooss,store$entryiter)[3]
      }
      
    }
#   print(ppos)
#    print("-")
#    print(store$iter)
#    print(entryAndExit1)
    store <- updateStore(store, newRowList, params$series,ppos,entryAndExit1)
    limitOrders1  <- order1 # BUY LIMIT ORDERS
    limitPrices1  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close - spread[i]/3)

    limitOrders2  <- order2 # SELL LIMIT ORDERS
    limitPrices2  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close + spread[i]/3)

	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
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
   # print(entryIterStore)
  return(entryIterStore)
  
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series),
              index=initIndexStore(newRowList,series),
              ppooss=initPosStore(newRowList,series),
              entryiter=initEntryExitStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series,psos,entryAndExit) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$index <- updateIndexStore(store$index,newRowList,series,store$iter)
  store$ppooss<-updatePosStore(store$ppooss,psos,series,store$iter)
  store$entryiter <- updateEntryExitStore(store$entryiter,entryAndExit,series,store$iter)
  return(store)
}
###############################################################################
# main strategy logic
getTradeReturn <- function(prices,entry,exit,short=FALSE){
  prices<-as.numeric(prices)
  #  print(short)
  # print(length(prices))
  #  print(prices[exit])
  #  print(prices[entry])
  
  if(short)
    return(prices[entry]/prices[exit]-1)
  else
    return(prices[exit]/prices[entry]-1)
}


lgStFtcci <-	function(clStore,highStore,lowStore,indexStore,column,iter,ppooss,entryAndExit) {
 # print(entryAndExit)
  startIndex <- iter - params$lookback - 1
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  pricess<-cbind(closeP,highP,lowP)
  priceSS<-clStore[,column]
  
  cci<-last(CCI(pricess,n=params$lookback))
  
  orderr<<-rep(0,3)
  posP<-ppooss[1:iter,column]
  #print(posP)
  #  print(posP[iter-1])
  long<-orderr[1]<-ifelse(cci > params$threshold,1,0)
  short<-orderr[2]<-ifelse(cci < -params$threshold,-1,0)
  poss<-long+short
  if(entryAndExit[column]==0){
    
    
    if(poss!=0){
      orderr[3]<-iter
      
    }
#    if(poss!=posP[iter-1]) entry<<-iter
  } else if(entryAndExit[column]!=0){
    
    rett<-getTradeReturn(priceSS,entryAndExit[column],exit = iter,isTRUE(posP[entryAndExit[column]]<0))
    #print(rett)
    if ((rett > -0.2)&&(rett < 0.2 )) {
      orderr[3]<- entryAndExit[column]
      #poss<-posP[iter-1]
      
      if(posP[iter-1]<0){
        orderr[2]<-poss
      }else if(posP[iter-1]>0){
        orderr[1]<-poss
      }
    }else{
      orderr[1]<-0
      orderr[2]<-0
      orderr[3]<- -entryAndExit[column]
      poss<-0
    }
    #else stopOuts[i]=1
  }
  #titStr <- paste("stoploss=", 0.01,":",sum(stopOuts),"stop outs")
  # print(poss)
  return(c(orderr,poss))
}
