
maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) {

    store <- initStore(newRowList, params$series)
  }
  else 
    store <- updateStore(store, newRowList, params$series)
  
  marketOrders <- rep(0,length(newRowList))
  limitOrders1 <- rep(0,length(newRowList))
  limitPrices1 <- rep(0,length(newRowList))
  limitOrders2 <- rep(0,length(newRowList))
  limitPrices2 <- rep(0,length(newRowList))
  
  if (store$iter > 55) {
    marketOrders   <- sapply(1:length(newRowList),
                             function(x) ifelse(x %in% params$series, 
                                                lgStFtMFI(store$cl,which(x==params$series),store$high,store$low,store$vo,store$iter)*params$posSizes[params$series[x]], 0))
  }
  
  # exit positions from yesterday
  marketOrders <- marketOrders - currentPos 
  data_store<<-store
  return(list(store=store,marketOrders=marketOrders,
              limitOrders1=limitOrders1,
              limitPrices1=limitPrices1,
              limitOrders2=limitOrders2,
              limitPrices2=limitPrices2))
}

###############################################################################
# The following function is purely to help to prevent errrors by checking that 
# the requirement parameters are available
###############################################################################

###############################################################################
# All the subsequent functions were designed to simplify and 
# improve the readaility of getNewPos(); 
#
# While not required, this type of function-based approach is advisable 
# and these functions will likely have analogues in your strategies
###############################################################################

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
              vo=initVoStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vo <- updateVoStore(store$vo,newRowList,series,store$iter) 
  return(store)
}

###############################################################################

# main strategy logic

lgStFtMFI <-	function(clStore,column,highStore,lowStore,voStore,iter) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  
  startIndex <- iter - 55 - 1
  

  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  hlc<-cbind(highP,lowP,closeP)
  
  smi<-SMI(hlc,n=13,nFast = 3,nSlow = 39,nSig=9)

  lastsmi<-last(smi[,1])
  signal<-last(smi[,2])
#print(lastsmi)
#  print(signal)
    
  if(lastsmi>signal){
    return(1)
  }else if(lastsmi<signal){
    return(-1)
  }else{
    return(0)
  }
  
}
