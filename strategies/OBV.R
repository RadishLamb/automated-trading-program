

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {
  
  if (is.null(store)) {
    store <- initStore(newRowList,params$series)
  }
  else 
    store <- updateStore(store, newRowList,params$series)
  
  marketOrders <- rep(0,length(newRowList))
  limitOrders1 <- rep(0,length(newRowList))
  limitPrices1 <- rep(0,length(newRowList))
  limitOrders2 <- rep(0,length(newRowList))
  limitPrices2 <- rep(0,length(newRowList))
  
  if (store$iter > params$lookback) {
    marketOrders   <- sapply(1:length(newRowList),
                             function(x) ifelse(x %in% params$series,
                                                lgStFt(store$cl,which(x==params$series),store$iter,store$vo)*params$posSizes[params$series[x]],0))
  }
#  print(store$iter)
  # exit positions from yesterday
  marketOrders <- marketOrders - currentPos 
  
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
  return(list(iter=0,cl=initClStore(newRowList,series),vo=initVoStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter)
  store$vo <- updateClStore(store$vo,newRowList,series,store$iter) 
  return(store)
}

###############################################################################

# main strategy logic

lgStFt <-	function(clStore,column,iter,voStore) {
  # decide if we should go long/short/flat (returning 1/-1/0)
  startIndex <- iter- params$lookback - 1

  volume <- voStore[startIndex:iter,column]

  OBV_value<-OBV(clStore[startIndex:iter,column],volume)
  #OBV_value<- na.omit(cumsum(ifelse(diffClose>=0,1,-1)*volume))
 # print(OBV_value)
 # tendOBV<-last(OBV_value[startIndex:params$lookback])-first(OBV_value[startIndex:params$lookback])
 # tendcl<-last(clStore[startIndex:params$lookback,column])-first(clStore[startIndex:params$lookback,column])
  
  close<-clStore[startIndex:iter,column]

  sma<-last(SMA(close, params$lookbackLONG))
  shortsma<-last(SMA(close, params$lookbackSHORT))
  tendcl<-ifelse(shortsma>sma,1,-1)
  
  smaOBV<-last(SMA(OBV_value,params$lookbackLONG))
  shortsmaOBV<-last(SMA(OBV_value,params$lookbackSHORT))
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
