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
    if (is.null(store)) store <- initStore(newRowList,params$series)
    marketOrders <- ifelse(abs(currentPos) > params$inventoryLimits, -currentPos, 0)
    allzero  <-rep(0,length(newRowList))
    limitOrders1<-limitOrders2<-limitPrices1<-limitPrices2<-allzero
    # use the range (High-Low) as a indicator for a reasonable "spread" for
    # this pseudo market making strategy
    spread <- sapply(1:length(newRowList),function(i)
                     params$spreadPercentage * (newRowList[[i]]$High -
                                                   newRowList[[i]]$Low))
    if (store$iter > params$lookback) {
    limitOrders1  <- rep(1,length(newRowList)) # BUY LIMIT ORDERS 低于这个价 买
    limitPrices1  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close + calculateDirection(store$cl,params$series[i],store$iter)*
                                        calculatePercentage(store$cl,params$series[i],store$iter)*
                                        newRowList[[i]]$Close - spread[i]/2)

    limitOrders2  <- rep(-1,length(newRowList)) # SELL LIMIT ORDERS 高于这个价 卖
    limitPrices2  <- sapply(1:length(newRowList),function(i) 
                                        newRowList[[i]]$Close + calculateDirection(store$cl,params$series[i],store$iter)*
                                        calculatePercentage(store$cl,params$series[i],store$iter)*
                                        newRowList[[i]]$Close + spread[i]/2)
    
    }
    store <- updateStore(store, newRowList, params$series,ppos)
    #print(store$iter)
	return(list(store=store,marketOrders=marketOrders,
	                        limitOrders1=limitOrders1,
	                        limitPrices1=limitPrices1,
	                        limitOrders2=limitOrders2,
	                        limitPrices2=limitPrices2))
}
##################################
# functions for managing the store
initClStore  <- function(newRowList,series) {
  clStore <- matrix(0,nrow=maxRows,ncol=length(series))
  return(clStore)
}
updateClStore <- function(clStore, newRowList, series, iter) {
 #print(series)
  for (i in 1:length(series))
    clStore[iter,series[i]] <- as.numeric(newRowList[[series[i]]]$Close)
  
  #print(clStore[1:iter,])
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series)
              ))
}
updateStore <- function(store, newRowList, series,psos) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  return(store)
}
######################
# main function
calculatePercentage<-function(clStore,column,iter){
  startIndex <- iter - params$lookback - 1
  percentage<-rep(0,params$lookback)
#  print(column)
  for(i in 1:params$lookback){
    percentage[i]<-last(clStore[startIndex:iter,column])/clStore[iter-params$lookback+i-1,column]-1
  }
  print(column)
  print(percentage)
  averagePercentage<-mean(percentage)
  return(averagePercentage)
}

calculateDirection<-function(clStore,column,iter){
  startIndex <- iter - params$lookback - 1
 # print(column)
#  print(head(clStore,20))
 # print((clStore[startIndex:iter,column]))
  close<-clStore[startIndex:iter,column]
 #print(close)
  sma<-last(SMA(close, params$lookback))
  direction<-ifelse(last(close)>sma,1,-1)
  return(direction)
}