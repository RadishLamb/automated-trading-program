
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
		                                lgStFt(store$cl,store$high,store$low,store$index,which(x==params$series),store$iter)*params$posSizes[params$series[x]], 0))
    }


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

lgStFt <-	function(clStore,highStore,lowStore,indexStore,column,iter) {


	startIndex <- iter - 55 - 1

	closeP<-clStore[startIndex:iter,column]

	highP<-highStore[startIndex:iter,column]
	lowP<-lowStore[startIndex:iter,column]
	

	
	pricess<-cbind(highP,lowP,closeP)
	cci<-CCI(pricess,n=params$lookback)
  lastcci<-last(cci)
  ccilag1<-cci[length(cci)-1]
  ccilag2<-cci[length(cci)-2]
  ccilag3<-cci[length(cci)-3]
#  print(CCI(pricess,n=params$lookback))
#  print(lastcci)
#  print(ccilag)

#从上往下突破+100，市场上涨阶段结束。
  if(lastcci<params$threshold && ccilag1>params$threshold &&
     ccilag2>params$threshold && ccilag3>params$threshold){
    return(-1)
#从下往上突破+100，强势状态，买。
  }else if(lastcci>params$threshold && ccilag1>params$threshold && lastcci>ccilag1){
    return(1)
#从下往上突破-100，市场探底阶段结束。    
  }else if(lastcci> -params$threshold && ccilag1< -params$threshold &&
           ccilag2 < -params$threshold && ccilag3 < -params$threshold){
    return(1)
  }else if(lastcci< -params$threshold && ccilag1< -params$threshold && lastcci<ccilag1){
    return(-1)
  }else{
    return(0)
  }
  
  
}
