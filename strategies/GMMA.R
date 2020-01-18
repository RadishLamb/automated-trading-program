

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {

	if (is.null(store)) {

		store <- initStore(newRowList, params$series)
	}
	else 
		store <- updateStore(store, newRowList, params$series)
	ppos<-rep(0,length(params$series))
	possizes<-rep(0,length(params$series))
	pos<-rep(0,length(params$series))
    marketOrders <- rep(0,length(newRowList))
    limitOrders1 <- rep(0,length(newRowList))
    limitPrices1 <- rep(0,length(newRowList))
    limitOrders2 <- rep(0,length(newRowList))
    limitPrices2 <- rep(0,length(newRowList))

  if (store$iter > 65) {
    for (i in 1:length(params$series)) {
     # atr_m<-calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)[2]
    #  StopLossUp<-max(store$high[(store$iter-params$atr_lookback):store$iter,i])-params$slf*atr_m
    #  StopLossDn<-min(store$low[(store$iter-params$atr_lookback):store$iter,i])+params$slf*atr_m
#      print(store$cl[store$iter,i])
     # if(store$cl[store$iter,i]<StopLossUp && store$cl[store$iter,i]>StopLossDn)
      #{
        ppos[i] <- lgStFt(store$cl,params$series[i],store$iter)
        possizes[i]<-calculatePosSizes(store$cl,store$high,store$low,params$series[i],store$iter)[1]

        pos[params$series[i]] <- ppos[i]*possizes[i]
        
      #}
    }
  }
   # print(ppos)
    #print(store$iter)
    # exit positions from yesterday
    marketOrders <- marketOrders +pos- currentPos 

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

initStore <- function(newRowList,series) {
  return(list(iter=0,cl=initClStore(newRowList,series),
              high=initHighStore(newRowList,series),
              low=initLowStore(newRowList,series)))
}
updateStore <- function(store, newRowList, series) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList,series,store$iter) 
  store$high <- updateHighStore(store$high,newRowList,series,store$iter)
  store$low <- updateLowStore(store$low,newRowList,series,store$iter)
  return(store)
}
###############################################################################

# main strategy logic

calculatePosSizes<-function(clStore,highStore,lowStore,column,iter){
  startIndex <- iter - 65 - 1
  
  closeP<-clStore[startIndex:iter,column]
  highP<-highStore[startIndex:iter,column]
  lowP<-lowStore[startIndex:iter,column]
  
  priceHLC<-cbind(highP,lowP,closeP)
  atrA <- na.omit(ATR(priceHLC,n=14))
  
  #atr_ave1<-mean(atrA[15:1000,2])
  lastAtr<-mean(atrA[,"atr"])
  posSizes<-round(10000/lastAtr)

  return(c(posSizes,lastAtr))
}
lgStFt <-	function(clStore,column,iter) {
	# decide if we should go long/short/flat (returning 1/-1/0)
	startIndex <- iter - 65 - 1

  gmma<-GMMA(clStore[startIndex:iter,column], short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60))
  lastgmma<-last(gmma)
  
  #print(isTRUE(kst>signal))
	longs<-ifelse(lastgmma[1]>lastgmma[2]&&
	                lastgmma[3]>lastgmma[4]&&
	                lastgmma[5]>lastgmma[6]&&
	                lastgmma[7]>lastgmma[8]&&
	                lastgmma[9]>lastgmma[10],1,0)
	shorts<-ifelse(lastgmma[1]<lastgmma[2]&&
	                 lastgmma[3]<lastgmma[4]&&
	                 lastgmma[5]<lastgmma[6]&&
	                 lastgmma[7]<lastgmma[8]&&
	                 lastgmma[9]<lastgmma[10],-1,0)
	#print(longs)
	#print(shorts)
	return(longs+shorts)
    
}
