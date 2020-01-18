####################################################################################
# STRATEGY CODE
#
# getOrders is the interface between the backtester and your strategies
####################################################################################

# It's arguments and return list should stay the same.

####################################################################################
# ARGUMENTS 
####################################################################################

# store: see explanation under returned list below

# newRowList: this is a list of single-row OHLCV xts objects, one for each series

# currentPos: this is the current position (so if one wants to close all
# positions one could set marketOrders as -currentPos)

# params: these are the strategy params

####################################################################################
# RETURNED LIST
####################################################################################

# This function must return a list with the the following named memebers

# store
# marketOrders
# limitOrders1
# limitPrices1
# limitOrders2
# limitPrices2

# store contains data required by the strategy is in the variable store
# store is returned by the function and passed back to it the next time 
# it is called by the backetester.

# marketOrders is a vector containing the number of lots to be traded at the 
# open for each series that the
# strategy will hold on the current day
#
# The i'th entry of the integer vector pos (of length nseries) represents 
# the number of contracts you want to be long (short) on the next trading day 
# if the entry is positive (negative).

# Entries of marketOrders, limitOrders1, and limitOrders2 should be integers 
################################################################################

# This strategy uses only market orders

# The strategy will go short if RSI is > 50 + threshold 
#                      long  if RSI is < 50 - threshold
# and will be flat otherwie

# This is a contrarian (a.k.a. mean-reversion) type of strategy
################################################################################

maxRows <- 3100 # used to initialize a matrix to store closing prices
# set maxRows as the number of rows in data (it can be larger but should not be smaller)

getOrders <- function(store, newRowList, currentPos, params) {

	if (is.null(store)) {
		checkParams(params)
		store <- initStore(newRowList, params$series)
	}
	else 
		store <- updateStore(store, newRowList, params$series)
	
    marketOrders <- rep(0,length(newRowList))
    limitOrders1 <- rep(0,length(newRowList))
    limitPrices1 <- rep(0,length(newRowList))
    limitOrders2 <- rep(0,length(newRowList))
    limitPrices2 <- rep(0,length(newRowList))

	if (store$iter > params$lookback) {
	  
		marketOrders   <- sapply(1:length(newRowList),
		                        function(x) ifelse(x %in% params$series, 
		                                lgStFt(store$cl,store$high,store$low,store$index,which(x==params$series),store$iter), 0))
	  
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

checkParams <- function(params) { # make sure params are correct
    if (!"lookback" %in% names(params))
        stop("Parameter lookback not defined for strategy KDJ")
    if (!"threshold" %in% names(params))
        stop("Parameter threshold not defined for strategy KDJ")
    if (params$threshold < 0 || params$threshold > 50)
        stop("Parameter threshold is not between 0 and 50")
}

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
	# decide if we should go long/short/flat (returning 1/-1/0)
  #print(iter)
	startIndex <- iter - params$lookback - 1
  
#	Close<-clStore[startIndex:iter,column]
	closeP<-clStore[startIndex:iter,column]

	highP<-highStore[startIndex:iter,column]
	lowP<-lowStore[startIndex:iter,column]
	
	date<-indexStore[startIndex:iter,column]
	ndate<-length(date)

	perHigh<-max(highP)
	perLow<-min(lowP)
	RSVV<-100*(closeP-perLow)/(perHigh-perLow)
#  print(perHigh)
#  print(perLow)
#  print(head(RSVV))

	#rsv<-c(50,50,as.numeric(RSVV))
	#print(length(rsv))
	#print(head(rsv))
	RSV1<-na.omit(xts(RSVV,order.by = as.Date(date[1:ndate])))
	#print(head(RSV1))
	KValue<-EMA(RSV1,n=2,ratio = 1/3)
	KValue[1]<-50
	DValue<-na.omit(EMA(KValue,n=2,ratio = 1/3))
	KValue<-KValue[-(1:2)]
	DValue<-DValue[-(1:2)]
	#print(head(KValue))
	
	k<-ifelse(KValue<20,1,0)
	KSignal<-ifelse(KValue>70,-1,k)
	d<-ifelse(DValue<20,1,0)
	DSignal<-ifelse(DValue>70,-1,d)

	KDSignal<-apply(merge(KSignal,DSignal),1,sum)
	KDSignal<-na.omit(KDSignal)
	KDSignal<-last(KDSignal)
	#print(KDSignal)
	#KDSignal<-xts(KDSignal,order.by = index(KSignal))


	if (KDSignal<=-1)
        return(-1) # short
	if (KDSignal>=1)
        return(1)  # long
    return(0)
}
