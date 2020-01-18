source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/OBV.R') 

numOfDays <- 2000
dataList <- getData(directory="PART12")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSHORTSeq <- seq(from=3,to=15,by=2)   #15往后可以继续测。11,27 13,27 15,45 11,24 为正。
lookbackLONGSeq  <- seq(from=18,to=46,by=3) #细一点 46还可以往后

paramsList  <- list(lookbackSHORTSeq,lookbackLONGSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookbackSHORT","lookbackLONG","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbS in lookbackSHORTSeq) {
  
  for (tres in lookbackLONGSeq) {
    params <- list(lookbackSHORT=lbS,lookbackLONG=tres,series=1:10,lookback=50,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)) 
    results <- backtest(dataList, getOrders, params, sMult)
    pfolioPnL <- plotResults(dataList,results)
    resultsMatrix[count,] <- c(lbS,tres,pfolioPnL$fitAgg)
    pfolioPnLList[[count]]<- pfolioPnL
    cat("Just completed",count,"out of",numberComb,"\n")
    print(resultsMatrix[count,])
    count <- count + 1
  }
  
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
