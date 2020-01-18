source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/CCI_TEMPLATE.R') 

numOfDays <- 2000
dataList <- getData(directory="PART12")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=7,to=27,by=3)   

thresholdSeq  <- seq(from=100,to=100,by=5) 
paramsList  <- list(lookbackSeq,thresholdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbS in lookbackSeq) {

    for (tres in thresholdSeq) {
        params <- list(lookback=lbS,threshold=tres,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)) 
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
