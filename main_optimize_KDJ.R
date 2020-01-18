source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/kst_TEMPLATE.R') 

numOfDays <- 500
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

slfSeq <- seq(from=-0.3,to=0.3,by=0.15)   

paramsList  <- list(slfSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=2)
colnames(resultsMatrix) <- c("slf","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbS in slfSeq) {

 #   for (tres in thresholdSeq) {
        params <- list(slf=lbS,series=1:10) 
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lbS,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
  #  }
  
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
