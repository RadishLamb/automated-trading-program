source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/simple_limit.R') 

numOfDays <- 2000
dataList <- getData(directory="PART12")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSSeq <- seq(from=3,to=7,by=2)   
lookbackLSeq <- seq(from=19,to=38,by=3)
spreadPercentageSeq<-seq(from=0.00001,to=0.00001,by=0.0000)

paramsList  <- list(lookbackSSeq,lookbackLSeq,spreadPercentageSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=4)
colnames(resultsMatrix) <- c("lookbackSHORT","lookbackLONG","spreadPercentage","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbS in lookbackSSeq) {
  for (lbL in lookbackLSeq) {
    for(spr in spreadPercentageSeq){
        params <- list(lookback=10,spreadPercentage=spr,inventoryLimits=rep(30000,10),lookbackSHORT=lbS,lookbackLONG=lbL,series=1:10)
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lbS,lbL,spr,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }
  }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
