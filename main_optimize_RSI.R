source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/total3_TEMPLATE.R') 

numOfDays <- 300
dataList <- getData(directory="PART1")
dataList <- lapply(dataList, function(x) x[251:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSSeq <- seq(from=6,to=6,by=3)   
lookbackLSeq <- seq(from=24,to=27,by=1)
thresholdSeq  <- seq(from=25,to=25,by=5) 
paramsList  <- list(lookbackSSeq,lookbackLSeq,thresholdSeq)
numberComb <- prod(sapply(paramsList,length))

resultsMatrix <- matrix(nrow=numberComb,ncol=4)
colnames(resultsMatrix) <- c("lookbackS","lookbackL","threshold","PD Ratio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lbS in lookbackSSeq) {
  for (lbL in lookbackLSeq) {
    for (tres in thresholdSeq) {
        params <- list(lookbackS=lbS,lookbackL=lbL,threshold=tres,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)) 
        results <- backtest(dataList, getOrders, params, sMult)
        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lbS,lbL,tres,pfolioPnL$fitAgg)
        pfolioPnLList[[count]]<- pfolioPnL
        cat("Just completed",count,"out of",numberComb,"\n")
        print(resultsMatrix[count,])
        count <- count + 1
    }
  }
}
print(resultsMatrix[order(resultsMatrix[,"PD Ratio"]),])
