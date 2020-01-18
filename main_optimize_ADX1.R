source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/aaadx_TEMPLATE.R') 

numOfDays <- 100
dataList <- getData(directory="PART12")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=5,to=14,by=2)   

thresholdSeq  <- seq(from=30,to=37,by=2) 
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
print(resultsMatrix)
print(resultsMatrix[order(resultsMatrix[,"PDRatio"]),])
#library(lattice) # needed for levelplot
#library(gridExtra)
library(reshape)
#fRange <- range(resultsMatrix[,3])
#at <- seq(from=fRange[1],to=fRange[2],length.out=100)

#plot1 <- levelplot(PDRatio ~ lookback * threshold,resultsMatrix, at=at,
#                   main="In-sample",col.regions=topo.colors(100))
#plot2 <- levelplot(fitness ~ n * thresh,resultsOut, at=at,
#                   main="Out-of-sample",col.regions=topo.colors(100))

resultsMatrix<-as.data.frame(resultsMatrix)
newdata <- melt(resultsMatrix,id.vars=c("lookback","threshold"),measure.vars="PDRatio")
mat <- cast(newdata, formula= lookback ~ threshold)
mat <- as.matrix(mat[,2:ncol(mat)])
levels<-seq(0,by=0.1,to=max(mat))
pdf('contour.pdf')
filled.contour(lookbackSeq,sdParamSeq,mat,levels=levels,col=rainbow(length(levels)),
               plot.title = title(xlab = "lookback", ylab = "threshold"),
               key.title = title(main="Log\nReturn"))
dev.off()

#print(resultsMatrix[order(resultsMatrix[,"PDRatio"]),])