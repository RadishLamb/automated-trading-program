source('framework/data.R'); 
source('framework/backtester.R')
source('framework/processResults.R'); 
source('strategies/bbands.R') 

numOfDays <- 56
dataList <- getData(directory="PART12")
dataList <- lapply(dataList, function(x) x[1:numOfDays])
sMult <- 0.2 # slippage multiplier

lookbackSeq <- seq(from=50,to=51,by=1)   
sdParamSeq<- seq(from=1,to=2,by=1)   

paramsList  <- list(lookbackSeq,sdParamSeq)
numberComb <- prod(sapply(paramsList,length))
##
params <- expand.grid(lookback=lookbackSeq,sdParam=sdParamSeq)

resultsMatrix <- matrix(nrow=numberComb,ncol=3)
colnames(resultsMatrix) <- c("lookback","sdParam","PDRatio")
pfolioPnLList <- vector(mode="list",length=numberComb)

count <- 1
for (lb12 in lookbackSeq) {

    for (lb9 in sdParamSeq) {
        params <- list(lookback=lb12,sdParam=lb9,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)) 
        results <- backtest(dataList, getOrders, params, sMult)

        pfolioPnL <- plotResults(dataList,results)
        resultsMatrix[count,] <- c(lb12,lb9,pfolioPnL$fitAgg)
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
newdata <- melt(resultsMatrix,id.vars=c("lookback","sdParam"),measure.vars="PDRatio")
mat <- cast(newdata, formula= lookback ~ sdParam)
mat <- as.matrix(mat[,2:ncol(mat)])
levels<-seq(0,by=0.1,to=max(mat))
pdf('contour.pdf')
filled.contour(lookbackSeq,sdParamSeq,mat,levels=levels,col=rainbow(length(levels)),
               plot.title = title(xlab = "lookback", ylab = "sdParam"),
               key.title = title(main="Log\nReturn"))
dev.off()

#print(resultsMatrix[order(resultsMatrix[,"PDRatio"]),])
