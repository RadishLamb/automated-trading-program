#相对强弱指标
library(quantmod)
DATA11<-read.csv('01.csv',header = TRUE)
head(DATA11)
DATA1<-xts(DATA11[,-1],order.by = as.Date(DATA11$Index))
head(DATA1)
tail(DATA1)

#提取收盘价数据
DATA1clp<-DATA1[,4]
names(DATA1clp)<-"DATA1clp"
head(DATA1clp,3)

#计算收盘价的价格变化量
clprcChange<-diff(DATA1clp)
names(clprcChange)<-"clprcChange"
head(clprcChange)

#upPrc表示价格上涨
upPrc<-ifelse(clprcChange>0,clprcChange,0)
names(upPrc)<-"upPrc"
downPrc<-ifelse(clprcChange<0,abs(clprcChange),0)

rsidata<-merge(DATA1clp,clprcChange,upPrc,downPrc)
head(rsidata)

#构造简单平均数，计算六日的上涨力度UP和下跌力度DOWN，
#分别为SMUP和SMDOWN，计算六日RSI

rsidata<-rsidata[-1,]
rsidataM<-coredata(rsidata)
head(rsidataM)
class(rsidataM)
len<-nrow(rsidataM)
len

SMUP<-rep(0,len)
SMDOWN<-rep(0,len)
upPrcM<-rsidataM[,3]
downPrcM<-rsidataM[,4]

for (i in 6:len) {
  SMUP[i]<-mean(upPrcM[(i-5):i],na.rm = TRUE)
  SMDOWN[i]<-mean(downPrcM[(i-5):i],na.rm = TRUE)
}

SMUP[SMUP==0]<-NA
SMDOWN[SMDOWN==0]<-NA

rsidataUD<-merge(rsidata,SMUP,SMDOWN)
rsidataUD<-na.omit(rsidataUD)

tail(rsidataUD)

SMUP1<-rsidataUD$SMUP
SMDOWN1<-rsidataUD$SMDOWN

rsi6<-100*SMUP1/(SMUP1+SMDOWN1)
names(rsi6)<-"rsi6"
rsiUDR<-merge(rsidataUD,rsi6)
head(rsiUDR)

summary(rsi6)

plot.zoo(rsiUDR[,-(2:4)],col = c("red","blue","yellow","green"),
         main="RSI Indicators")
#用TTR包计算RSI
library(TTR)

RSI6<-RSI(DATA1clp,n=6,maType = "SMA")
RSI6<-na.omit(RSI6)
all(RSI6==rsi6)

#RSI短期向上突破长期，黄金交叉；短期向下突破长期，死亡交叉。
#短期RSI为6，长期为24
RSI24<-RSI(DATA1clp,n=24,maType = "SMA")
RSI24<-na.omit(RSI24)
head(RSI24)
class(RSI6)
par(mfrow=c(2,1))
plot(DATA1clp,type = "l",
     main="收盘价",
     ylab="收盘价")
plot(RSI6,type = "l",main="RSI黄金交叉和死亡交叉",
     ylab="RSI")
lines(RSI24,col="red",lty=6,lwd=2)
legend("bottomright",legend = c("RSI6","RSI24"),
       col = c("black","red"),
       lty = c(1,6),cex=0.7)
#看出短期比较波动大，长期的平滑。
#RSI6大于80时，预期股价下跌，RSI6小于20时，可能回升。
#RSI6大多数情况下上穿RSI24，出现不少“黄金交叉”

#策略：
#当RSI6>80或者RSI6下穿RSI24时，卖出。
#当RSI6<20或者RSI6上穿RSI24时，买入。
##为什么是“或者”而不是“且”呢？

longsig1<-ifelse(RSI6<20,1,0)
names(longsig1)<-"longsig1"
head(longsig1[longsig1==1],4)

shortsig1<-ifelse(RSI6>80,-1,0)
names(shortsig1)<-"shortsig1"
head(shortsig1[shortsig1==(-1)],4)

rsi6lag<-lag(RSI6,1)
rsi24lag<-lag(RSI24,1)
head(rsi24lag)
RSIDATA<-merge(RSI6,rsi6lag,RSI24,rsi24lag)
RSIDATA<-na.omit(RSIDATA)
head(RSIDATA)

longsig2<-apply(RSIDATA, 1, function(x){
  ifelse((x[1]>x[3])&(x[2]<x[4]),1,0)
})
head(longsig2,20)

longsig2<-xts(longsig2,order.by = index(RSIDATA))
names(longsig2)<-"longsig2"

shortsig2<-apply(RSIDATA,1,function(x){
  ifelse((x[1]<x[3])&(x[2]>x[4]),-1,0)
})
head(shortsig2,30)

shortsig2<-xts(shortsig2,order.by = index(RSIDATA))
names(shortsig2)<-"shortsig2"

AllSignal<-merge(longsig1,longsig2,shortsig1,shortsig2,all = TRUE)
AllSignal<-na.omit(AllSignal)
head(AllSignal)
tail(AllSignal)

ComboSignal<-apply(AllSignal, 1, sum)
ComboSignal<-xts(ComboSignal,order.by = index(AllSignal))
head(ComboSignal)

longsig<-ifelse(ComboSignal>=1,1,0)
shortsig<-ifelse(ComboSignal<=(-1),-1,0)


ret<-ROC(DATA1clp,type = "discrete")
head(ret)
buy<-lag(longsig,1)
sell<-lag(shortsig,1)
trade<-lag(ComboSignal,1)
head(trade)
buyRet<-buy*ret
sellRet<-sell*ret
tradeRet<-trade*ret

plot.zoo(cbind(buyRet,sellRet,tradeRet),screens = c(1,1,2),
         xlab = NULL, ylab=c("buy/sell","trade"),
         lty = c(1,2,1),
         col=c("green","red","blue"),
         main = "RSI策略")

#预测成功吗？
strat<-function(signal,ret){
  RSIRet<-signal*ret
  WinRate<-length(RSIRet[RSIRet>0])/length(RSIRet[RSIRet!=0])
  meanWin<-mean(RSIRet[RSIRet>0])
  meanloss<-mean(RSIRet[RSIRet<0])
  return(c(WinRate,meanWin,meanloss))
}

Buy<-strat(buy,ret)
Sell<-strat(sell,ret)
Trade<-strat(trade,ret)
Test<-rbind(Buy,Sell,Trade)
colnames(Test)<-c("WinRate","meanWin","meanloss")

library(PerformanceAnalytics)
names(ret)<-"stockRet"
names(tradeRet)<-"tradeRet"
charts.PerformanceSummary(cbind(ret,tradeRet),lty=c(1,4),
                          main = "RSI指标交易")