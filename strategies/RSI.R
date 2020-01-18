#���ǿ��ָ��
library(quantmod)
DATA11<-read.csv('01.csv',header = TRUE)
head(DATA11)
DATA1<-xts(DATA11[,-1],order.by = as.Date(DATA11$Index))
head(DATA1)
tail(DATA1)

#��ȡ���̼�����
DATA1clp<-DATA1[,4]
names(DATA1clp)<-"DATA1clp"
head(DATA1clp,3)

#�������̼۵ļ۸�仯��
clprcChange<-diff(DATA1clp)
names(clprcChange)<-"clprcChange"
head(clprcChange)

#upPrc��ʾ�۸�����
upPrc<-ifelse(clprcChange>0,clprcChange,0)
names(upPrc)<-"upPrc"
downPrc<-ifelse(clprcChange<0,abs(clprcChange),0)

rsidata<-merge(DATA1clp,clprcChange,upPrc,downPrc)
head(rsidata)

#�����ƽ�������������յ���������UP���µ�����DOWN��
#�ֱ�ΪSMUP��SMDOWN����������RSI

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
#��TTR������RSI
library(TTR)

RSI6<-RSI(DATA1clp,n=6,maType = "SMA")
RSI6<-na.omit(RSI6)
all(RSI6==rsi6)

#RSI��������ͻ�Ƴ��ڣ��ƽ𽻲棻��������ͻ�Ƴ��ڣ��������档
#����RSIΪ6������Ϊ24
RSI24<-RSI(DATA1clp,n=24,maType = "SMA")
RSI24<-na.omit(RSI24)
head(RSI24)
class(RSI6)
par(mfrow=c(2,1))
plot(DATA1clp,type = "l",
     main="���̼�",
     ylab="���̼�")
plot(RSI6,type = "l",main="RSI�ƽ𽻲����������",
     ylab="RSI")
lines(RSI24,col="red",lty=6,lwd=2)
legend("bottomright",legend = c("RSI6","RSI24"),
       col = c("black","red"),
       lty = c(1,6),cex=0.7)
#�������ڱȽϲ����󣬳��ڵ�ƽ����
#RSI6����80ʱ��Ԥ�ڹɼ��µ���RSI6С��20ʱ�����ܻ�����
#RSI6�����������ϴ�RSI24�����ֲ��١��ƽ𽻲桱

#���ԣ�
#��RSI6>80����RSI6�´�RSI24ʱ��������
#��RSI6<20����RSI6�ϴ�RSI24ʱ�����롣
##Ϊʲô�ǡ����ߡ������ǡ��ҡ��أ�

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
         main = "RSI����")

#Ԥ��ɹ���
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
                          main = "RSIָ�꽻��")