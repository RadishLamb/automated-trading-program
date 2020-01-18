example_strategies <- c("fixed", 
                        "bbands",
                        "copycat", 
                        "random", 
                        "OBV", 
                        "rsi_TEMPLATE",
                        "KDJ_TEMPLATE",
                        "kst_TEMPLATE",
                        "CCI_TEMPLATE",
                        "aaadx_TEMPLATE",
                        "total_TEMPLATE",
                        "total2_TEMPLATE",
                        "total3_TEMPLATE_RSI",
                        "total3_TEMPLATE_KDJ",
                        "total3_TEMPLATE_CCI",
                        "total3_TEMPLATE_KDJ_stoploss",
                        "total3_TEMPLATE_CCI_stoploss",
                        "total3_TEMPLATE_MACD_stoploss",
                        "total3_TEMPLATE_EMV_stoploss",
                        "total3_TEMPLATE_MACD",
                        "bbands_trend_following",
                        "bbands_contrarian",
                        "bbands_holding_period",
                        "simple_limit",
                        "Final_Strategy_1",
                        "GMMA",
                        "BIAS",
                        "VWAP",
                        "MFI",
                        "team4",
                        "TRIX",
                        "SMI"
                        )

example_params <- list(
                    "fixed"=list(sizes=rep(1,10)),
                    "random"=list(maxLots=100),
                    "team4"=list(spreadPercentage=0.6,inventoryLimits=rep(30000,10),stoploss=0.5,targetProfit=0.6,series=1:10),
                    "bbands"=list(series=1:10,lookback=50,sdParam=2,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "copycat"=NULL,
                    "OBV"=list(lookback=80,lookbackSHORT=38,lookbackLONG=50,threshold=25,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "kst_TEMPLATE"=list(series=1:10,slf=-0.3,atr_lookback=50),
                    "GMMA"=list(series=1:10),
                    "TRIX"=list(series=1:10,lookback=21,nSig=15,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "MFI"=list(series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "SMI"=list(series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "VWAP"=list(series=1:10),
                    "BIAS"=list(series=1:10,slf=0.1,atr_lookback=20),
                    "CCI_TEMPLATE"=list(series=1:10,threshold=100,lookback=20,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "aaadx_TEMPLATE"=list(series=1:10,lookback=7,threshold=36,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "total_TEMPLATE"=list(series=1:10),
                    "total2_TEMPLATE"=list(series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "total3_TEMPLATE_RSI"=list(lookbackS=6,lookbackL=25,threshold=27,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    
                    #"total3_TEMPLATE_RSI"=list(lookbackS=6,threshold=30,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "total3_TEMPLATE_KDJ"=list(series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=11,threshold=25),
                    "total3_TEMPLATE_KDJ_stoploss"=list(stoploss=0.2,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=11,threshold=25),
                    
                    "total3_TEMPLATE_CCI"=list(lookbackS=6,lookbackL=25,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=27,threshold=105,thresholdrsi=30),
                    "total3_TEMPLATE_CCI_stoploss"=list(stoploss=0.3,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=27,threshold=105),
                    "total3_TEMPLATE_MACD_stoploss"=list(stoploss=0.5,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143)),
                    "total3_TEMPLATE_EMV_stoploss"=list(stoploss=0.5,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=27,threshold=105),
                    "total3_TEMPLATE_MACD"=list(lookback12=19,lookback26=36,lookback9=2,series=1:10,posSizes=c(2457,211,343666,1470,1441,8407,5321,1728,1268,143),lookback=27,threshold=105),
                    "rsi_TEMPLATE"=list(lookbackS=6,lookbackL=25,threshold=30,series=1:10),
                    "KDJ_TEMPLATE"=list(lookback=8,threshold=25,series=1:10),
                    "bbands_contrarian"=list(lookback=20,sdParam=1.5,series=1:4,posSizes=rep(1,10)),
                    "bbands_trend_following"=list(lookback=50,sdParam=1.5,series=c(1,3,5,7,8,9),posSizes=rep(1,10)),
                    "bbands_holding_period"=list(lookback=50,sdParam=1.5,series=c(1,3),posSizes=rep(1,10),holdPeriod=6),
                    "simple_limit"=list(spreadPercentage=0.1,inventoryLimits=rep(30000,10),series=1:10,lookbackS=6,threshold=27),
                    "Final_Strategy_1"=list(lookbackS=6,lookbackL=25,threshold1=30,stoploss=0.5,series=1:10,lookback=27,threshold=105))

load_strategy <- function(strategy) {

    strategyFile <- file.path('strategies', paste0(strategy,'.R'))

    # load strategy
    cat("Sourcing",strategyFile,"\n")
    source(strategyFile) # load in getOrders

    # set params
    params <<- example_params[[strategy]]
    print("Parameters:")
    print(params)
}
