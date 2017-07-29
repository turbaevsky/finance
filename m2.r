library(quantmod)

lst <- c('WIZZ.L','KAZ.L','VED.L','ICP.L','RIO.L','STAN.L','JUP.L','IHG.L')

for (n in lst) {
    obj <- getSymbols(n,src='google',env=NULL)
    chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
    chartSeries(obj,subset='last 9 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
    }
