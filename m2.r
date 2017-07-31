library(quantmod)

lst <- c('WIZZ.L','KAZ.L','VED.L','ICP.L','RIO.L','STAN.L','JUP.L','IHG.L')

for (n in lst) {
    obj <- getSymbols(n,src='google',env=NULL)
    # Update quotes
    q <- getQuote(n)
    d <- Sys.Date()
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
    q <- xts(q,d)
    #print(q)
                                        #obj <- merge(obj,q,by="Date")
    obj <- rbind(obj,q)
    print(tail(obj))
    # Chart
    chartSeries(obj,subset='last 9 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
    chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
    }
