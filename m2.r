library(quantmod)
#library(RCurl)
library(jsonlite)
library(telegram)

bot <- TGBot$new(token = '183104961:AAFOVTLmfQ0MDHdt2ZnLgtUZYkM_gbDFkLs')
bot$set_default_chat_id(181982455)

j <- fromJSON('list.json')
sp500 <- j$sp500
ftse250 <- j$ftse250
ftse350 <- j$ftse350
ftas <- j$ftas
excluded <- j$excluded

j <- fromJSON('monitor.json')
shortlst <- j$shortlst
lim <- j$lim # Stop-loss limit
stocks <- j$stocks # No of stocks
tax <- j$tax # Taxes
long <- j$long # Buying price

#####################################################################
### Functions ###
#####################################################################
addSSTO <- newTA(stoch,HLC,col=c(4,5,6),type=c('n','l','l'))
#####################################################################
gQuote <- function(n='LON:FXPO'){ # get quotes from google (real-time)
                                        #url <- paste('http://finance.google.com/finance/info?client=ig&q=',n,sep='')
    url <- paste('https://finance.google.com/finance?q=',n,'&output=json',sep='')
    data <- getURL(url)
    json <- fromJSON(gsub('\n|[//]|[\\]','',data))
    return(json)
}
#####################################################################
updQuote <- function(obj,n,google=FALSE) #Update price table by actual quotes
{
    q <- getQuote(paste(n,'.L',sep=''))
    #print(q)
    #q <- na.fill(q)
    d <- as.Date(q[[1]])
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
################### Insert google quotes (gQuote) ###############
    if (google){
        cls <- gQuote(paste('LON:',n,sep=''))
        cls <- as.numeric(cls$l)
        q$Close <- cls
        if (q$High<cls) q$High <- cls
        if (q$Low>cls) q$Low <- cls
        }
##################################################
    q <- xts(q,d)
    obj <- rbind(obj[,1:5],q)
    obj <- obj[!duplicated(index(obj),fromLast = TRUE ),]
    return(obj)
}
#####################################################################
dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}
longDate <- function(d) {return(as.Date(as.character(d),'%b %d, %Y'))}

#' Data convertor from finantial time
toXTS <- function(data){
    d <- cbind(as.Date(apply(data[1],2,longDate)),data[2])
    return(xts(d[,-1], order.by=d[,1]))
}
#####################################################################
#' Quote update
upd <- function(){
    d <- 'GB00B886CK92.L'
    b <- 'GB00B8QYPR36.L'
    uk <- '^UK50'
    print(getQuote(d))
    print(getQuote(b))
    print(getQuote(uk))
}
#####################################################################
#####################################################################
analyse <- function(lst=ftas,quotes=TRUE,web=FALSE,change=5,adx=25,k=0.05,ssto=0.9,google=FALSE,src='google',telegram=FALSE,long=TRUE){
# change = price change for 3 weeks in percent, adx = ADX low limit, k = koeff. for MACD and SSTO crossing
# ssto - SSTO D% high limit
    for (n in lst) {
        if (!n %in% excluded) {
        try({
            print(paste('Getting',n,'from',src))
            obj <- getSymbols(paste(n,'.L',sep=''),src=src,env=NULL)
            obj <- na.omit(obj)

            if (quotes) obj <- na.omit(updQuote(obj,n,google=google))
            adx <- last(ADX(obj[,c(2,3,4)]))$ADX[[1]]
            macd <- MACD(obj[,4],12,26,9,maType='EMA')
            lmacd <- last(macd)$macd[[1]]
            lsig <- last(macd)$signal[[1]]
            div <- macd$macd-macd$signal
            sma200 <- last(SMA(obj[,4],200))[[1]]
            cls <- last(obj[,4])[[1]]
            low <- last(obj[,3])[[1]]
            sma <- last(SMA(obj[,4],10))[[1]]
            lsma <- SMA(obj[,4])
            lsma50 <- lsma[length(lsma)-50][[1]]
            change50 <- (sma-lsma50)/sma*100
            lsma15 <- lsma[length(lsma)-15][[1]]
            change15 <- (sma-lsma15)/sma*100
            lsma5 <- lsma[length(lsma)-5][[1]]
            change5 <- (sma-lsma5)/sma*100
            ema <- last(EMA(obj[,4],30))[[1]]
            atr <- last(ATR(obj))$atr[[1]]
            pct <- last(BBands(obj[,4]))$up[[1]]
            pct <- (pct-cls)/pct
            rsi <- last(RSI(obj[,4]))[[1]]
            bol <- last(BBands(obj[,c(2:4)]))
            # Check unusial volume
            lastV <- head(tail(obj[,5],2),1)[[1]]
            avgV <- mean(unlist(head(tail(obj[,5],32),30)))[[1]] # Avg of last 30 elements
            factor <- lastV/avgV
            p <- length(div)
            st <- last(stoch(HLC(obj))) # low stochastic, %K, %D and lowD
############################################################################
### IF module ###
############################################################################
            if ((n %in% shortlst) ||
                ((avgV>=1e5 && cls>sma200 && cls>=5
		        && adx>=adx && change15>change) &&
		        #(((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd) ||
		        ((1+k)*st[[2]]>=st[[3]] && (1-k)*st[[2]]<=st[[3]] && st[[2]]<=ssto)
                )){
############################################################################
                cond <- ''
                if (div[p-3][[1]]>div[p-2][[1]] && div[p-2][[1]]<div[p-1][[1]] && div[p-1][[1]]<div[p][[1]]) cond <- paste(cond,'two risen divs,')
                if (div[p-2][[1]]>div[p-1][[1]] && div[p-1][[1]]<div[p][[1]])
                    cond <- paste(cond,'hole divs,')
                if ((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd) cond <- paste(cond,'macd crossed signal,')
                if ((1+k)*st[[2]]>=st[[3]] && (1-k)*st[[2]]<=st[[3]]) cond <- paste(cond,'SSTO crossed')

                print(paste('Plotting',n,'because of',cond))
                msg <- paste('adx=',round(adx),'atr=',signif(atr,2),'rsi14(30long/70short)=',round(rsi),'change,% 1w/3w/3m=',signif(change5,2),'/',signif(change15,2),'/',signif(change50,2),',SSTO:fastD=',signif(st$fastD,3),'slowD=',signif(st$slowD,3))
                print(msg)
                if (web==TRUE)
                    #browseURL(paste('https://finance.yahoo.com/calendar/earnings?day=',format(Sys.Date(),'%Y-%m-%d'),'&symbol=',n,sep=''))
                    print(gQuote(n)$events)
                print(tail(obj[,1:5]))
                print(tail(merge(div,macd)))
                if (quotes) try(print(getOptionChain(n)))
                if (n %in% shortlst){
                    no <- which(shortlst == n)
                    balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
                    print(paste('Balance=',signif(balance,3)))
                }
                no <- which(shortlst == n)
# Chart ##
# MACD could be (5,34,5) to see Elliot's wave
                if (!telegram && long){
                chartSeries(obj,subset='last 9 months',TA=c(addSMA(200),addMACD(),addSMA(),addEMA(30),addVo()),multi.col=FALSE,name=n,log.scale=T)
                plot(addLines(h=lim[no],col='red'))
                invisible(readline(prompt="Press [enter] to continue"))
                }

                if (telegram) png('test.png')
                chartSeries(obj,subset='last 3 months',TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
                #plot(addTA(stoch(HLC(obj))),col=c('red','blue','green'))
                plot(addSSTO())
                plot(addLines(h=lim[no],col='red'))
                if (telegram) {
                    dev.off()
                    bot$sendPhoto('test.png', caption=paste(n,cond,msg))
                         }
                if (!telegram) invisible(readline(prompt="Press [enter] to continue"))
            }})}}}
##############################################################################
ticker <- function(name='FXPO',min=10,subset='last 3 months',src='google',
                   telegram=FALSE,google=FALSE,chart=FALSE){
    while(TRUE){
    for (n in name){
        obj <- getSymbols(paste(n,'.L',sep=''),src=src,env=NULL)
        obj <- na.omit(obj)
        q <- getQuote(paste(n,'.L',sep=''))
        obj <- updQuote(obj,n,google=google)
        st <- last(stoch(HLC(obj))) # low stochastic, %K, %D and lowD
                                        #print(getQuote(n))
        if (n %in% shortlst){
            no <- which(shortlst == n)
            if (!google){
                cls <- q[[2]]
                tm <- q[[1]]
            }
            else{
                cls <- as.numeric(gQuote(n)$l)
                tm <- 'real-time'
                }
            balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
            msg1 <- paste(n,format(Sys.time(),'%H:%M'),'Bal=',signif(balance,3),'Last=',cls,'(',gQuote(n)$cp,'%) Limit=',lim[no],'(',tm,')')
            if (cls<=lim[no]) msg1 <- paste('!',msg1)
            #if (google) msg <- paste(n,gQuote(n),'SSTO fastD=',signif(st$fastD,3),'slowD=',signif(st$slowD,3))
            if (telegram) bot$sendMessage(msg1)
            if (google) print(msg1)
            else print(msg1)
        }
        if (chart){
            chartSeries(obj,subset=subset,TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
            plot(addLines(h=lim[no],col='red'))
            plot(addSSTO())
            }
        Sys.sleep(60/length(name)*min)
    }}
}
##############################################################################
### Funds charting using csv ###
##############################################################################
stat <- function(data='~/Desktop/b886ck92.csv',series='S',subset='last 6 months'){
# '~/Desktop/b8qypr36.csv
    ck92 <- read.csv(data)
    ck <- toXTS(ck92)
    if (series=='S'){
        chartSeries(ck,subset=subset,TA=c(addSMA(),addEMA(30),addSMA(200),addMACD()))
    }
    else if (series=='w') barplot(weeklyReturn(ck))
    else if (series=='m') barplot(monthlyReturn(ck))
}
##############################################################################
dayTicker <- function(n='FXPO',min=5,imitate=FALSE,google=FALSE){
    f <- data.frame()
    while (TRUE){
        for (t in 1:min){
            if (google){
                j <- gQuote(paste('LON:',n,sep=''))
                #dt <- strptime(j$lt,"%b %d, %I:%M%p")
                dt <- Sys.time()
                val <- as.numeric(j$l)
                        }
            else {
                j <- getQuote(n)
                dt <- j$Trade
                val <- j$Last
                  }
            if (imitate){
                dt <- Sys.time()
                val <- runif(1,200,400)
                }
            if (t==1) {
                price <- data.frame(dt,val,val,val,val)
                names(price) <- c('Date','Open','High','Low','Close')
                        }
            else if (t==min) price$Close <- val
            if (val>price$High) price$High <- val
            if (val<price$Low) price$Low <- val
            if (imitate) Sys.sleep(1)
            else Sys.sleep(60)
            }
        f <- rbind(f,price)
        x <- xts(f[2:5],f$Date)
        print(nrow(x))
        if (nrow(x)>2 && nrow(x)<=10) chartSeries(x)
        else if (nrow(x)>10 && nrow(x)<=30) chartSeries(x,TA=c(addSMA()))
        else if (nrow(x)>30) chartSeries(x,TA=c(addSMA(),addEMA(30),addSSTO()))
    }
##############################################################################
}
