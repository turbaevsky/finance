library(quantmod)
library(RCurl)
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
                                        # based on http://www.marketcalls.in/database/google-realtime-intraday-backfill-data.html and https://stackoverflow.com/questions/46070126/google-finance-json-stock-quote-stopped-working
#q= stock symbol on Google finance
#x= exchange symbol
#i= interval (here 60 means 60 sec (1 minute interval))
#p= no of period(here 5d denotes 5 days of data)
#f= parameters (day, close, open, high and low)
#df= difference (cpct is may be in % change )
#auto =1,
#ts = time start… if you cut the last 4 digits…the rest gives the start day in seconds

u2time <- function(unixtime){
    #print(unixtime)
    return(as.character(as.POSIXct(unixtime, origin="1970-01-01")))
}

gData <- function(n='FXPO.L',period='15d',i=3600,data='d,o,h,l,c,v',starttime=Sys.time()){
    starttime <- as.numeric(substr(as.character(as.numeric(starttime)),1,10))
    url <- paste('https://finance.google.com/finance/getprices?q=',n,'&p=',period,'&f=',data,'&i=',i,'&ts=',starttime,sep='')
    d <- read.table(url,sep=',',header=F,skip=7)
    val <- as.numeric(substr(d[1,1],2,11))
    t <- apply(d[1],2,function(x) val+as.numeric(x)*i)
    t[1,1] <- val
    d <- cbind(t,d[2:6])
    t <- apply(d[1],2,u2time)
    d <- cbind(t,d[2:6])
    rownames(d) <- d[,1]
    d <- d[2:6]
    colnames(d) <- c('Close','High','Low','Open','Volume')
    d <- as.xts(d)
    return(d)
    }


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


gQuote2 <- function(n='FXPO.L'){
    url <- paste('https://finance.google.com/finance/getprices?q=',n,'&p=3m&f=c&i=60',sep='')
    d <- tail(read.table(url,sep=',',header=F,skip=7),1)[[1]]
    return(d)
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
        #cls <- gQuote(paste('LON:',n,sep=''))
        #cls <- as.numeric(cls$l)
        cls <- gQuote2(paste(n,'.L',sep=''))
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
                if (web==TRUE){
                    #browseURL(paste('https://finance.yahoo.com/calendar/earnings?day=',format(Sys.Date(),'%Y-%m-%d'),'&symbol=',n,sep=''))
                    webinfo <- gQuote(paste('LON:',n,sep=''))
                    print(paste(webinfo$name,':',webinfo$sname,':',webinfo$iname))
                    print(webinfo$events)
                    }
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
                   telegram=FALSE,chart=FALSE){
    while(TRUE){
        for (n in name){
            nn <- paste(n,'.L',sep='')
            obj <- getSymbols(nn,src=src,env=NULL)
            obj <- na.omit(obj)
            obj <- updQuote(obj,n,google=TRUE)
            st <- last(stoch(HLC(obj))) # low stochastic, %K, %D and lowD
                                        #print(getQuote(n))
        if (n %in% shortlst){
            no <- which(shortlst == n)
            cls <- gQuote2(nn)
            balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
            msg1 <- paste(n,format(Sys.time(),'%H:%M'),'Bal=',signif(balance,3),'Last=',cls,'Limit=',lim[no])
            if (cls<=lim[no]) msg1 <- paste('!',msg1)
            if (telegram) bot$sendMessage(msg1)
            print(msg1)
        }
        if (chart){
            chartSeries(obj,subset=subset,TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
            if (n %in% shortlst)
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
dayTicker <- function(names='FXPO',min=5,period='2d'){
    while (TRUE){
        for (name in names){
            n <- paste(name,'.L',sep='')
            ser <- gData(n=n,period=period,i=min*60)
            chartSeries(ser,TA=c(addSMA(),addEMA(30),addMACD(),addSSTO()),name=name)
            if (name %in% shortlst){
                no <- which(shortlst == name)
                plot(addLines(h=lim[no],col='red'))
            }
            plot(addLines(v=grep("16:30",index(ser)),col='green'))
            Sys.sleep(60/length(names)*min)
    }
    }
    }
##############################################################################
