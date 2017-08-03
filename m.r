library(RCurl)
library(jsonlite)
library(plyr)
library(quantmod)


dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}
longDate <- function(d) {return(as.Date(as.character(d),'%b %d, %Y'))}

#' Data convertor from finantial time
toXTS <- function(data){
    d <- cbind(as.Date(apply(data[1],2,longDate)),data[2])
    return(xts(d[,-1], order.by=d[,1]))
    }

##################### Plot last ####################
plotLast <- function(name,start='Jan+01%2C+2017',end='Jun+26%2C+2017'){
x <- getURL(paste("http://www.google.com/finance/historical?q=NASDAQ:",name,"&startdate=",start,"&enddate=",end,"&output=csv",sep=''))
y <- read.csv(text = x)
y <- cbind(as.Date(apply(y[1],2,dT)),y[2:5])
plot(y[[1]],y[[2]],'l')
}
##################### Read bid/ask ##################
# https://github.com/DataWookie/flipsideR.git
#source('~/Desktop/flipsideR-master/R/option-chain.R')
#source('~/Desktop/flipsideR-master/R/configure.R')
########################################################
stat <- function(data='~/Desktop/b886ck92.csv',series='S',subset='last 9 months'){
# '~/Desktop/b8qypr36.csv
    ck92 <- read.csv(data)
    ck <- toXTS(ck92)
    if (series=='S'){
        chartSeries(ck,subset=subset,TA=c(addSMA(),addEMA(30),addMACD()))
        #addMACD()
        #addEMA()
        #addCCI()
        #addRSI()
        }
    else if (series=='w') barplot(weeklyReturn(ck))
    else if (series=='m') barplot(monthlyReturn(ck))
}
#' Quote update
upd <- function(){
    d <- 'GB00B886CK92.L'
    b <- 'GB00B8QYPR36.L'
    uk <- '^UK50'
    print(getQuote(d))
    print(getQuote(b))
    print(getQuote(uk))
}
