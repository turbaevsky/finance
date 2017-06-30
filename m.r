library(RCurl)
library(jsonlite)
library(plyr)
library(quantmod)


dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}
longDate <- function(d) {return(as.Date(as.character(d),'%A, %B %d, %Y'))}

#' Data convertor from finantial time
toXTS <- function(data){
    d <- cbind(as.Date(apply(data[1],2,longDate)),data[2:5])
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
source('~/Desktop/flipsideR-master/R/option-chain.R')
source('~/Desktop/flipsideR-master/R/configure.R')
########################################################
ck92 <- read.csv('~/Desktop/b886ck92.csv')
ck <- toXTS(ck92)
chartSeries(ck)
addSMA()
weeklyReturns(ck)
