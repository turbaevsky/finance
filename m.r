library(RCurl)
library(jsonlite)
library(plyr)


dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}

##################### Plot last ####################
plotLast <- function(){
x <- getURL("http://www.google.com/finance/historical?q=NASDAQ:GOOG&startdate=Jan+01%2C+2017&enddate=Jun+26%2C+2017&output=csv")
y <- read.csv(text = x)
y <- cbind(as.Date(apply(y[1],2,dT)),y[2:5])
plot(y[[1]],y[[2]],'l')
}
##################### Read bid/ask ##################
# https://github.com/DataWookie/flipsideR.git
source('~/Desktop/flipsideR-master/R/option-chain.R')
source('~/Desktop/flipsideR-master/R/configure.R')
########################################################
AAPL = getOptionChain("GOOG")
print(head(AAPL))
