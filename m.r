library(RCurl)
dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}

x <- getURL("http://www.google.com/finance/historical?q=NASDAQ:GOOG&startdate=Jan+01%2C+2017&enddate=Jun+26%2C+2017&output=csv")
y <- read.csv(text = x)
y <- cbind(as.Date(apply(y[1],2,dT)),y[2:5])
plot(y[[1]],y[[2]],'l')
