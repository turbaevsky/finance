library(RCurl)
dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}

x <- getURL("http://www.google.com/finance/historical?q=NASDAQ:GOOG&startdate=Jan+01%2C+2009&enddate=Aug+2%2C+2012&output=csv")
y <- read.csv(text = x)
y <- cbind(as.Date(apply(y[1],2,dT)),y[2:5])
plot(y[[1]],y[[2]],'l')
