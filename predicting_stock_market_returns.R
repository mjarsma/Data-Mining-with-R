###################################################
### The Available Data
###################################################
library(DMwR)
data(GSPC)

###################################################
### Handling time dependent data in R
###################################################
# xts-package is used for handling time dependent data
library(xts)
# the first argument that the xts() receives is the time series data.
# the second argument is used for providing time tags: the seq()
# generates sequences of numbers.
x1 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by="day"))
x1[1:5]
x2 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len=100,by="min"))
x2[1:4]
x3 <- xts(rnorm(3),as.Date(c('2005-01-01','2005-01-10','2005-01-12')))
x3

x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["20000105"]
x1["2000-04"]
x1["2000-03-27/"]
x1["2000-02-26/2000-03-03"]
x1["/20000103"]

# creating multiple time series:
mts.vals <- matrix(round(rnorm(25),2),5,5)
colnames(mts.vals) <- paste('ts',1:5,sep='')
mts <- xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04',
                                 '2003-01-05','2003-01-06','2003-02-16')))
mts
mts["2003-01",c("ts2","ts5")]
index(mts)
coredata(mts)


###################################################
### Reading the data from the CSV file
###################################################
GSPC <- as.xts(read.zoo('sp500.csv',header=T))


###################################################
### Getting the data from the Web
###################################################
library(tseries)
GSPC <- as.xts(get.hist.quote("^GSPC",start="1970-01-02",
                              quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
head(GSPC)
GSPC <- as.xts(
  get.hist.quote(
    "^GSPC", 
    start="1970-01-02",
    end='2009-09-15',
    quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))



library(quantmod)
getSymbols('^GSPC')



getSymbols('^GSPC',from='1970-01-01',to='2009-09-15')
colnames(GSPC) <- c("Open", "High", "Low", "Close","Volume","AdjClose")


setSymbolLookup(IBM=list(name='IBM',src='yahoo'),
                USDEUR=list(name='USD/EUR',src='oanda',
                            from=as.Date('2009-01-01')))
getSymbols(c('IBM','USDEUR'))
head(IBM)
head(USDEUR)