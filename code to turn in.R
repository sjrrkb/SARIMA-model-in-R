library(TSA)
library(readxl)

iron=ts(read_excel(choose.files()), start=1956, frequency=12)
iron
ironK<-iron[,-1]
ironK
plot(ironK)


#######################################################
series<-log(ironK)
plot(series)
acf(as.vector(series))

win.graph(width=12, height=6, pointsize = 8)
par(mfrow=c(2,1))

series.1<-(ironK)
plot(series.1,xlab='Years\nFigure 1a',ylab='Iron Production(thousand tones)',main='Time Series of Iron Production')
acf(as.vector(series.1),main='Sample ACF of Iron Production', xlab='Lag, \nFigure 1b')

series.2<-log(ironK)
plot(series.2,xlab='Years\nFigure 2a',ylab='log(Iron Production)',main='Time Series of log(Iron Production)')
acf(as.vector(series.2),main='Sample ACF of log(Iron Production)', xlab='Lag, \nFigure 2b')

series.3<-diff(log(ironK))
plot(series.3,xlab='Years\nFigure 3a',ylab='diff(log(Iron Production))',main='Time Series of diff(log(Iron Production))')
abline(h=0)
acf(as.vector(series.3),main='Sample ACF of diff(log(Iron Production))', xlab='Lag, \nFigure 3b')
pacf(as.vector(series.3))
win.graph(width=12, height=4, pointsize = 8)
par(mfrow=c(1,2))

series.6<-diff(diff(log(ironK)),lag=6)
acf(as.vector(series.6),ci.type='ma',ylab='lag 6 ACF',main='Sample Lag 6 ACF',xlab='Lag\nFigure 4a')

series.12<-diff(diff(log(ironK)),lag=12)
acf(as.vector(series.12),ci.type='ma',ylab='lag 12 ACF',main='Sample Lag 12 ACF',xlab='Lag\nFigure 4b')
plot(series.12)

################
##Lag 12 model##
################
series2<-arima(series,order=c(1,1,1),seasonal=list(order=c(0,0,2),period=12))

series2
win.graph(width=12, height=6, pointsize = 8)

par(mfrow=c(2,2))
acf(as.vector(rstandard(series2)),ci.type='ma', main=expression("Residual ACF of ARIMA(1,1,1)x(0,0,2)"[12]),xlab='Lag\nFigure 7a')
pacf(as.vector(rstandard(series2)),main=expression("Residual PACF of ARIMA(1,1,1)x(0,0,2)"[12]),xlab='Lag\nFigure 7b')
hist(as.vector(rstandard(series2)),main=expression("Residual Histogram of ARIMA(1,1,1)x(0,0,2)"[12]),xlab='Lag\nFigure 7c')
qqnorm(as.vector(rstandard(series2)),main=expression("Residual Q-Q Plot of ARIMA(1,1,1)x(0,0,2)"[12]),xlab='Lag\nFigure 7d')
qqline(as.vector(rstandard(series2)))
shapiro.test(as.vector(rstandard(series2)))
LB.test(series2)

par(mfrow=c(1,1))
plot(series2,n1=c(1956,1),n.ahead=24,pch=15,main=expression("24 Month Forecast with Limits for ARIMA(1,1,1)x(0,0,2)"[12]),ylab='Iron Production Data(thousand tones)',transform = exp,xlab='Year\nFigure 10')

###############
##Lag 6 model##
###############
series6<-arima(series,order=c(1,1,1),seasonal=list(order=c(1,0,1),period=6))
series6
par(mfrow=c(2,2))
acf(as.vector(rstandard(series6)),ci.type='ma', main=expression("Residual ACF of ARIMA(1,1,1)x(0,0,2)"[6]),xlab='Lag\nFigure 5a')
pacf(as.vector(rstandard(series6)),main=expression("Residual PACF of ARIMA(1,1,1)x(0,0,2)"[6]),xlab='Lag\nFigure 5b')
hist(as.vector(rstandard(series6)),main=expression("Residual Histogram of ARIMA(1,1,1)x(0,0,2)"[6]),xlab='Lag\nFigure 5c')
qqnorm(as.vector(rstandard(series6)),main=expression("Residual Q-Q Plot of ARIMA(1,1,1)x(0,0,2)"[6]),xlab='Lag\nFigure 5d')
qqline(as.vector(rstandard(series6)))
shapiro.test(as.vector(rstandard(series6)))
LB.test(series6)

par(mfrow=c(1,1))
plot(series6,n1=c(1956,1),n.ahead=24,pch=15,main=expression("24 Month Forecast with Limits for ARIMA(1,1,1)x(0,0,2)"[6]),ylab='Iron Production Data(thousand tones)',transform = exp,xlab='Year\nFigure 9')
