library(quantmod)
library(forecast)
library(xlsx)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(xts)
library(readr)
library(moments)
library(rugarch)
library(prophet)
library(tsfknn)

# Extracting stock data for Amazon
getSymbols('AMZN', from= '2015-01-01')

# Separating Closing Prices of stocks from data
AMZN_CP = AMZN[,4]

# Plotting graph of Amazon Stock Prices to observe the trend
plot(AMZN_CP)

# Plotting the ACF and PACF plot of data
par(mfrow=c(1,2))
Acf(AMZN_CP, main = 'ACF Plot')
Pacf(AMZN_CP, main = 'PACF Plot')

# Plotting Additive and Multiplicative Decomposition
AMZN.ts <- ts(AMZN_CP, start=c(2010,1,1), frequency = 365.25)
AMZN.add  <- decompose(AMZN.ts,type = "additive")
plot(AMZN.add)
AMZN.mult <- decompose(AMZN.ts,type = "multiplicative")
plot(AMZN.mult)

# ADF test on Closing Prices 
print(adf.test(AMZN_CP))

# Splitting into test and train data 
N = length(AMZN_CP)
n = 0.7*N
train = AMZN_CP[1:n, ]
test  = AMZN_CP[(n+1):N,  ]
predlen=length(test)

# Taking log of dataset 
logs=diff(log(AMZN_CP), lag =1)
logs = logs[!is.na(logs)]

# Log returns plot
plot(logs, type='l', main= 'Log Returns Plot')

# ADF test on log of Closing Prices
print(adf.test(logs))

# ACF and PACF of log data 
Acf(logs, main = 'ACF of log data')
Pacf(logs, main = 'PACF of log data')

# Fitting the ARIMA model
# Auto ARIMA with seasonal = FALSE
fit1<-auto.arima(AMZN_CP, seasonal=FALSE)
tsdisplay(residuals(fit1), lag.max = 40, main='(1,1,1) Model Residuals')
fcast1<-forecast(fit1, h=30)
plot(fcast1)
accuracy(fcast1)

# Auto ARIMA with lambda = "auto"
fit2<-auto.arima(AMZN_CP, lambda = "auto")
tsdisplay(residuals(fit2), lag.max = 40, main='(2,1,2) Model Residuals')
fcast2<-forecast(fit2, h=30)
plot(fcast2)
accuracy(fcast2)

# ARIMA model with optimized p,d and q
fit3<-arima(AMZN_CP, order=c(8,2,8))
tsdisplay(residuals(fit3), lag.max = 40, main='(8,2,8) Model Residuals')
fcast3<-forecast(fit3, h=30)
plot(fcast3)
accuracy(fcast3)

# Histogram and Emperical Distribution
m=mean(logs);
s=sd(logs);
hist(logs, nclass=40, freq=FALSE, main='Closing Price Histogram');
curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(logs), main='Closing Price Empirical Distribution');
curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")

# Kurtosis
kurtosis(logs)

# Dataset forecast upper first 5 values
fitarfima = autoarfima(data = train, ar.max = 2, ma.max = 2, 
                       criterion = "AIC", method = "full")
fitarfima

# Defining the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(2,2)))

# Estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=train)
plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")

# Model akike
infocriteria(garch11closepricefit)

# Normal residuals
garchres <- data.frame(residuals(garch11closepricefit))  
plot(garchres$residuals.garch11closepricefit.)

# Standardized residuals
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE))
 
#Normal Q plot
qqnorm(garchres$residuals.garch11closepricefit..standardize...TRUE.)
qqline(garchres$residuals.garch11closepricefit..standardize...TRUE.)

#Squared standardized residuals Ljung Box
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)^2) 
Box.test(garchres$residuals.garch11closepricefit..standardize...TRUE..2, type="Ljung-Box")

# GARCH
garch11closepricefit_logs=ugarchfit(spec=garch11closeprice, data=logs)
ni.garch11 <- newsimpact(garch11closepricefit_logs)
plot(ni.garch11$zx, ni.garch11$zy, type="l", lwd=2, col="blue", main="GARCH(1,1) - NewsImpact", ylab=ni.garch11$yexpr, xlab=ni.garch11$xexpr
)

# EGARCH
egarch11.spec = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)),
                           mean.model = list(armaOrder=c(0,0)))
egarch11closepricefit=ugarchfit(spec=egarch11.spec, data=logs)
 ni.egarch11 <- newsimpact(egarch11closepricefit)
 plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue",main="EGARCH(1,1) - News Impact",
      ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)

# TGARCH  
tgarch11.spec = ugarchspec(variance.model = list(model="fGARCH",submodel="TGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))
amz.tgarch11.fit = ugarchfit(spec=tgarch11.spec, data=logs)
ni.tgarch11 <- newsimpact(amz.tgarch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type="l", lwd=2, col="blue",main="TGARCH(1,1) - News Impact",ylab=ni.tgarch11$yexpr, xlab=ni.tgarch11$xexpr)

# GARCH Forecasting
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=train)
garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 30) 
plot(garchforecast) 
accuracy(garchforecast, garchlosepricefit, test=NULL, d=NULL, D=NULL)

# Prophet
df <- data.frame(ds = index(AMZN),
                 y = as.numeric(AMZN[,'AMZN.Close']))
prophetpred <- prophet(df)
future <- make_future_dataframe(prophetpred, periods = 30)
forecastprophet <- predict(prophetpred, future)
plot(
  prophetpred,
  forecastprophet,
  uncertainty = TRUE,
  plot_cap = TRUE,
  xlabel = "ds",
  ylabel = "y"
)
dataprediction <- data.frame(forecastprophet$ds,forecastprophet$yhat)
trainlen <- length(AMZN_CP)
dataprediction <- dataprediction[c(1:trainlen),]
prophet_plot_components(prophetpred,forecastprophet)

# K Nearest Neighbours
df <- data.frame(ds = index(AMZN),
                 y = as.numeric(AMZN[,'AMZN.Close']))

predknn <- knn_forecasting(df$y, h = 30, lags = 1:30, k = 40, msas = "MIMO")
ro <- rolling_origin(predknn)
print(ro$global_accu)
plot(predknn, type="c")

# Neural Networks
# Hidden layers creation
alpha <- 1.5^(-10)
hn <- length(AMZN_CP)/(alpha*(length(AMZN_CP)+30))
lambda <- BoxCox.lambda(AMZN_CP)
dnn_pred <- nnetar(AMZN_CP, size= hn, lambda = lambda)
dnn_forecast <- forecast(dnn_pred, h= 30, PI = TRUE)
plot(dnn_forecast)
accuracy(dnn_forecast)

# ETS
library(readr)
library(tidyverse)
library(ggplot2)
library(scales)
library(forecast)
setwd("C:/Users/vaibh")
AMZN <- read_csv("AMZN_Stock_Data.csv")

glimpse(AMZN)
head(AMZN)
tail(AMZN)
 
# Let the package automatically select the model base on AIC and BIC
auto.amzn.aic = ets(AMZN$Close,model="ZZZ",ic="aic") 
auto.amzn.aic$method

auto.amzn.bic = ets(AMZN$Close,model="ZZZ",ic="bic") 
auto.amzn.bic$method

auto.amzn.aic.damped = ets(AMZN$Close,model="ZZZ",damped = TRUE, ic="aic") #
auto.amzn.aic.damped$method

auto.amzn.bic.damped = ets(AMZN$Close,model="ZZZ",damped = TRUE, ic="bic") #
auto.amzn.bic.damped$method

# The model selected by the package was MNN, MAN and MAdN
# Package autoselection
auto.amzn = ets(AMZN$Close, model = "ZZZ")
auto.amzn$method

# Applying model, MNN, MAN and MAdN
amzn.MNN = ets(AMZN$Close, model = "MNN")
summary(amzn.MNN)

amzn.MAN = ets(AMZN$Close, model = "MAN")
summary(amzn.MAN)

amzn.MAdN = ets(AMZN$Close, model = "MAN", damped = TRUE)
summary(amzn.MAdN)

# Although the MASE of the series is simillar. 
# But, the smallest MASE can be observed in the MAN series. 
# This suggested that MAN model is better model. 
# However, due to similarity of the MASE values, it was suspected that residuals will show similar result.

# Model diagnostic
checkresiduals(amzn.MNN)
checkresiduals(amzn.MAN)
checkresiduals(amzn.MAdN)

# Plot the forecast for 90 days
forecast.MNN = forecast(amzn.MNN, h = 30)
forecast.MAN = forecast(amzn.MAN, h = 30)
forecast.MAdN = forecast(amzn.MAdN, h = 30)

plot(forecast.MAN, ylab = "Closing stock", type = "l", fcol = "green", xlab = "Series")
plot(forecast.MNN, ylab = "Closing stock", type = "l", fcol = "red", xlab = "Series")
plot(forecast.MAdN, ylab = "Closing stock", type = "l", fcol = "blue", xlab = "Series")

accuracy(forecast.MNN)
accuracy(forecast.MAdN)
accuracy(forecast.MAN) 



