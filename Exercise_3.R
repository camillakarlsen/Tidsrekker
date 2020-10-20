library(readxl)
library(astsa)
library(itsmr)
library("forecast")
library(tseries)
library("TTR")
library("aTSA")

df <- read_xlsx("xls.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="l", xlab="Date", ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")

#PACF page 86
#x = df$`Nye tilfeller`
#pacf(x, lag = length(x)-1, pl= TRUE)
# skal vi bruke kumulativt antall eller nye tilfeller? Nye tilfeller trur æ? 
y = df$`Kumulativt antall`
pacf(y, lag = length(y)-1, pl = TRUE) #litt rart at kun pacf ved lag 0 er utenfor de bl� strekene? 
mu = mean(y) # sample mean
mu
autocov = acvf(y, length(y)-1)
gamma0 = autocov[1] # acvf at lag 0
gamma1 = autocov[2]

## Create a time series object
inds <- seq(df$Dato[1], df$Dato[196], by = "day")
x <- ts(df$`Nye tilfeller`,
        start = c(2020, as.numeric(format(inds[1], "%j"))),
        frequency = 365)

plot.ts(x, ylab="New cases")
summary(x)

#ACF
acf(x, main="ACF") #values are correlated 
#PACF page 86
p <- pacf(x)

mu_x = mean(x) # sample mean
mu_x
autocov_x = acf(x, length(x)-1, type="covariance")
gamma0_x = autocov_x$acf[1] # acvf at lag 0
gamma1_x = autocov_x$acf[2]

#Differencing the time series to remove non-stationarity
diff_1 <- diff(x)
plot(diff_1)
abline(h=mean(diff_1), col="red")

diff_2 <- diff(x, differences = 2)
plot(diff_2)
abline(h=mean(diff_2), col="red")
#stationary in mean but not in variance

#ACF
acf(diff_2, main="ACF") #values are correlated 
#PACF page 86
pacf(diff_2)
#acf droppes after lag 0 -> p=0, since the curve get a cut of here. 
#The value of q is ??

fit <- arima(diff_2, c(0, 2, 1))
pred <- predict(fit, n.ahead = 30)
pred
ts.plot(x,pred$pred, lty = c(1,3))
fcast <- forecast::forecast(fit, level=c(95), h=30)
plot(fcast)

#Decomposing Non-Seasonal Data - smoothing method - trend component
xSMA_3 <- SMA(x,n=3)
plot(df$Dato,xSMA_3, type = "l",xlab="Date")

xSMA_5 <- SMA(x,n=5)
plot(df$Dato, xSMA_5, type="l")

xSMA_7 <- SMA(x,n=7)
plot(df$Dato,xSMA_7,type="l")

xSMA_15 <- SMA(x,n=15)
plot(df$Dato,xSMA_15,type="l", xlab="Date")

ts_15 <- ts(xSMA_15[-(1:15)], start = c(2020, as.numeric(format(inds[15], "%j"))),
             frequency = 365)
acf(ts_15, lag=length(ts_15))
pacf(ts_15, lag=length(ts_15))

adf.test(diff_2)

#Auto arima
arima.model <- auto.arima(x)
arima.model
plot.ts(arima.model$residuals)
acf(ts(arima.model$residuals), main="ACF Residuals")
pacf(ts(arima.model$residuals), main="PACF Residuals")

#Forecast using an Arima model 
forecast <- forecast::forecast(arima.model, level=c(95), h=10)
forecast
plot(forecast)

acf(forecast$residuals)
Box.test(forecast$residuals, lag=20, type="Ljung-Box")

plot.ts(forecast$residuals)
adf.test(diff_2)
l <- log(x)
dl <- diff(l)
ddl <- diff(l, differences = 2)
plot.ts(cbind(x, diff_1, diff_2, l, dl,ddl))

