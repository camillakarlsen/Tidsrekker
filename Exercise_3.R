library(readxl)
library(astsa)
library(itsmr)
library("forecast")
library(tseries)
library("TTR")

df <- read_xlsx("xls.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="o", xlab="Date", ylab= "Cumulative cases")
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
?pacf

mu_x = mean(x) # sample mean
mu_x
autocov_x = acf(x, length(x)-1, type="covariance")
gamma0_x = autocov_x$acf[1] # acvf at lag 0
gamma1_x = autocov_x$acf[2]

#Differencing the time series
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
pacf(diff_2, lag = length(diff_2))

#Decomposing Non-Seasonal Data - smoothing method - trend component
xSMA_3 <- SMA(x,n=3)
plot.ts(xSMA_3)

xSMA_5 <- SMA(x,n=5)
plot.ts(xSMA_5)

xSMA_7 <- SMA(x,n=7)
plot.ts(xSMA_7)

#Auto arima
arima.model <- auto.arima(x)
arima.model
plot.ts(arima.model$residuals)
acf(ts(arima.model$residuals), main="ACF Residuals")
pacf(ts(arima.model$residuals), main="PACF Residuals")

#Forecast using an Arima model 
forecast <- forecast(arima.model, level=c(95), h=10)
forecast
plot(forecast)

acf(forecast$residuals)
Box.test(forecast$residuals, lag=20, type="Ljung-Box")

plot.ts(forecast$residuals)

