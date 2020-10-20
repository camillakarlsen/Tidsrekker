#Loading packages 
library("readxl")
library("forecast")
library("tseries")

#Reading data from excel
#setwd("/Users/camillakarlsen/Desktop/Tidsrekker/Tidsrekker")
df <- read_xlsx("xls_ex8.xlsx",skip=1)[-c(236),] #Fjerner siste rad siden denne ikke er fullstendig
#df <- read_xlsx("C:\\Users\\marti\\Documents\\NTNU\\Tidsrekker\\Tidsrekker\\xls_ex8.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="l", xlab="Date", 
     ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")

## Create a time series object
dayofYear <- (df$Dato[1] - as.Date("2020-01-01") + 1)
tseries <- ts(df$`Nye tilfeller`,
              start = c(2020, dayofYear),
              frequency = 365)

plot.ts(tseries, ylab="New cases", type="o")
summary(tseries)

# ACF, PACF
acf(tseries, main="")
pacf(tseries, main="")

# BoxCox transformation
lambda <- BoxCox.lambda(tseries[1:length(tseries)])
lambda
boxcox_fit <- BoxCox(tseries,lambda=lambda) 
plot.ts(boxcox_fit,type="l")
abline(h=mean(boxcox_fit), col="red")

# ACF, PACF
acf(boxcox_fit, main="")
pacf(boxcox_fit, main="")

##Differencing 
forecast::ndiffs(boxcox_fit, test = "kpss")  #Number of differences needed  
transformed <- diff(boxcox_fit)
plot.ts(transformed,type="l")
abline(h=mean(transformed), col="red")

# ACF, PACF
acf(transformed, main="")
pacf(transformed, main="")

#Augmented Dickey-Fuller Test
tseries::adf.test(transformed, k=0)
kpss.test(transformed, null="Trend")

#ARIMA
fit <- auto.arima(boxcox_fit, ic="aicc", trace=TRUE, d=1)
fit
checkresiduals(fit$residuals, test="FALSE")
Box.test(fit$residuals, type = "Ljung-Box", lag=10, fitdf = 2)
accuracy(fit)



# Pr?ver differencing som vi fikk kommentar p? at vi burde gj?re
transformed.data = diff(boxcox_fit, lag = 7, differences = 1)
plot.ts(transformed.data,type="l")
abline(h=mean(transformed.data), col="red")

#ARIMA
fit1 <- auto.arima(transformed.data, ic="aicc", trace=TRUE, d=1)
fit1
checkresiduals(fit1$residuals, test="FALSE")
Box.test(fit$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

fit2 = Arima(boxcox_fit, order = c(7,1,7))
fit2
checkresiduals(fit2$residuals, test="FALSE")
Box.test(fit2$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

fit3 = astsa::sarima(boxcox_fit, 2, 1, 2, 2, 1, 2, 7)
fit3

#Forecast next 14 days 
forecast <- forecast::forecast(fit, h=14, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$upper
forecast$x <- tseries
autoplot(forecast)
forecast$mean

forecast$fitted <-InvBoxCox(forecast$fitted,lambda=lambda)
plot(df$Dato, df$`Nye tilfeller`, type="l", ylab="Number of new cases", 
     xlab="Time")
lines(df$Dato,forecast$fitted, col="red")

forecast$residuals <- InvBoxCox(forecast$residuals, lambda=lambda)
plot(df$`Dato`,forecast$residuals, type="l")
