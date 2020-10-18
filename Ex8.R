#Loading packages 
library("readxl")
library("forecast")
library("tseries")

#Reading data from excel
df <- read_xlsx("C:\\Users\\marti\\Documents\\NTNU\\Tidsrekker\\Tidsrekker\\xls_ex8.xlsx",skip=1)
#df <- read_xlsx("xls_ex8.xlsx",skip=1) #fikk feilmelding på denne
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

# Prøver differencing som vi fikk kommentar på at vi burde gjøre
transformed.data = diff(boxcox_fit, lag = 7, differences = 1)
plot.ts(transformed.data,type="l")
abline(h=mean(transformed.data), col="red")

#ARIMA
fit1 <- auto.arima(transformed.data, ic="aicc", trace=TRUE, d=1)
fit1
checkresiduals(fit1$residuals, test="FALSE")
Box.test(fit$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

?auto.arima
fit2 = Arima(boxcox_fit, order = c(7,1,7))
fit2
checkresiduals(fit2$residuals, test="FALSE")
Box.test(fit2$residuals, type = "Ljung-Box", lag=10, fitdf = 2)
