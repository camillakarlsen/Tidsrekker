#Loading packages 
library("readxl")
library("forecast")
library("tseries")

#Reading data from excel
df <- read_xlsx("xls.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="l", xlab="Date", ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")

## Create a time series object
dayofYear <- (df$Dato[1] - as.Date("2020-01-01") + 1)
tseries <- ts(df$`Nye tilfeller`,
              start = c(2020, dayofYear),
              frequency = 365)

plot.ts(tseries, ylab="New cases", type="o")
summary(tseries)

#ACF
acf(tseries, main="") #values are correlated 

#Transforming the time series

##Box-Cox
lambda <- BoxCox.lambda(tseries) #hvorfor gir denne lambda=1? Med en mindre lambda blir jo variansen borte? 
tseries_BC <- BoxCox(tseries,lambda=0.5) 
plot.ts(tseries_BC,type="o")

##Differencing
diff_t <- diff(tseries_BC)
plot.ts(diff_t,type="o")
abline(h=mean(diff_t), col="red")

forecast::ndiffs(tseries_BC, test = "kpss") #Finds the number of differences needed using the KPSS test 

#ACF and PACF of transformed data
acf(diff_t,main="") #MA(1) or MA(2) 
pacf(diff_t,main="") #AR(1) or AR(2) #Lag 0 er ikke med her, første er lag 1

#Augmented Dickey-Fuller Test
tseries::adf.test(diff_t, k=0) #-> stationary

#ARIMA-model
arima <- Arima(tseries_BC, order=c(2,1,2))
arima
plot.ts(arima$residuals)
fit <- auto.arima(tseries_BC, ic="aicc",start.p = 0, max.p = 2, start.q = 0, max.q = 2, trace=TRUE)
fit

#Check the residuals 
res <- fit$residuals
plot.ts(res)
Box.test(res, type = "Ljung-Box", lag=10, fitdf = 4) #hva skal lag være?
checkresiduals(res) #ACF er rar - seasonal? 

#Forecast next 30 days - usikker på hvordan vi skal gjøre dette enda
forecast <- forecast::forecast(arima, h=30)
inv.forecast <- InvBoxCox(forecast$mean,lambda=0.5)
inv.upper <- InvBoxCox(forecast$upper, lambda = 0.5)[,2]
inv.lower <- InvBoxCox(forecast$lower, lambda = 0.5)[,2]
d=seq(1,30)
plot(d,inv.forecast, type="o", ylim=range(50,20), xlab="Days after 21.02.2020")
lines(d,inv.upper,col="red")
lines(d,inv.lower, col="red")


