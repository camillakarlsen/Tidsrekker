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
inds <- seq(df$Dato[1], df$Dato[196], by = "day")
tseries <- ts(df$`Nye tilfeller`,
        start = c(2020, as.numeric(format(inds[1], "%j"))),
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

#ACF and PACF of transformed data
acf(diff_t,main="") #MA(0), MA(1) or MA(2)
pacf(diff_t,main="") #AR(1) or AR(2)

#Augmented Dickey-Fuller Test
tseries::adf.test(diff_t, k=0) #-> stationary

#ARIMA-model - ikke riktig 
arima <- arima(tseries_BC, order=c(1,1,2))
plot.ts(arima$residuals)
forecast <- forecast::forecast(arima, h=30)
inv.forecast <- InvBoxCox(forecast$mean,lambda=0.5)
inv.upper <- InvBoxCox(forecast$upper, lambda = 0.5)[,2]
inv.lower <- InvBoxCox(forecast$lower, lambda = 0.5)[,2]
d=seq(1,30)
plot(d,inv.forecast, type="o", ylim=range(50,20), xlab="Days after 21.02.2020")
lines(d,inv.upper,col="red")
lines(d,inv.lower, col="red")


