#Loading packages 
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

plot.ts(tseries, ylab="New cases")
summary(tseries)

#ACF
acf(tseries, main="ACF") #values are correlated 

#Transforming the time series
##Differencing
diff_t <- diff(tseries)
plot.ts(diff_t)
##Box-Cox
lambda <- BoxCox.lambda(diff_t) #hvorfor gir denne lambda=1? Med en mindre lambda blir jo variansen borte? 
tseries_fit <- BoxCox(diff_t,lambda=0.2)
plot.ts(tseries_fit)
abline(h=mean(tseries_fit), col="red")

acf(tseries_fit) 
pacf(tseries_fit)

#Different tests 
Box.test(tseries_fit, lag=1, type="Ljung-Box")
tseries::adf.test(tseries_fit) #-> stationary
Box.test(tseries, lag=1, type="Ljung-Box")
tseries::adf.test(tseries) #-> non stationary




