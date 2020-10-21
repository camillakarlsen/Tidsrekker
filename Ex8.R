#Loading packages 
library("readxl")
library("forecast")
library("tseries")
library("astsa")

#Reading data from excel
#setwd("/Users/camillakarlsen/Desktop/Tidsrekker/Tidsrekker")
df <- read_xlsx("xls_ex8.xlsx",skip=1)# [-c(236),] #Fjerne siste rad siden denne ikke er riktig?
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

acf(transformed, main="")
pacf(transformed, main="")

#Augmented Dickey-Fuller Test
tseries::adf.test(transformed, k=0)
kpss.test(transformed, null="Trend")

# Seasonal differencing
transformed.data = diff(transformed, lag = 7, differences = 1)
plot.ts(transformed.data,type="l")
abline(h=mean(transformed.data), col="red")

acf(transformed.data, main="") #q = 1, Q = 1 eller 2 
pacf(transformed.data, main="") #p = 1, P = 0 tails off

model_from_acf = Arima(boxcox_fit, order=c(1,1,1), seasonal = list(order=c(0,1,2),period=7),method="ML")
model_from_acf
#Fit ARIMA model - AICC
lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2){
      for (Q in 0:2){
        model = Arima(boxcox_fit, order=c(p,1,q), seasonal = list(order=c(P,1,Q),period=7),method="ML")
        AICC = model$aicc
        if (AICC<lowest_aicc){
          best_model <- model
          lowest_aicc <- AICC
        }
      }
    }
  }
}

best_model #ARIMA(1,1,0)(0,1,2)[7]


#Check residuals
checkresiduals(best_model$residuals, test="FALSE")
Box.test(best_model$residuals, type = "Ljung-Box", lag=20, fitdf = 3) #hva skal lag være? 


#Forecast next 14 days (blir veldig lav? på grunn av siste registrering?)
forecast <- forecast::forecast(best_model, h=14, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$x <- tseries
autoplot(forecast)

#Simulating
?arima.sim

#GARCH


