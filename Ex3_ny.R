#Loading packages 
library("readxl")
library("forecast")
library("tseries")

#Reading data from excel
df <- read_xlsx("xls.xlsx",skip=1)
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

#ACF
acf(tseries, main="") #values are correlated 

#Transforming the time series
##Box-Cox
lambda <- BoxCox.lambda(tseries[1:196])
boxcox_fit <- forecast::BoxCox(tseries,lambda=lambda) 
plot.ts(boxcox_fit,type="l")
abline(h=mean(boxcox_fit), col="red")

##Differencing 
forecast::ndiffs(boxcox_fit, test = "kpss")  #Number of differences needed  
transformed <- diff(boxcox_fit)
plot.ts(transformed,type="l")
abline(h=mean(transformed), col="red")

#ACF and PACF of BoxCox transformed data
acf(boxcox_fit,main="") 
pacf(boxcox_fit,main="") #AR(1) or AR(2)

acf(transformed,main="") 
pacf(transformed,main="") #AR(1) or AR(2)

#SEASONAL
seasonal <- diff(transformed, lag = 7, differences = 1)
plot.ts(seasonal,type="l")
abline(h=mean(seasonal), col="red")

acf(seasonal, main="") #q= 1, Q=1
pacf(seasonal, main="") #p=1, P=1

#Augmented Dickey-Fuller Test
tseries::adf.test(transformed, k=0) #-> stationary
kpss.test(transformed, null="Trend") #-> Stationary

#ARIMA-model and checking the residuals for each model
fit <- auto.arima(boxcox_fit, ic="aicc", trace=TRUE, d=1)
fit # ARIMA(2,1,3) AIC=733,14 
checkresiduals(fit$residuals, test="FALSE")
Box.test(fit$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

fit_bic <- auto.arima(boxcox_fit, ic="bic", trace=TRUE, d=1)
fit_bic # ARIMA(1,1,1) AICC= 736,4
checkresiduals(fit_bic$residuals, test="FALSE")
Box.test(fit_bic$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

fit_aicc <- Arima(boxcox_fit, order=c(2,1,2)) #aicc is inf in auto.arima
fit_aicc #smallest AICc and BIC #AICC 718,99
checkresiduals(fit_aicc$residuals, test="FALSE")
Box.test(fit_aicc$residuals, type = "Ljung-Box", lag=10, fitdf = 4)

fit_2 <- Arima(boxcox_fit, order=c(2,1,0))
fit_2 #AICC 743,29
checkresiduals(fit_2$residuals, test="FALSE")
Box.test(fit_2$residuals, type = "Ljung-Box", lag=10, fitdf = 2)

fit_3 <- Arima(boxcox_fit, order=c(0,1,1))
fit_3 #736,65
checkresiduals(fit_3$residuals, test="FALSE")
Box.test(fit_3$residuals, type = "Ljung-Box", lag=10, fitdf = 1)

fit_4 <- Arima(boxcox_fit, order=c(1,1,2))
fit_4 #738,52
checkresiduals(fit_4$residuals, test="FALSE")
Box.test(fit_4$residuals, type = "Ljung-Box", lag=10, fitdf = 3)



#Forecast next 30 days ARIMA(1,1,1)
forecast <- forecast::forecast(fit_bic, h=30, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$x <- tseries
autoplot(forecast, ylim=range(0,350))
forecast$mean


forecast$fitted <-InvBoxCox(forecast$fitted,lambda=lambda)
plot(df$Dato, df$`Nye tilfeller`, type="l", ylab="Number of new cases", 
     xlab="Time")
lines(df$Dato,forecast$fitted, col="red")


forecast$residuals <- InvBoxCox(forecast$residuals, lambda=lambda)
plot(df$`Dato`,forecast$residuals, type="l")



# Plot forecast med nye tall
dfny <- read_xlsx("inkl nye tall.xlsx",skip=1)
tseriesny <- ts(dfny$`Nye tilfeller`,
              start = c(2020, dayofYear),
              frequency = 365)
plot(forecast, ylim=range(0,450))
lines(tseriesny)


##NY MODEL

lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2){
      for (Q in 0:2){
        model = Arima(boxcox_fit, order=c(p,1,q), 
                      seasonal = list(order=c(P,1,Q),period=7),method="ML")
        AICC = model$aicc
        if (AICC<lowest_aicc){
          best_model_3 <- model
          lowest_aicc <- AICC
        }
      }
    }
  }
}

best_model_3
model2 = Arima(boxcox_fit, order=c(1,1,1), 
              seasonal = list(order=c(0,1,1),period=7),method="ML")

saveRDS(best_model_3, "model_3.rds")
saveRDS(lambda, "lambda_3.rds")

checkresiduals(best_model_3$residuals, test="FALSE")

#Forecast next 30 days best_model
forecast <- forecast::forecast(best_model_3, h=14, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$x <- tseries
autoplot(forecast, ylim=range(0,350)) 
