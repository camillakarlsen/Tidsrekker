#Loading packages 
library("readxl")
library("forecast")
library("tseries")
library("astsa")
library("sarima")
library("rugarch")

#Set working direction
#setwd("/Users/camillakarlsen/Desktop/Tidsrekker/Tidsrekker")
setwd("C:\\Users\\marti\\Documents\\NTNU\\Tidsrekker\\Tidsrekker")

#Reading data from excel
df <- read_xlsx("xls_ex8.xlsx",skip=1)
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

acf(tseries, main="")
pacf(tseries, main="")

# BoxCox transformation
lambda <- BoxCox.lambda(tseries[1:length(tseries)])
boxcox_fit <- BoxCox(tseries,lambda=lambda) 
plot.ts(boxcox_fit,type="l")
abline(h=mean(boxcox_fit), col="red")

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
transformed.seasonal = diff(transformed, lag = 7, differences = 1)
plot.ts(transformed.seasonal,type="l")
abline(h=mean(transformed.seasonal), col="red")

acf(transformed.seasonal, main="") #q = 1, Q = 1 eller 2 
pacf(transformed.seasonal, main="") #p = 1, P = 0 tails off

#Fit model according to acf and pacf

model_from_acf_1 = Arima(boxcox_fit, order=c(1,1,1), 
                         seasonal = list(order=c(0,1,1),period=7),method="ML")
model_from_acf_1
model_from_acf_2 = Arima(boxcox_fit, order=c(1,1,1), 
                         seasonal = list(order=c(0,1,2),period=7),method="ML")
model_from_acf_2

#Fit ARIMA model based on lowest AICC
lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2){
      for (Q in 0:2){
        model = Arima(boxcox_fit, order=c(p,1,q), 
                      seasonal = list(order=c(P,1,Q),period=7),method="ML")
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
Box.test(best_model$residuals, type = "Ljung-Box", lag= 20, fitdf = 3) 
Box.test(best_model$residuals, type = "Box-Pierce", lag=20, fitdf = 3)

#Forecast next 14 days (blir veldig lav på grunn av siste registrering?)
forecast <- forecast::forecast(best_model, h=14, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$x <- tseries
autoplot(forecast)

####Remove outlier - new model and forecast
# siden det kan ta litt tid � f� pr�vesvar tror jeg de to siste verdiene
# kan vore ganske usikre
# har sammenliknet verdier for de siste dagene og antall tilfeller den
# 12.10 er endret fra 108 til 165 og
# 13.10 er endret fra 11 til 155
# tenker derfor vi kanskje kan fjerne begge disse eller evt oppdatere tallene?

df_new <- read_xlsx("xls_ex8.xlsx",skip=1)[-c(236),]
df_new$Dato <- as.Date(df_new$Dato, "%d.%m.%Y") 
df_new$`Nye tilfeller`
tseries_new <- ts(df_new$`Nye tilfeller`,
              start = c(2020, dayofYear),
              frequency = 365)
lambda_new <- BoxCox.lambda(tseries_new[1:length(tseries_new)])
boxcox_fit_new <- BoxCox(tseries_new,lambda=lambda_new) 
transformed_new <- diff(boxcox_fit_new)
transformed.seasonal_new = diff(transformed_new, lag = 7, differences = 1)

lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2){
      for (Q in 0:2){
        model = Arima(boxcox_fit_new, order=c(p,1,q), 
                      seasonal = list(order=c(P,1,Q),period=7),method="ML")
        AICC = model$aicc
        if (AICC<lowest_aicc){
          best_model_new <- model
          lowest_aicc <- AICC
        }
      }
    }
  }
}

best_model_new

checkresiduals(best_model_new$residuals, test="FALSE")

forecast_new <- forecast::forecast(best_model_new, h=14, biasadj=TRUE)
forecast_new$mean <- InvBoxCox(forecast_new$mean,lambda=lambda_new)
forecast_new$upper <- InvBoxCox(forecast_new$upper, lambda = lambda_new)
forecast_new$lower <- InvBoxCox(forecast_new$lower, lambda = lambda_new)
forecast_new$x <- tseries_new
autoplot(forecast_new, ylim=c(0,350))

# Update last two values
df_updated <- read_xlsx("xls_ex8_updated.xlsx",skip=1)
df_updated$Dato <- as.Date(df_updated$Dato, "%d.%m.%Y") 
df_updated$`Nye tilfeller`
tseries_updated <- ts(df_updated$`Nye tilfeller`,
                  start = c(2020, dayofYear),
                  frequency = 365)
tseries_updated
lambda_updated <- BoxCox.lambda(tseries_updated[1:length(tseries_updated)])
lambda_updated
boxcox_fit_updated <- BoxCox(tseries_updated,lambda=lambda_updated) 
boxcox_fit_updated
plot.ts(boxcox_fit_updated)
transformed_updated <- diff(boxcox_fit_updated)
transformed.seasonal_updated = diff(transformed_updated, lag = 7, differences = 1)

lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2){
      for (Q in 0:2){
        model = Arima(boxcox_fit_updated, order=c(p,1,q), 
                      seasonal = list(order=c(P,1,Q),period=7),method="ML")
        AICC = model$aicc
        if (AICC<lowest_aicc){
          best_model_updated <- model
          lowest_aicc <- AICC
        }
      }
    }
  }
}

best_model_updated

checkresiduals(best_model_updated$residuals, test="FALSE")

forecast_updated <- forecast::forecast(best_model_updated, h=14, biasadj=TRUE)
forecast_updated$mean <- InvBoxCox(forecast_updated$mean,lambda=lambda_updated)
forecast_updated$upper <- InvBoxCox(forecast_updated$upper, lambda = lambda_updated)
forecast_updated$lower <- InvBoxCox(forecast_updated$lower, lambda = lambda_updated)
forecast_updated$x <- tseries_updated
autoplot(forecast_updated, ylim=c(0,350))


#Simulating
?arima.sim

sim = simulate(best_model, nsim = 70)
plot(sim)

sim1 = sim_sarima(best_model_new, n=70)
?sim_sarima

??sarima.Sim

??sim.ssarima

#GARCH
plot(best_model$residuals)
pacf(best_model$residuals^2) # try garch(1,1)?
plot(best_model_new$residuals)
pacf(best_model_new$residuals^2) # try garch(2,2)?
?ugarchspec
garchmod = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(2,2)), 
                      mean.model = list(armaOrder=c(0,0),include.mean=TRUE))

garch = ugarchfit(spec = garchmod, data=best_model_new$residuals,solver.control = list(trace=0))
garch # => garch(1,0)?

garchmod1 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,0)), 
                      mean.model = list(armaOrder=c(0,0),include.mean=TRUE))

garch1 = ugarchfit(spec = garchmod1, data=best_model_new$residuals,solver.control = list(trace=0))
garch1
infocriteria(garch1)[1]

aic = 10000
for (p in 0:2) {
  for (q in 0:2) {
    garchmod = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(p,q)), 
                          mean.model = list(armaOrder=c(0,0),include.mean=TRUE))
    fit = ugarchfit(spec = garchmod1, data=best_model_new$residuals,solver.control = list(trace=0))
    ic = infocriteria(fit)[1]
    if (ic<aic){
      best.garch = fit
      aic = ic
    }
  }
}
best.garch #GARCH(1,0)

