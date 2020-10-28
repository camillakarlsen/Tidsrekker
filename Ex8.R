#Loading packages 
library("readxl")
library("forecast")
library("tseries")
library("astsa")
library("sarima")
library("rugarch")
library("dplyr")

#Set working direction
setwd("/Users/camillakarlsen/Desktop/Tidsrekker/Tidsrekker")
#setwd("C:\\Users\\marti\\Documents\\NTNU\\Tidsrekker\\Tidsrekker")

#Reading data from excel
df <- read_xlsx("xls_ex8_updated.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="l", xlab="Date", 
     ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")

## Create a time series object
dayofYear <- (df$Dato[1] - as.Date("2020-01-01") + 1)
tseries <- ts(df$`Nye tilfeller`,
              start = c(2020, dayofYear), frequency = 365)

plot.ts(tseries, ylab="New cases", type="o")
summary(tseries)

acf(tseries, main="")
pacf(tseries, main="")

lambda <- BoxCox.lambda(tseries[1:length(tseries)])
lambda
boxcox_fit <- forecast::BoxCox(tseries,lambda=lambda) 
plot.ts(boxcox_fit)
abline(h=mean(boxcox_fit), col="red")

transformed <- diff(boxcox_fit)
plot.ts(transformed)
abline(h=mean(transformed), col="red")

acf(transformed, main="")
pacf(transformed, main="")

transformed.seasonal = diff(transformed, lag = 7, differences = 1)
plot.ts(transformed.seasonal,type="l")
abline(h=mean(transformed.seasonal), col="red")

acf(transformed.seasonal, main="") #q = 1, Q = 1 
pacf(transformed.seasonal, main="") #p = 1, P = 0 tails off

#Fit model according to acf and pacf

model_from_acf = Arima(boxcox_fit, order=c(1,1,1), 
                         seasonal = list(order=c(0,1,1),period=7),method="ML")
model_from_acf

lowest_aicc <- 10000
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:1){
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

best_model

checkresiduals(best_model$residuals, test="FALSE")

forecast <- forecast::forecast(best_model, h=14, biasadj=TRUE)
forecast$mean <- InvBoxCox(forecast$mean,lambda=lambda)
forecast$upper <- InvBoxCox(forecast$upper, lambda = lambda)
forecast$lower <- InvBoxCox(forecast$lower, lambda = lambda)
forecast$x <- tseries
autoplot(forecast, ylim=c(0,350))


#Simulating 
#Define Sarimamodel
SarimaModel = list(ar=-0.43, sma = c(-0.84, -0.16), 
                   iorder=1,siorder=1,nseasons=7, sigma2=1.2)

#Kun test da jeg ikke har fått koden til å funke helt 
#- simulerer negative verdier og mye mindre verier enn ønsket
# er det noen parametre som burde endres? 
sim_test = sim_sarima(n=14, model = SarimaModel, x=tseries,
                 n.start=length(tseries), eps = best_model$residuals)

#Simulate five corresponding two week realizations 
sim_function <- prepareSimSarima(n=14, model = SarimaModel, x=tseries, 
                  n.start=length(tseries), eps = best_model$residuals)

firstday <- df$Dato[length(tseries)] + 1 - as.Date("2020-01-01") + 1

plot(forecast, ylim=c(0,400))
for (i in 1:5){ #obs har abs rundt simuleringen nå for å få positive verdier
  lines(ts(abs(sim_function()), start = c(2020, firstday), frequency = 365), 
        type="l", col="red")
}


#GARCH
plot(best_model$residuals)
pacf(best_model$residuals^2) # try garch(1,1)?

garchmod = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(2,2)), 
                      mean.model = list(armaOrder=c(0,0),include.mean=TRUE))

garch = ugarchfit(spec = garchmod, data=best_model$residuals,
                  solver.control = list(trace=0))
garch # => garch(1,0)?

garchmod1 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,0)), 
                      mean.model = list(armaOrder=c(0,0),include.mean=TRUE))

garch1 = ugarchfit(spec = garchmod1, data=best_model$residuals,
                   solver.control = list(trace=0))
garch1
infocriteria(garch1)[1]

aic = 10000
for (p in 0:1) {
  for (q in 0:1) {
    garchmod = ugarchspec(variance.model = 
                            list(model="sGARCH", garchOrder=c(p,q)), 
                          mean.model = list(armaOrder=c(0,0),include.mean=TRUE))
    fit = ugarchfit(spec = garchmod, data=best_model$residuals,
                    solver.control = list(trace=0))
    ic = infocriteria(fit)[1]
    if (ic<aic){
      best.garch = fit
      aic = ic
    }
  }
}
best.garch #GARCH(1,0)

