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

#Fit model according to AICc
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
autoplot(forecast, ylim=c(0,600))

summary(forecast)

#sarima.for(tseries, 14, p=1, d=1, q=0, P=0, D=1, Q=2, S=7, plot.all = TRUE)

#Simulating 

##Definerer Sarimamodel
SarimaModel = list(ar=best_model$coef[1], sma = best_model$coef[2:3], 
                   iorder=1,siorder=1,nseasons=7, sigma2=1.2)

##Loading the best model from exercise 3
best_model_3 <- readRDS("model_3.rds")

SarimaModel_3 = list(ar=best_model_3$coef[1], sma = best_model_3$coef[2:3], 
                   iorder=1,siorder=1,nseasons=7, sigma2=1.5)

#Simulate five corresponding two week realizations 
#Mulig det er noe feil i denne funksjonen 
#Burde vel egentlig ha transformert dataene tilbake også om vi bruker 
#boxcox_fit som start verdier? Har du noen tanker om hva som kan være feil? 
sim_function <- prepareSimSarima(n=14, model = SarimaModel, x=list(before=boxcox_fit), 
                                 n.start=length(tseries))

sim_function_3 <- prepareSimSarima(n=14, model = SarimaModel_3, x=list(before=boxcox_fit), 
                                   n.start=length(tseries))

#Defining the first day in the simulation
firstday <- df$Dato[length(tseries)] + 1 - as.Date("2020-01-01") + 1

#Plot forecast and simulation 

plot(forecast, ylim=c(0,400), xlim=c(2020.7, 2020.822))
for (i in 1:5){ 
  lines(ts(sim_function(), start = c(2020, firstday), frequency = 365), 
        type="l", col="red")
  lines(ts(sim_function_3(), start = c(2020, firstday), frequency = 365), 
        type="l", col="green")
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

