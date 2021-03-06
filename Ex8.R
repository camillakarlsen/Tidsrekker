#Loading packages 
library("readxl")
library("forecast")
library("tseries")
library("astsa")
library("sarima")
library("rugarch")
library("dplyr")

#Set working directory
#setwd("/Users/camillakarlsen/Desktop/Tidsrekker/Tidsrekker")
setwd("C:\\Users\\marti\\Documents\\NTNU\\Tidsrekker\\Tidsrekker")

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


#DENNE SER UT TIL Å VÆRE RIKTIG  - ny model 
plot(forecast, ylim=c(0,700), xlim=c(2020.76, 2020.822))
for (i in 1:5){ 
  lines(InvBoxCox(simulate(best_model, 14), lambda = lambda), 
        type="l", col="red")
}

#Med gammel model - ikke riktig 
# Skal vi forecaste 03.09 eller fra 13.10? 
# Skal vi bruke estimert model fra øving 3 eller skal vi tilpasse en til de nye dataene men med p,q,P,Q som fra øving 3? 

Model3 <- Arima(boxcox_fit, order=c(1,1,1), 
                seasonal = list(order=c(0,1,1),period=7),method="ML")

Model3 <- Arima(boxcox_fit, order=c(1,1,1))

forecast2 <- forecast::forecast(Model3, h=14, biasadj=TRUE)
forecast2$mean <- InvBoxCox(forecast2$mean,lambda=lambda)
forecast2$upper <- InvBoxCox(forecast2$upper, lambda = lambda)
forecast2$lower <- InvBoxCox(forecast2$lower, lambda = lambda)
forecast2$x <- tseries

plot(forecast2, ylim=c(0,600), xlim=c(2020.76, 2020.822))
for (i in 1:5){ 
  lines(InvBoxCox(simulate(Model3, 14), lambda = lambda), 
        type="l", col="red")
}

##Loading the best model and lambda from exercise 3
best_model_3 <- readRDS("model_3.rds")
lambda_3 <- readRDS("lambda_3.rds")

forecast_3 <- forecast::forecast(best_model_3, h=54, biasadj=TRUE)
forecast_3$mean <- InvBoxCox(forecast_3$mean,lambda=lambda_3)
forecast_3$upper <- InvBoxCox(forecast_3$upper, lambda = lambda_3)
forecast_3$lower <- InvBoxCox(forecast_3$lower, lambda = lambda_3)
forecast_3$x <- ts(tseries[1:length(forecast_3$x)], start = c(2020, dayofYear), frequency = 365)
autoplot(forecast_3, ylim=c(0,600))


plot(forecast_3, ylim=c(0,700), xlim=c(2020.66, 2020.822))
for (i in 1:5){ 
  lines(InvBoxCox(simulate(best_model_3, 54), lambda = lambda_3), 
        type="l", col="red")
  lines(tseries)
}

'''
##Definerer Sarimamodel
SarimaModel = list(ar=best_model$coef[1], sma = best_model$coef[2:3], 
                   iorder=1,siorder=1,nseasons=7, sigma2=1.2)

##Loading the best model from exercise 3
best_model_3 <- readRDS("model_3.rds")

SarimaModel_3 = list(ar=best_model_3$coef[1], sma = best_model_3$coef[2:3], 
                   iorder=1,siorder=1,nseasons=7, sigma2=1.5)

SarimaModel_3

#Simulate five corresponding two week realizations 


Mulig det er noe feil i denne funksjonen 
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
'''

plot(simulate(best_model, nsim = 14))
plot(sim_sarima(n=14, model = list(nseasons=52)))

#GARCH
plot(best_model$residuals)
acf(abs(best_model$residuals))
acf(best_model$residuals^2)
pacf(best_model$residuals^2, main = "") # try garch(1,1)

#best_model
lnames <- c(paste0("ar", which(sapply(best_model$model$phi, function(th) {
  isTRUE(all.equal(th, 0))
}))), paste0("ma", which(sapply(best_model$model$theta, function(th) {
  isTRUE(all.equal(th, 0))
}))))
constraints <- rep(list(0), length(lnames))
names(constraints) <- lnames
order <- c(length(best_model$model$phi), length(best_model$model$theta))


lowest.aicc = 10000
for (p in 0:4) {
  for (q in 0:1) {
    if (p>0 | q>0) {
      garchmod = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p,q)), 
                            mean.model = list(armaOrder = order, include.mean = TRUE), 
                            distribution.model = "std", fixed.pars = constraints)
      fit = ugarchfit(spec = garchmod, data = transformed.seasonal)
      
      j = p + q + 1.0
      n = length(transformed.seasonal)
      aicc = infocriteria(fit)[1] + 2*j*(j+1)/(n*(n-j-1))
      if (aicc<lowest.aicc){
        best.garch = fit
        lowest.aicc = aicc
      }
    }
  }
}

best.garch 

j = 3
aicc = infocriteria(best.garch)[1] + 2*j*(j+1)/(n*(n-j-1))
aicc


checkresiduals(best.garch@fit)
