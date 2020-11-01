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

acf(transformed.seasonal, main="")  
pacf(transformed.seasonal, main="") 

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


#Simulating 

# sarima model
plot(forecast, ylim=c(0,700), xlim=c(2020.76, 2020.822))
for (i in 1:5){ 
  lines(InvBoxCox(simulate(best_model, 14), lambda = lambda), 
        type="l", col="red")
}

# model from ex3
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
      garchmod = ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(p,q)), 
                            mean.model = list(armaOrder = order, 
                                              include.mean = TRUE), 
                            distribution.model = "std", 
                            fixed.pars = constraints)
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
