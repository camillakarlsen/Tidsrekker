library(readxl)
library(astsa)
library(itsmr)

df <- read_xlsx("xls.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="o", xlab="Date", ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")


#PACF page 86
#x = df$`Nye tilfeller`
#pacf(x, lag = length(x)-1, pl= TRUE)
# skal vi bruke kumulativt antall eller nye tilfeller?
y = df$`Kumulativt antall`
pacf(y, lag = length(y)-1, pl = TRUE) #litt rart at kun pacf ved lag 0 er utenfor de blå strekene?
mu = mean(y) # sample mean
mu
autocov = acvf(y, length(y)-1)
gamma0 = autocov[1] # acvf at lag 0
gamma1 = autocov[2]
