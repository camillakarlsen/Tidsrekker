library(readxl)
library(astsa)

df <- read_xlsx("xls.xlsx",skip=1)
df$Dato <- as.Date(df$Dato, "%d.%m.%Y") 

#Plot the data
plot(df$Dato, df$`Kumulativt antall`, type="o", xlab="Date", ylab= "Cumulative cases")
plot(df$Dato, df$`Nye tilfeller`, type="o", xlab="Date", ylab= "New cases")


#PACF page 86
x = df$`Nye tilfeller`
pacf(x, lag = length(x), pl= TRUE)
#y = df$`Kumulativt antall`
#pacf(y, lag = length(y), pl = TRUE)
