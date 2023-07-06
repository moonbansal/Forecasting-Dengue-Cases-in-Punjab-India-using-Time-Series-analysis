install.packages("vars")
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

Cases <- ts(fategarh$Cases, start = c(2018,1,1), frequency = 12)
HI <- ts(fategarh$HI, start = c(2018,1,1), frequency = 12)
CI <- ts(fategarh$CI, start = c(2018,1,1), frequency = 12)
BI <- ts(fategarh$BI, start = c(2018,1,1), frequency = 12)

ts_plot(Cases)
ts_plot(HI)
ts_plot(CI)
ts_plot(BI)

pp.test(Cases)
pp.test(HI)
pp.test(CI)
pp.test(BI)

v1 <- cbind(Cases, HI, CI, BI)
colnames(v1) <- cbind("Cases", "HI", "CI", "BI")

lagselect <- VARselect(v1, lag.max = 15, type = "const")
lagselect$selection

Model1 <- VAR(v1, p = 3, type = "const", season = NULL, exog = NULL)
summary(Model1)

Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1


forecast <- predict(Model1, n.ahead = 12, ci = 0.95)
fanchart(forecast, names = "Cases", main = "Fanchart for Cases", xlab = "Month", ylab = "Cases")
fanchart(forecast, names = "HI", main = "Fanchart for HI",xlab = "Month", ylab = "HI")
fanchart(forecast, names = "CI", main = "Fanchart for CI", xlab = "Month", ylab = "CI")
fanchart(forecast, names = "BI", main = "Fanchart for BI", xlab = "Month", ylab = "BI")
forecast

# Forecasting for 2022
Cases2 <- ts(fategarh$Cases[1:48], start = c(2018,1,1), frequency = 12)
HI2 <- ts(fategarh$HI[1:48], start = c(2018,1,1), frequency = 12)
CI2 <- ts(fategarh$CI[1:48], start = c(2018,1,1), frequency = 12)
BI2 <- ts(fategarh$BI[1:48], start = c(2018,1,1), frequency = 12)

v2 <- cbind(Cases2, HI2, CI2, BI2)
colnames(v2) <- cbind("Cases2", "HI2", "CI2", "BI2")

Model2 <- VAR(v2, p = 3, type = "const", season = NULL, exog = NULL)
summary(Model2)

forecast2 <- predict(Model2, n.ahead = 12, ci = 0.95)
fanchart(forecast2, names = "Cases2", main = "Fanchart for Cases", xlab = "Month", ylab = "Cases")
fanchart(forecast2, names = "HI2", main = "Fanchart for HI",xlab = "Month", ylab = "HI")
fanchart(forecast2, names = "CI2", main = "Fanchart for CI", xlab = "Month", ylab = "CI")
fanchart(forecast2, names = "BI2", main = "Fanchart for BI", xlab = "Month", ylab = "BI")
forecast2


#forecasting 2022 for jalandhar 

Cases3 <- ts(jalandhar$Cases[1:48], start = c(2018,1,1), frequency = 12)
HI3 <- ts(jalandhar$HI[1:48], start = c(2018,1,1), frequency = 12)
CI3 <- ts(jalandhar$CI[1:48], start = c(2018,1,1), frequency = 12)
BI3 <- ts(jalandhar$BI[1:48], start = c(2018,1,1), frequency = 12)

v3 <- cbind(Cases3, HI3, CI3, BI3)
colnames(v3) <- cbind("Cases3", "HI3", "CI3", "BI3")

Model3 <- VAR(v3, p = 3, type = "const", season = NULL, exog = NULL)
summary(Model3)

forecast3 <- predict(Model3, n.ahead = 12, ci = 0.95)
fanchart(forecast3, names = "Cases3", main = "Fanchart for Cases", xlab = "Month", ylab = "Cases")
fanchart(forecast2, names = "HI2", main = "Fanchart for HI",xlab = "Month", ylab = "HI")
fanchart(forecast2, names = "CI2", main = "Fanchart for CI", xlab = "Month", ylab = "CI")
fanchart(forecast2, names = "BI2", main = "Fanchart for BI", xlab = "Month", ylab = "BI")
forecast3
