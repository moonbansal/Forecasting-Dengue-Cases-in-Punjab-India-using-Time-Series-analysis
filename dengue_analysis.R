library(GGally)
library(stats)
library(dplyr)
library(tidyverse)
library(seasonal)
library(prophet)

# Direct correlations function
corrf <- function(df) {
  plot(df[ ,2:5])
  correlations <- round(cor(df[,2:5]), digit=2)
  print(correlations)
  GGally::ggpairs(df[,2:5])
}

#Function for lagged correlations 
lagcorrf <- function(factor1, factor2, title) {
  lagcorr <- ccf(factor1, factor2, lag.max=10, plot=FALSE)
  plot(lagcorr, main=title)
  axis(side = 1, at = seq(-10, 10, by = 2))
  print(lagcorr)
}


#Temporal analysis plots function...check again 

temporal <- function(df, region) {
  ts_main <- ts(df[,2:5], frequency = 12, start=2018, end=2022)
  
  plot(ts_main, xlab ="Monthly Data (2018-2021)",
       main =region,
       col.main ="darkgreen")
}

# CALCULATIONS--------

# Amritsar
amritsar <- read.csv("amritsar.csv")
amritsar <- na.omit(amritsar)
corrf(amritsar)
lagcorrf(amritsar$Cases, amritsar$HI, "Amritsar Cases vs HI")
lagcorrf(amritsar$Cases, amritsar$CI, "Amritsar Cases vs CI")
lagcorrf(amritsar$Cases, amritsar$BI, "Amritsar Cases vs BI")
temporal(amritsar, "Amritsar")

# Barnala 
barnala <- read.csv("Barnala.csv")
barnala <- na.omit(barnala)
corrf(barnala)
lagcorrf(barnala$Cases, barnala$HI, "Barnala Cases vs HI")
lagcorrf(barnala$Cases, barnala$CI, "Barnala Cases vs CI")
lagcorrf(barnala$Cases, barnala$BI, "Barnala Cases vs BI")
temporal(barnala, "Barnala")

# Bathinda 
bathinda <- read.csv("Bathinda.csv")
bathinda <- na.omit(bathinda)
corrf(bathinda)
lagcorrf(bathinda$Cases, bathinda$HI, "Bathinda Cases vs HI")
lagcorrf(bathinda$Cases, bathinda$CI, "Bathinda Cases vs CI")
lagcorrf(bathinda$Cases, bathinda$BI, "Bathinda Cases vs BI")
temporal(bathinda, "Bathinda")

# Faridkot
faridkot <- read.csv("Faridkot.csv")
faridkot <- na.omit(faridkot)
corrf(faridkot)
lagcorrf(faridkot$Cases, faridkot$HI, "Faridkot Cases vs HI")
lagcorrf(faridkot$Cases, faridkot$CI, "Faridkot Cases vs CI")
lagcorrf(faridkot$Cases, faridkot$BI, "Faridkot Cases vs BI")
temporal(faridkot, "Faridkot")

# Fategarh sahib
fategarh <- read.csv("Fatehgarh Sahib.csv")
fategarh <- na.omit(fategarh)
corrf(fategarh)
lagcorrf(fategarh$Cases, fategarh$HI, "Fategarh Cases vs HI")
lagcorrf(fategarh$Cases, fategarh$CI, "Fategarh Cases vs CI")
lagcorrf(fategarh$Cases, fategarh$BI, "Fategarh Cases vs BI")
temporal(fategarh, "Fatehgarh Sahib")

# Fazilka 
fazilka <- read.csv("fazilka.csv")
fazilka <- na.omit(fazilka)
corrf(fazilka)
lagcorrf(fazilka$Cases, fazilka$HI, "Fazilka cases vs HI")
lagcorrf(fazilka$Cases, fazilka$CI, "Fazilka cases vs CI")
lagcorrf(fazilka$Cases, fazilka$BI, "Fazilka cases vs BI")
temporal(fazilka, "Fazilka")

# Ferozpur 
ferozpur <- na.omit(read.csv("ferozpur.csv", header = TRUE))
corrf(ferozpur)
lagcorrf(ferozpur$Cases, ferozpur$HI, "Ferozpur cases vs HI")
lagcorrf(ferozpur$Cases, ferozpur$CI, "Ferozpur cases vs CI")
lagcorrf(ferozpur$Cases, ferozpur$BI, "Ferozpur cases vs BI")
temporal(ferozpur, "Ferozpur")

#Gurdaspur 
gurdaspur <- na.omit(read.csv("gurdaspur.csv", header = TRUE))
corrf(gurdaspur)
lagcorrf(gurdaspur$Cases, gurdaspur$HI, "Gurdaspur cases vs HI")
lagcorrf(gurdaspur$Cases, gurdaspur$CI, "Gurdaspur cases vs CI")
lagcorrf(gurdaspur$Cases, gurdaspur$BI, "Gurdaspur cases vs BI")
temporal(gurdaspur, "Gurdaspur")

#Hoshiarpur 
hoshiarpur <- na.omit(read.csv("hoshiarpur.csv", header = TRUE))
corrf(hoshiarpur)
lagcorrf(hoshiarpur$Cases, hoshiarpur$HI, "Hoshiarpur cases vs HI")
lagcorrf(hoshiarpur$Cases, hoshiarpur$CI, "Hoshiarpur cases vs CI")
lagcorrf(hoshiarpur$Cases, hoshiarpur$BI, "Hoshiarpur cases vs BI")
temporal(hoshiarpur, "Hoshiarpur")

#Jalandhar 
jalandhar <- na.omit(read.csv("jalandhar.csv", header = TRUE))
corrf(jalandhar)
lagcorrf(jalandhar$Cases, jalandhar$HI, "Jalandhar cases vs HI")
lagcorrf(jalandhar$Cases, jalandhar$CI, "Jalandhar cases vs CI")
lagcorrf(jalandhar$Cases, jalandhar$BI, "Jalandhar cases vs BI")
temporal(jalandhar, "Jalandhar")

#Kapurthala 
kapurthala <- na.omit(read.csv("kapurthala.csv", header = TRUE))
corrf(kapurthala)
lagcorrf(kapurthala$Cases, kapurthala$HI, "Kapurthala cases vs HI")
lagcorrf(kapurthala$Cases, kapurthala$CI, "Kapurthala cases vs CI")
lagcorrf(kapurthala$Cases, kapurthala$BI, "Kapurthala cases vs BI")
temporal(kapurthala, "Kapurthala")

#ludhiana 
ludhiana <- na.omit(read.csv("ludhiana.csv", header = TRUE))
corrf(ludhiana)
lagcorrf(ludhiana$Cases, ludhiana$HI, "Ludhiana cases vs HI")
lagcorrf(ludhiana$Cases, ludhiana$CI, "Ludhiana cases vs CI")
lagcorrf(ludhiana$Cases, ludhiana$BI, "Ludhiana cases vs BI")
temporal(ludhiana, "Ludhiana")

#mansa 
mansa <- na.omit(read.csv("mansa.csv", header = TRUE))
corrf(mansa)
lagcorrf(mansa$Cases, mansa$HI, "mansa cases vs HI")
lagcorrf(mansa$Cases, mansa$CI, "mansa cases vs CI")
lagcorrf(mansa$Cases, mansa$BI, "mansa cases vs BI")
temporal(mansa, "mansa")

#moga
moga <- na.omit(read.csv("moga.csv", header = TRUE))
corrf(moga)
lagcorrf(moga$Cases,moga$HI, "Moga cases vs HI")
lagcorrf(moga$Cases,moga$CI, "Moga cases vs CI")
lagcorrf(moga$Cases,moga$BI, "Moga cases vs BI")
temporal(moga, "Moga")

#pathankot
pathankot <- na.omit(read.csv("pathankot.csv", header = TRUE))
corrf(pathankot)
lagcorrf(pathankot$Cases, pathankot$HI, "Pathankot cases vs HI")
lagcorrf(pathankot$Cases, pathankot$CI, "Pathankot cases vs CI")
lagcorrf(pathankot$Cases, pathankot$BI, "Pathankot cases vs BI")
temporal(pathankot, "pathankot")

#patiala 
patiala <- na.omit(read.csv("patiala.csv", header = TRUE))
corrf(patiala)
lagcorrf(patiala$Cases, patiala$HI, "patiala cases vs HI")
lagcorrf(patiala$Cases, patiala$CI, "patiala cases vs CI")
lagcorrf(patiala$Cases, patiala$BI, "patiala cases vs BI")
temporal(patiala, "patiala")

#rupnagar
rupnagar <- na.omit(read.csv("rupnagar.csv", header = TRUE))
corrf(rupnagar)
lagcorrf(rupnagar$Cases, rupnagar$HI, "Rupnagar cases vs HI")
lagcorrf(rupnagar$Cases, rupnagar$CI, "Rupnagar cases vs CI")
lagcorrf(rupnagar$Cases, rupnagar$BI, "Rupnagar cases vs BI")
temporal(rupnagar, "rupnagar")

#sangrur 
sangrur <- na.omit(read.csv("sangrur.csv", header = TRUE))
corrf(sangrur)
lagcorrf(sangrur$Cases, sangrur$HI, "sangrur cases vs HI")
lagcorrf(sangrur$Cases, sangrur$CI, "sangrur cases vs CI")
lagcorrf(sangrur$Cases, sangrur$BI, "sangrur cases vs BI")
temporal(sangrur, "sangrur")

#sas nagar
sas <- na.omit(read.csv("sasnagar.csv", header = TRUE))
corrf(sas)
lagcorrf(sas$Cases, sas$HI, "sas nagar cases vs HI")
lagcorrf(sas$Cases, sas$CI, "sas nagar cases vs CI")
lagcorrf(sas$Cases, sas$BI, "sas nagar cases vs BI")
temporal(sas, "sas nagar")

#sbs nagar
sbs <- na.omit(read.csv("sbsnagar.csv", header = TRUE))
corrf(sbs)
lagcorrf(sbs$Cases, sbs$HI, "SBS cases vs HI")
lagcorrf(sbs$Cases, sbs$CI, "SBS cases vs CI")
lagcorrf(sbs$Cases, sbs$BI, "SBS cases vs BI")
temporal(sbs, "sbs nagar")

# shir muktsar sahib
shir <- na.omit(read.csv("shirmuktsar.csv", header = TRUE))
corrf(shir)
lagcorrf(shir$Cases, shir$HI, "Shir muktsar cases vs HI")
lagcorrf(shir$Cases, shir$CI, "Shir muktsar cases vs CI")
lagcorrf(shir$Cases, shir$BI, "Shir muktsar cases vs BI")
temporal(shir, "shir muktsar sahib")

#taran taran 
taran <- na.omit(read.csv("tarantaran.csv", header = TRUE))
corrf(taran)
lagcorrf(taran$Cases, taran$HI, "Taran taran cases vs HI")
lagcorrf(taran$Cases, taran$CI, "Taran taran cases vs CI")
lagcorrf(taran$Cases, taran$BI, "Taran taran cases vs BI")
temporal(taran, "taran taran")


# TRYING PROPHET 

fore_func_future <- function(region) {
  region$ds <- as.Date(region$Date, format = "%d/%m/%y")
  region$y <- region$Cases
  m <- prophet(region)
  future <- make_future_dataframe(m, periods = 12, freq = "month", include_history = TRUE)
  forecast <- predict(m, future)
  dyplot.prophet(m, forecast) #interactive plot
}

fore_ground <- function(region) {
  region$ds <- as.Date(region$Date, format = "%d/%m/%y")
  region$y <- region$Cases
  m2 <- prophet(region[1:48,])
  future2 <- make_future_dataframe(m2, periods = 12, freq = "month", include_history = TRUE)
  forecast2 <- predict(m2, future2)
  dyplot.prophet(m2, forecast2)
}

fore_func_future(amritsar)
fore_ground(amritsar)

fore_func_future(barnala)
fore_ground(barnala)

fore_func_future(bathinda)
fore_ground(bathinda)

fore_func_future(faridkot)
fore_ground(faridkot)

fore_func_future(fategarh)
fore_ground(fategarh)

fore_func_future(fazilka)
fore_ground(fazilka)

fore_func_future(ferozpur)
fore_ground(ferozpur)

fore_func_future(gurdaspur)
fore_ground(gurdaspur)

fore_func_future(hoshiarpur)
fore_ground(hoshiarpur)

fore_func_future(jalandhar)
fore_ground(jalandhar)

fore_func_future(kapurthala)
fore_ground(kapurthala)

fore_func_future(ludhiana)
fore_ground(ludhiana)

fore_func_future(mansa)
fore_ground(mansa)

fore_func_future(moga)
fore_ground(moga)

fore_func_future(pathankot)
fore_ground(pathankot)

fore_func_future(patiala)
fore_ground(patiala)

fore_func_future(rupnagar)
fore_ground(rupnagar)

fore_func_future(sangrur)
fore_ground(sangrur)

fore_func_future(sas)
fore_ground(sas)

fore_func_future(sbs)
fore_ground(sbs)

fore_func_future(shir)
fore_ground(shir)

fore_func_future(taran)
fore_ground(taran)

#plot(m, forecast, xlabel = "Years", ylabel = "Cases")
#prophet_plot_components(m, forecast)

#df.cv <- cross_validation(m, units = "days", initial = 1096, period = 120, horizon = 360)
#plot_cross_validation_metric(df.cv, metric = "mse")
