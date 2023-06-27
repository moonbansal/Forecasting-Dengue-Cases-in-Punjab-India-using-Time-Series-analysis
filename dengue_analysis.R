library(GGally)
library(stats)
library(dplyr)
library(tidyverse)
library(seasonal)

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

# Ferozpur 
ferozpur <- na.omit(read.csv("ferozpur.csv", header = TRUE))

#Gurdaspur 
gurdaspur <- na.omit(read.csv("gurdaspur.csv", header = TRUE))

#Hoshiarpur 
hoshiarpur <- na.omit(read.csv("hoshiarpur.csv", header = TRUE))

#Jalandhar 
jalandhar <- na.omit(read.csv("jalandhar.csv", header = TRUE))

#Kapurthala 
kapurthala <- na.omit(read.csv("kapurthala.csv", header = TRUE))

#ludhiana 
ludhiana <- na.omit(read.csv("ludhiana.csv", header = TRUE))

#mansa 
mansa <- na.omit(read.csv("mansa.csv", header = TRUE))

#moga
moga <- na.omit(read.csv("moga.csv", header = TRUE))

#pathankot
pathankot <- na.omit(read.csv("pathankot.csv", header = TRUE))

#patiala 
patiala <- na.omit(read.csv("patiala.csv", header = TRUE))

#rupnagar
rupnagar <- na.omit(read.csv("rupnagar.csv", header = TRUE))

#sangrur 
sangrur <- na.omit(read.csv("sangrur.csv", header = TRUE))

#sas nagar
sas <- na.omit(read.csv("sasnagar.csv", header = TRUE))

#sbs nagar
sbs <- na.omit(read.csv("sbsnagar.csv", header = TRUE))

# shir muktsar sahib
shir <- na.omit(read.csv("shirmuktsar.csv", header = TRUE))

#taran taran 
taran <- na.omit(read.csv("tarantaran.csv", header = TRUE))