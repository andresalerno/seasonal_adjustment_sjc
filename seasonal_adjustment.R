
# Document Information ----
my.d <- rstudioapi::getActiveDocumentContext()

# Document Path ----
my.file.location <- rstudioapi::getActiveDocumentContext()$path

# Directory Path ----
my.dir <- dirname(my.file.location)

# Setting up the working directory ----
setwd(my.dir)

# Loading important libraries ----
if(!require("fpp")) install.packages("fpp")
if(!require("fpp2")) install.packages("fpp2")
if(!require("seasonal")) install.packages("seasonal")

library(fpp)
library(fpp2)

# 1) Fetching data beer ----

data(ausbeer)
ts_beer = ts(ausbeer, frequency = 4, start = 1956)

# Using Additive Model (Time Series Decomposition) # Formula -> Seasonally Adjusted = Times Series - Seasonal ----
decompose_beer = decompose(ts_beer, "additive")
adjust_beer = ts_beer - decompose_beer$seasonal

# Plotting ----
plot(adjust_beer)


# 2) Fetching Airpassengers ----

library(Ecdat)
data(AirPassengers)
ts_AirPassengers = ts(AirPassengers, frequency = 12, start = 1949)

# Using Additive Model (Time Series Decomposition) # Formula -> Seasonally Adjusted = Times Series - Seasonal ----
decompose_AirPassengers = decompose(ts_AirPassengers, "multiplicative")
adjust_AirPassengers = ts_AirPassengers / decompose_AirPassengers$seasonal

# Plotting ----
plot(adjust_AirPassengers, col = "red", lwd = 2)


# Seasonally adjusted time series provide a way to understand the underlying trends in data by removing 
# the “noise” of seasonal fluctations so outliers and anomalies are easier to see. Just as removing 
# seasonality makes problems easier to spot with your eyes, it also makes them easier for the computer.


# 3) Fetching Airpassengers using seasonal package ----
# Resource: https://www.youtube.com/watch?v=Tnjsik2ClT4&t=98s

# Fetching data ----
library(seasonal)
library(seasonalview)
AirPassengers
# Checking installation
checkX13(fail = FALSE, fullcheck = TRUE, htmlcheck = TRUE)

# Knowing the dataset ----

str(AirPassengers)

plot(AirPassengers, main = "Série Temporal: AirPassengers", lwd = 2)


# Seasonal adjustment using X13-ARIMA-SEATS ----
ajuste <- seas(AirPassengers)

# Significância dos parâmetros ----
summary(ajuste)
# Manually identify outliers
identify(ajuste, type = c("ao", "tc", "ls"))

udg(ajuste, "x13mdl")
view(ajuste)

# Plotting ----
plot(ajuste, main = "Air Passengers com ajuste sazonal")

# Gráfico dos fatores sazonais
monthplot(ajuste, col.base = 1, labels = month.abb)
legend("topright", legend = c("SI", "FS", "MÉDIA FS"), lty = 1, lwd = c(1, 2, 2), col = c(4, 2, 1), cex = 0.7)
# espera-se que as linhas vermelhas acompanhem bem as linhas azuis



# Diagnóstico de sazonalidade
qs(ajuste)

# Série com ajuste sazonal
serie_adjust <- series(ajuste, "s11")
plot(serie_adjust)

# Predict ----
predict(ajuste, AirPassengers)

# OUTPUT ----
#Call:
#  seas(x = AirPassengers)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  Weekday           -0.0029497  0.0005232  -5.638 1.72e-08 *** # variáveis de regressão (ETAPAS DE PRE-AJUSTE)
#  Easter[1]          0.0177674  0.0071580   2.482   0.0131 *   # variáveis de regressão (ETAPAS DE PRE-AJUSTE)
#  AO1951.May         0.1001558  0.0204387   4.900 9.57e-07 *** # variáveis de regressão (ETAPAS DE PRE-AJUSTE)
#  MA-Nonseasonal-01  0.1156205  0.0858588   1.347   0.1781     # parâmetros do modelo ARIMA (esse parâmetro não é significativo - não tem asterisco). Você pode estimar o modelo retirando esse parâmetro
#  MA-Seasonal-12     0.4973600  0.0774677   6.420 1.36e-10 *** # parãmetros do modelo ARIMA 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#SEATS adj.  ARIMA: (0 1 1)(0 1 1)  Obs.: 144  Transform: log
#AICc: 947.3, BIC: 963.9  QS (no seasonality in final):    0  
#Box-Ljung (no autocorr.): 26.65   Shapiro (normality): 0.9908
#
# Qual foi o modelo ARIMA usado? ARIMA: (0 1 1)(0 1 1)
# Foi usada transformação dos dados? Sim, Transform: log
# Critérios de informação? AICc: 947.3, BIC: 963.9
# Estatística QS para o diagnóstico de sazonalidade: QS (no seasonality in final):    0 # não encontrou sazonalidade nos dados (0)
#  

