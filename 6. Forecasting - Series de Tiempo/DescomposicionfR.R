rm(list=ls())

#########################################################################
### -- Programa de Especialización Nivel 1-- ## 
#########################################################################
### Autores: Jose Cardenas - Andre Chavez ## 

#########################################################################

library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)
#library(arima)

# Carga de datos
Yt<-read.delim("datatesisaereo.txt",header=T)
head(Yt,20)
Yt<-ts(Yt,start=c(2000,1),freq=12) #12 mensual
Yt
# Grafico de la serie
plot(Yt)
#Observamos un cambio de estructura por el cambio de gobierno

#Modelaremos bajo un enfoque de descomposición

# Agrupacion de meses
boxplot(Yt ~ cycle(Yt))
# Para detectar si hay estacionalidad, no debe estar en el nivel de las medias.
cycle(Yt)

## descomposicion
Yt.ts.desc = decompose(Yt,type="multiplicative") #Si no coloco el tipo por defecto es el aditivo.
plot(Yt.ts.desc, xlab='Año')
Yt.ts.desc 

#EXTRACCIÓN DE LA TENDENCIA
tendencia=data.frame(Yt.ts.desc$trend)

#A LOS DATOS FALTANTES PONERLE 0
tendencia$Yt.ts.desc.trend[is.na(tendencia$Yt.ts.desc.trend)]=0

#
tendencia$x=seq(1:nrow(tendencia))

#REALIZANDO UN MODELO DE REGRESIÓN LINEAL SIMPLE
modelo=lm(Yt.ts.desc.trend~x,data=tendencia)

#ESTIMACIÓN DE LA TENDENCIA
tendencia_estimada=modelo$fitted.values

#EXTRAIGO LOS COEFICIENTES ESTACIONARIOS
estacional=data.frame(Yt.ts.desc$seasonal)

# estimacion de la serie

dataf=data.frame(tendencia_estimada,estacional);colnames(dataf)=c('tend_est','estacionalidad')

#ESTIMACIÓN DE LA SERIE MEDIANTE UN ENFOQUE DE DESCOMPOSICIÓN MULTIPLICATIVO
dataf$Yt_est=dataf$tend_est*dataf$estacionalidad

## Grafico del modelo final

plot(Yt,col="red")
points(dataf$Yt_est,type='l',col="blue")

#razones por la que sale caquita
#UN ENFOQUE DE DESCOMPOSICIÓN NO ASUME CAMBIOS DE ESTRUCTURA
#PODRIAMOS USAR UNA CUADRÁTICA EN VEZ DE LINEAL


#VISUALIZAR LOS PRONÓSTICOS.

dataf$Yt_est


#Indicadores
#-SSE
#-MAE
#-AIC
#-MAPE
#-BIC


