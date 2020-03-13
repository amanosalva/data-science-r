rm(list=ls())

#########################################################################
### -- Programa de Especializaci�n Nivel 1-- ## 
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

#Modelaremos bajo un enfoque de descomposici�n

# Agrupacion de meses
boxplot(Yt ~ cycle(Yt))
# Para detectar si hay estacionalidad, no debe estar en el nivel de las medias.
cycle(Yt)

## descomposicion
Yt.ts.desc = decompose(Yt,type="multiplicative") #Si no coloco el tipo por defecto es el aditivo.
plot(Yt.ts.desc, xlab='A�o')
Yt.ts.desc 

#EXTRACCI�N DE LA TENDENCIA
tendencia=data.frame(Yt.ts.desc$trend)

#A LOS DATOS FALTANTES PONERLE 0
tendencia$Yt.ts.desc.trend[is.na(tendencia$Yt.ts.desc.trend)]=0

#
tendencia$x=seq(1:nrow(tendencia))

#REALIZANDO UN MODELO DE REGRESI�N LINEAL SIMPLE
modelo=lm(Yt.ts.desc.trend~x,data=tendencia)

#ESTIMACI�N DE LA TENDENCIA
tendencia_estimada=modelo$fitted.values

#EXTRAIGO LOS COEFICIENTES ESTACIONARIOS
estacional=data.frame(Yt.ts.desc$seasonal)

# estimacion de la serie

dataf=data.frame(tendencia_estimada,estacional);colnames(dataf)=c('tend_est','estacionalidad')

#ESTIMACI�N DE LA SERIE MEDIANTE UN ENFOQUE DE DESCOMPOSICI�N MULTIPLICATIVO
dataf$Yt_est=dataf$tend_est*dataf$estacionalidad

## Grafico del modelo final

plot(Yt,col="red")
points(dataf$Yt_est,type='l',col="blue")

#razones por la que sale caquita
#UN ENFOQUE DE DESCOMPOSICI�N NO ASUME CAMBIOS DE ESTRUCTURA
#PODRIAMOS USAR UNA CUADR�TICA EN VEZ DE LINEAL


#VISUALIZAR LOS PRON�STICOS.

dataf$Yt_est


#Indicadores
#-SSE
#-MAE
#-AIC
#-MAPE
#-BIC

