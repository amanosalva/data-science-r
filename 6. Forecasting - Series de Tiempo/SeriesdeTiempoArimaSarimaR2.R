rm(list=ls())

# Autor : Jose Cardenas

# Librerias
#install.packages("ggplot2")
library(ggplot2)
#install.packages("TSA")
library(TSA)
#install.packages("forecast")
library(forecast)
library(scales)
library(stats)
#library(arima)

# Carga de datos
Yt<-read.delim("datatesisaereo.txt",header=T)
Yt<-ts(Yt,start=c(2000,1),freq=12)

date<-seq(as.Date("2000/01/01"),as.Date("2012/12/01"),by="months")

data<-data.frame(Yt,date)

fit<-auto.arima(Yt)
fit

################################################################################
################################ IDENTIFICACION ################################
################################################################################

############
# ORIGINAL #
############

# Plot
plot(Yt,col="lightblue",
     main="Grafico de la serie ...",
     ylab="Viajeros (en miles)",
     xlab="Tiempo")


##########################
# TRANSFORMACION BOX-COX #
##########################

# Usamos la transformacion de Box-Cox
lambda<-BoxCox.lambda(Yt,lower=-5,upper=5)
Yt_t<-BoxCox(Yt,lambda=lambda)


# Plot
plot(Yt_t,col="lightblue",
     main="Grafico de la serie .......",
     ylab="Migracion(ejemplo) (en miles)",
     xlab="Tiempo")
# Necesitaremos diferenciar al menos regularmente


# ACF
acf(Yt_t,lag.max=39,
    xaxp=c(0,16,16),
    main="ACF de la serie ......",
    ylab="Autocorrelaciones",
    xlab="Retardo")


# PACF
pacf(Yt_t,lag.max=39,
     xaxp=c(0,16,16),
     main="PACF de la serie .......",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")


# Parte regular: AR y MA 
# Parte estacional: AR y MA 
# (1,_,1)(1,_,1)[12]


##############
# 1 DIFF REG #
##############

# Diferencio la serie 1  vez regularmente
Yt_t_1r<-diff(Yt_t,1,1)

# Plot
plot(Yt_t_1r,
     main="Grafico de la serie ...... diferenciada una vez regularmente",
     ylab="Migraciones (en miles)",
     xlab="Tiempo")
# Se estabiliza en nivel pero no del todo en variabilidad
# No necesitaremos diferenciacion estacional


# ACF
acf(Yt_t_1r,lag.max=39,
    xaxp=c(0,16,16),
    main="ACF de la serie ......",
    ylab="Autocorrelaciones",
    xlab="Retardo")
# AR
# (_,1,_)(1,0,1)[12]


# PACF
pacf(Yt_t_1r,lag.max=39,
     xaxp=c(0,16,16),
     main="PACF de la serie .......",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")
# AR
# (1,1,1)(2,0,1)[]


##########################################################################
################################# AJUSTE #################################
##########################################################################

# Ajustamos un modelo SARIMA(0,1,1)(0,1,1)[12]
fit<-Arima(Yt,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
fit


############################################################################
################################ VALIDACIÓN ################################
############################################################################

# Plot de residuales
plot(fit$residuals,
     main="",
     ylab="",
     xlab="Tiempo")

# Normalidad de residuos
qqnorm(fit$residuals)
qqline(fit$residuals)
ks.test(fit$residuals,"pnorm")

# ACF de residuales
acf(fit$residuals,lag.max=39,   ##n/4
    xaxp=c(0,20,20),
    main="",   
    ylab="Autocorrelacion",
    xlab="Retardo")

# Ljung-Box test
Box.test(fit$residuals,lag=1,type="Ljung-Box",fitdf=0)

# Box y Pierce test
Box.test(fit$residuals,lag=1,type="Box-Pierce",fitdf=0)


# Serie original, estimacion y pronosticos con ic
plot(forecast(fit,h=24),col="red")

