rm(list=ls())

#########################################################################
### -- Programa de Especialización Nivel 1-- ## 
#########################################################################
### Autores: Jose Cardenas - Andre Chavez ## 

#########################################################################

# ######### 1) LIBRERIAS A UTILIZAR ################# 

gas = scan('http://www.uam.es/joser.berrendero/datos/gas6677.dat')
plot(gas)
gas.ts = ts(gas, start = c(1966,1), frequency = 12)
print(gas.ts)
plot(gas.ts)
boxplot(gas.ts ~ cycle(gas.ts))
cycle(gas.ts)

## descomposicion
gas.ts.desc = decompose(gas.ts)
plot(gas.ts.desc, xlab='Año')
gas.ts.desc 

##iotros

data(AirPassengers)
plot(AirPassengers)
data(co2)
plot(co2) 
plot(decompose(co2))
plot(decompose(AirPassengers,type="multiplicative")) 

#Para calcular la tendencia con "decompose":

TendenciaAir<-decompose(AirPassengers)$trend
plot(TendenciaAir) 

#Para calcular la tendencia con "filter":
plot(filter(AirPassengers,filter=rep(1/3,3),sides=2,method="convolution"))
plot(filter(AirPassengers,filter=rep(1/5,5),sides=2,method="convolution"))
plot(filter(AirPassengers,filter=rep(1/9,9),sides=2,method="convolution")) 

## ejercicio
jj = scan('http://www.uam.es/joser.berrendero/datos/jj.dat')
