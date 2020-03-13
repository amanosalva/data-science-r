
rm(list=ls())
##########################################################################
##### -- Programa de EspecializaciÃ³n en Data Science - Nivel I -- ########
##########################################################################
######## Tema : Modelos de Regresion Avanzados ###########################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

library("readxl")
library("ggplot2")
library(glmnet)  #para regresiones penalizadas, necesitamos que los datos estén forma de matriz
library(gee)
library(readxl)
library(leaps)


#### -- 2) Modelo de Regresion Lineal

## Cargar la data

regtecnica=read.csv("regtecnica.csv",header=T,sep=";",dec=",")

## Particion Muestral
set.seed(1234) 
sample <- sample.int(nrow(regtecnica), round(.8*nrow(regtecnica)))
regtecnica.train<- regtecnica[sample, ]
regtecnica.valid <- regtecnica[-sample, ]

##################
#En clasificación usamos otra sentencia para partición muestreal. Usaremos otra sentencia.
##################


## supuestos
#Se considera alto a los que son mayor a 0.6
cor(regtecnica) # multicolinealidad

## primer modelo

modelo=lm(potenciaresp~.,data=regtecnica.train)
summary(modelo)
#trafico, tiempo, densidad son variables que no son significativas
#El R cuadrado ajustado le quita el número de variables, el cual a más cantidad inflaría el R cuadrado normal. Para comparar modelos se usaría el R cuadrado ajustado
# y si se quiere interpretar sería el R cuadrado normal.

## indicadores

modelo$coefficients
predlm=predict(modelo,regtecnica.valid)
#Correlación entre lo predecido y el original mientras más cercano a 1, nuestra predicción habrá sido mejor
cor(predlm,regtecnica.valid$potenciaresp)

#este número sirve para comparar
sse=sum((predlm-regtecnica.valid$potenciaresp)^2)

#también para comparar modelos, se basa en logaritmos
AIC(modelo)

#Este gráfico debe tener un comportamiento aleatorio. En este caso se ve una tendencia lineal así que probablemente nuestro modelo no esté absorviendo algunas relaciones lineales.
plot(regtecnica.train$distancianodo,modelo$residuals) # ver que relacion hay




## segundo modelo

modelo2=lm(potenciaresp~distancianodo+potencianodo+picovalle,data=regtecnica.train)
summary(modelo2)

## indicadores

modelo$coefficients
predlm2=predict(modelo2,regtecnica.valid)
cor(predlm2,regtecnica.valid$potenciaresp)
sse2=sum((predlm2-regtecnica.valid$potenciaresp)^2)
AIC(modelo2)

## comentarios 

# el inverso de potencia del nodo hace que las demas se vuelvan significativa ( crear una variable
#inverso y colocar en la variable)

##########################################################

## Practicar

ingreso=read_excel("polingreso.xlsx")
nuevo=read_excel("nuevoproducto.xlsx")

##########################################################

#### -- 3) Modelo Penalizada Ridge y Lasso

## Cargar la data

carros=read_excel("carros2011imputado2.xlsx")

## modelo Regresion Ridge

## Particion Muestral

set.seed(1234) 
sample <- sample.int(nrow(carros), round(.8*nrow(carros)))
carros.train<- carros[sample, ]
carros.validation <- carros[-sample, ]

precio.train=as.matrix(carros.train$precio_promedio)
predictores.train=as.matrix(carros.train[,2:17])

precio.validation=as.matrix(carros.validation$precio_promedio)
predictores.validation=as.matrix(carros.validation[,2:17])

## Modelado
#alpha=0 => ridge, alpha=1 => lasso,  alpha=0.5 => elastic net
fitridge=glmnet(predictores.train,precio.train,alpha=0)
fitridge$beta
plot(fitridge) # las q se alejan mas son las mas importantes

## Encontrar los mejores coeff
## crossvalidation
foundrigde=cv.glmnet(predictores.train,precio.train,alpha=0,nfolds=5)
plot(foundrigde) # con landa de log de 0 a 2 se estabiliza
#se estabiliza entre 0 y 2.xx, aplicarías la exponencial para conocer el lambda
attributes(foundrigde)
foundrigde$lambda
foundrigde$lambda.1se # muestra el landa optimo sugerencia
foundrigde$lambda.min #garantiza el menor error minimo cuadrado, casi siempre este da el mejor y estimado, este tiene más sentido del negocio. Es más interpretable.

#Coeficientes finales, luego de aplicar la penalización
coef(fitridge,s=foundrigde$lambda.1se)
coef(fitridge,s=foundrigde$lambda.min)

## Indicadores

prediridge=predict(foundrigde,predictores.validation,s="lambda.min")
ridgemse=sqrt(mean((prediridge-precio.validation)^2))
ridgemse

## modelo Regresion Lasso

fitlasso=glmnet(predictores.train,precio.train,alpha=1)## aplha 1 es cambiar la norma con la 1

## Encontrar los mejores coeff

founlasso=cv.glmnet(predictores.train,precio.train,alpha=1,nfolds=5) 
plot(founlasso)
#los óptimos son los de -5 a 1
founlasso$lambda.1se # muestra el landa optimo sugerencia
founlasso$lambda.min 

coef(fitlasso,s=founlasso$lambda.1se) #mucho más castigador, ya que solo le importa que se obtenga la mejor precisión
coef(fitlasso,s=founlasso$lambda.min) #más orientado al negocio.

#PARSIMONIA: Encontrar el mejor modelo que explique menor con la menor cantidad de parámetros (cada parámetro está asociado a una variable)

#ESTOS MÉTODOS SON MÁS MATEMÁTICOS, POR ESO NO ESTÁN ORIENTADOS A CALCULAR LA SIGNIFICANCIA YA QUE ESO ES ESTADÍSTICA.

## Indicadores

predilasso=predict(founlasso,predictores.validation,s="lambda.min")
lassomse=sqrt(mean((predilasso-precio.validation)^2))
lassomse

## modelo Regresion mediante redes elasticas

fitnet=glmnet(predictores.train,precio.train,alpha=0.5)## aplha 0.5 es cambiar la norma con la 0.5

## Encontrar los mejores coeff

founnet=cv.glmnet(predictores.train,precio.train,alpha=0.5,nfolds=5) 
plot(founnet)
founnet$lambda.1se # muestra el landa optimo sugerencia
founnet$lambda.min 

coef(fitnet,s=founnet$lambda.1se)
coef(fitnet,s=founnet$lambda.min)

## Indicadores

predinet=predict(founnet,predictores.validation,s="lambda.min")
netmse=sqrt(mean((predinet-precio.validation)^2))
netmse

#comparacion de los 3, tiene que ser menor.
cbind(ridgemse,lassomse,netmse)

