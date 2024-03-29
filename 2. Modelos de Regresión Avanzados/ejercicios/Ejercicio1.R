#Anthony Manosalva L�pez
#Primera pr�ctica.

library("readxl")
library("ggplot2")
library(glmnet)  #para regresiones penalizadas, necesitamos que los datos est�n forma de matriz
library(gee)
library(readxl)
library(leaps)
rm(list=ls())

set.seed(1234)
polingreso=read_excel("polingreso.xlsx")
View(polingreso)


#Ingresos mensuales en base a la pulicidad masiva, n�mero de competidores, y en base al precio de los productos

names(polingreso)
#Ingreso es el target.

#menor SSE (suma de cuadrado de los errores)
sample <- sample.int(nrow(polingreso), round(.8*nrow(polingreso)))
polingreso.train<- polingreso[sample, ]
polingreso.valid <- polingreso[-sample, ]

cor(polingreso.train)
cor(polingreso.valid)
#AL OJO: Publicidad masiva, son las m�s correlacionadas

modelo=lm(ingreso~.,data=polingreso.train)
summary(modelo)

#Indicadores
modelo$coefficients
predlm=predict(modelo,polingreso.valid)
#Correlaci�n entre lo predecido y el original mientras m�s cercano a 1, nuestra predicci�n habr� sido mejor
cor(predlm,polingreso.valid$ingreso)

############################## Suma de errores
sse=sum((predlm-polingreso.valid$ingreso)^2)
sse
####################################MSE regresi�n linea
regressionmse=sqrt(mean((predlm-polingreso.valid$ingreso)^2))
regressionmse
#774176.1
#(se usar� para comparar modelos)

################
#Modelo Ridge y Lasso
################
ingreso.train = as.matrix(polingreso.train$ingreso)
predictores.train=as.matrix(polingreso.train[2:4])

ingreso.test = as.matrix(polingreso.valid$ingreso)
predictores.test=as.matrix(polingreso.valid[2:4])

## Modelado
#alpha=0 => ridge, alpha=1 => lasso,  alpha=0.5 => elastic net
fitridge=glmnet(predictores.train,ingreso.train,alpha=0)
fitridge$beta
plot(fitridge) # las q se alejan mas son las mas importantes

## Encontrar los mejores coeff
## crossvalidation
foundrigde=cv.glmnet(predictores.train,ingreso.train,alpha=0,nfolds=5)
plot(foundrigde) # con landa de log de 0 a 2 se estabiliza
attributes(foundrigde)
foundrigde$lambda
foundrigde$lambda.1se # muestra el landa optimo sugerencia
foundrigde$lambda.min #garantiza el menor error minimo cuadrado, casi siempre este da el mejor y estimado, este tiene m�s sentido del negocio. Es m�s interpretable.

#Coeficientes finales, luego de aplicar la penalizaci�n
coef(fitridge,s=foundrigde$lambda.1se)
coef(fitridge,s=foundrigde$lambda.min)

## Indicadores

prediridge=predict(foundrigde,predictores.test,s="lambda.min")
ridgemse=sqrt(mean((prediridge-ingreso.test)^2))
ridgemse
#42775.93



#### Regresi�n LASSO
fitlasso=glmnet(predictores.train,ingreso.train,alpha=1)## aplha 1 es cambiar la norma con la 1

## Encontrar los mejores coeff

founlasso=cv.glmnet(predictores.train,ingreso.train,alpha=1,nfolds=5) 
plot(founlasso)
#los �ptimos son los de -5 a 1
founlasso$lambda.1se # muestra el landa optimo sugerencia
founlasso$lambda.min 

coef(fitlasso,s=founlasso$lambda.1se) #mucho m�s castigador, ya que solo le importa que se obtenga la mejor precisi�n
coef(fitlasso,s=founlasso$lambda.min) #m�s orientado al negocio.


predilasso=predict(founlasso,predictores.test,s="lambda.min")
lassomse=sqrt(mean((predilasso-ingreso.test)^2))
lassomse
#41841.32


####################################MSE final
regressionmse
ridgemse
lassomse
