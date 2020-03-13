
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- #######
##########################################################################
######## Tema : Modelos Regresion y Clasificación Avanzados ##############
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

library("readxl")
library("ggplot2")
library(glmnet)
library(gee)
library(readxl)
library(leaps)
library(dplyr)
library(caret)
library(pROC)


#### -- 2) Modelo de Regresion Logistica

## Cargar la data

PimaIndiansDiabetes2=read.csv("PimaIndiansDiabetes.csv")

## Observar aleatoriamente 3 valores de la data

sample_n(PimaIndiansDiabetes2, 3)

## supuestos

cor(PimaIndiansDiabetes2[,1:8])

## tratamiento de la target

PimaIndiansDiabetes2$diabetes=ifelse(PimaIndiansDiabetes2$diabetes=="neg",0,1)
PimaIndiansDiabetes2$diabetes=as.factor(PimaIndiansDiabetes2$diabetes)

## Particion Muestral

set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

## Modelado

modelo_logistica=glm(diabetes~.,data=train.data,family="binomial" )
summary(modelo_logistica)


## indicadores

proba1=predict(modelo_logistica, newdata=test.data,type="response")
AUC1 <- roc(test.data$diabetes, proba1)
## calcular el AUC
auc_modelo1=AUC1$auc
## calcular el GINI
gini1 <- 2*(AUC1$auc) -1
# Calcular los valores predichos
PRED <-predict(modelo_logistica,test.data,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")
# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])
# Precision
Accuracy1=tabla$overall[1]
# Calcular el error de mala clasificaci?n
error1=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1


#### -- 3) Modelo de Regresion Penalizada

## Modelo Lasso

x <- model.matrix(diabetes~., train.data)[,-1]
y <- as.numeric(as.character(train.data$diabetes))

modelo_lasso=glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

## Encontrar los mejores coeff
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

par(mfrow = c(1,1))
plot(cv.lasso)

coef=coef(cv.lasso,s=cv.lasso$lambda.min)
coef

## modelando 

model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

## coefficients
coef(model)

## indicadores

## test data
x.test <- model.matrix(diabetes ~., test.data)[,-1]

proba2=predict(model,x.test,type="response")
AUC2 <- roc(test.data$diabetes, proba2)
## calcular el AUC
auc_modelo2=AUC1$auc
## calcular el GINI
gini2 <- 2*(AUC1$auc) -1
# Calcular los valores predichos
PRED <-predict(model,x.test,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$diabetes,positive = "1")
# sensibilidad
Sensitivity2=as.numeric(tabla$byClass[1])
# Precision
Accuracy2=tabla$overall[1]
# Calcular el error de mala clasificaci?n
error2=mean(PRED!=test.data$diabetes)

# indicadores
auc_modelo2
gini2
Accuracy2
error2
Sensitivity2
