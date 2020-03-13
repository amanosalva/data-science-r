## Anthony Steve Brian Manosalva López
## Segunda práctica.


rm(list=ls())

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
library(corrplot)


#### -- 2) Modelo de Regresion Logistica

## Cargar la data (limpia)

Train=read_excel("carros2011imputado2.xlsx")
Train=as.data.frame(Train)
str(Train)

#Target es: Transmision_manual

Train$fabricante <- as.factor(Train$fabricante)
Train$fabricante <- as.numeric(Train$fabricante)

Train$modelo <- as.factor(Train$modelo)
Train$modelo <- as.numeric(Train$modelo)

Train$tipo <- as.factor(Train$tipo)
Train$tipo <- as.numeric(Train$tipo)

Train$numero_de_airbags <- as.factor(Train$numero_de_airbags)
Train$numero_de_airbags <- as.numeric(Train$numero_de_airbags)

Train$tracción <- as.factor(Train$tracción)
Train$tracción <- as.numeric(Train$tracción)

Train$hecho_o_no_en_USA <- as.factor(Train$hecho_o_no_en_USA)
Train$hecho_o_no_en_USA <- as.numeric(Train$hecho_o_no_en_USA)

str(Train)

## Manipulando la variable TARGET

Train$transmisión_manual <- ifelse(Train$transmisión_manual == "Si", 1, 0)
Train$transmisión_manual = as.factor(Train$transmisión_manual)

head(Train)

## Observar aleatoriamente 3 valores de la data
str(Train$transmisión_manual)
sample_n(Train, 6)

## supuestos
#por defecto pearson, en caso de querer spearman, colocar el parámetro.
cor(Train[,c(2,3,4,5,6,7,8,9,)])
corrplot(cor(Train[,2:8]), method = "number")

## Particion Muestral

set.seed(123)


training.samples <- Train$transmisión_manual %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]

# balanceado?
table(train.data$transmisión_manual)
#Proporción
210/(210 + 104)


## Modelado

modelo_logistica=glm(transmisión_manual~.,data=train.data,family="binomial" )
summary(modelo_logistica)


## indicadores

proba1=predict(modelo_logistica, newdata=test.data,type="response")


AUC1 <- roc(test.data$transmisión_manual, proba1)
## calcular el AUC
auc_modelo1=AUC1$auc
auc_modelo1
## calcular el GINI
gini1 <- 2*(AUC1$auc) -1
gini1
# Calcular los valores predichos
PRED <-predict(modelo_logistica,test.data,type="response")
#PRED=ifelse(PRED<=0.5,0,1  )  #nooo
PRED=ifelse(PRED<=0.6687898,0,1)
PRED=as.factor(PRED)
# Calcular la matriz de confusión
tabla=confusionMatrix(PRED,test.data$transmisión_manual,positive = "1")
tabla
# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])
# Precisión
Accuracy1=tabla$overall[1]
# Calcular el error de mala clasificación
error1=mean(PRED!=test.data$transmisión_manual)

Sensitivity1
Accuracy1
error1

cbind(auc_modelo1, gini1, Sensitivity1, Accuracy1, error1)


# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1


cbind(auc_modelo1, gini1, Sensitivity1, Accuracy1, error1)
