
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel I -- #######
##########################################################################
######## Tema : Arboles de Decision #######################################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

library("readxl")
library("ggplot2")
library(gee)
library(readxl)
library(leaps)
library(dplyr)
library(caret)
library(pROC)
library(party)
library(C50)
library(rpart)

#### -- 2) Modelo de Arboles de Decision

## Cargar la data

Train=read.csv("data_loan_status_limpia.csv")

## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## supuestos

cor(Train[,1:8])

## Particion Muestral

set.seed(123)
training.samples <- Train$Loan_Status %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]


## Modelos de Arboles 


# modelo 1.- Arbol CHAID

modelo1<-ctree(Loan_Status~.,data = train.data, 
               controls=ctree_control(mincriterion=0.95))

##probabilidades
proba1=sapply(predict(modelo1, newdata=test.data,type="prob"),'[[',1)

# curva ROC	
AUC1 <- roc(test.data$Loan_Status, proba1) 
auc_modelo1=AUC1$auc

# Gini
gini1 <- 2*(AUC1$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba1 <= 0.5 ,0,1)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])

# Precision
Accuracy1=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error1=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1

# modelo 2.- Arbol CART 

arbol.completo <- rpart(Loan_Status~.,data = train.data,method="class",cp=0, minbucket=0)
xerr <- arbol.completo$cptable[,"xerror"] ## error de la validacion cruzada
minxerr <- which.min(xerr)
mincp <- arbol.completo$cptable[minxerr, "CP"]

modelo2 <- prune(arbol.completo,cp=mincp)

##probabilidades
proba2=predict(modelo2, newdata=test.data,type="prob")[,2]

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo2, newdata=test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity2=as.numeric(tabla$byClass[1])

# Precision
Accuracy2=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error2=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo2
gini2
Accuracy2
error2
Sensitivity2

# modelo 3.- Arbol c5.0

train.data$Loan_Status= as.factor(train.data$Loan_Status)
modelo3 <- C5.0(Loan_Status~.,data = train.data,trials = 55,rules= TRUE,tree=FALSE,winnow=FALSE)

##probabilidades
proba3=predict(modelo3, newdata=test.data,type="prob")[,2]

# curva ROC
AUC3 <- roc(test.data$Loan_Status, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 2*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo3, newdata=test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity3=as.numeric(tabla$byClass[1])

# Precision
Accuracy3=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error3=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo3
gini3
Accuracy3
error3
Sensitivity3



## --Tabla De Resultados ####

AUC=rbind(auc_modelo1,
          auc_modelo2,
          auc_modelo3
)
GINI=rbind(gini1,
           gini2,
           gini3
)
Accuracy=rbind(Accuracy1,
               Accuracy2,
               Accuracy3
)

ERROR= rbind(error1,
             error2,
             error3
)
SENSIBILIDAD=rbind(Sensitivity1,
                   Sensitivity2,
                   Sensitivity3
)

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD)
rownames(resultado)=c('CHAID',
                      'CART',
                      'C50'
)
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado


