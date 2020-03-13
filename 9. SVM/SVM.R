
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- ######
##########################################################################
######## Tema : SVM ######################################################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

library("ggplot2")
library(glmnet)
library(gee)
library(readxl)
library(leaps)
library(dplyr)
library(caret)
library(pROC)
library(e1071)


#### -- 1) Modelo de Regresion Logistica

## Cargar la data

Train=read.csv("data_loan_status_limpia.csv")

## factor de loan_status

Train$Loan_Status=as.factor(Train$Loan_Status)

## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## Particion Muestral

set.seed(123)
training.samples <- Train$Loan_Status %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]

## Modelado

modelo_logistica=glm(Loan_Status~.,data=train.data,family="binomial" )
summary(modelo_logistica)


## indicadores

proba1=predict(modelo_logistica, newdata=test.data,type="response")
AUC1 <- roc(test.data$Loan_Status, proba1)
## calcular el AUC
auc_modelo1=AUC1$auc
## calcular el GINI
gini1 <- 2*(AUC1$auc) -1
# Calcular los valores predichos
PRED <-predict(modelo_logistica,test.data,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)
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

# modelo 2.- SVM

# modelo 2.- SVM Radial

modelo2=svm(Loan_Status~.,data = train.data,kernel="radial",costo=100,gamma=1,probability = TRUE, method="C-classification")

##probabilidades
proba2<-predict(modelo2, newdata=test.data,decision.values = TRUE, probability = TRUE) 
proba2=attributes(proba2)$probabilities[,2]

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo2,test.data,type="class")

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

# modelo 2.- SVM Linear

modelo3=svm(Loan_Status~.,data = train.data,kernel="linear",costo=100,probability = TRUE, method="C-classification")

##probabilidades
proba3<-predict(modelo3, newdata=test.data,decision.values = TRUE, probability = TRUE) 
proba3=attributes(proba3)$probabilities[,2]

# curva ROC
AUC3 <- roc(test.data$Loan_Status, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 2*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo3,test.data,type="class")

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

# modelo 2.- SVM sigmoid

modelo4=svm(Loan_Status~.,data = train.data,kernel="sigmoid",costo=100,probability = TRUE, method="C-classification")

##probabilidades
proba4<-predict(modelo4, newdata=test.data,decision.values = TRUE, probability = TRUE) 
proba4=attributes(proba4)$probabilities[,2]

# curva ROC
AUC4 <- roc(test.data$Loan_Status, proba4) 
auc_modelo4=AUC4$auc

# Gini
gini4 <- 2*(AUC4$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo4,test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,test.data$Loan_Status,positive = "1")

# sensibilidad
Sensitivity4=as.numeric(tabla$byClass[1])

# Precision
Accuracy4=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error4=mean(PRED!=test.data$Loan_Status)

# indicadores
auc_modelo4
gini4
Accuracy4
error4
Sensitivity4


## --Tabla De Resultados ####

AUC=rbind(auc_modelo1,
          auc_modelo2,
          auc_modelo3,
          auc_modelo4
)
GINI=rbind(gini1,
           gini2,
           gini3,
           gini4
)
Accuracy=rbind(Accuracy1,
               Accuracy2,
               Accuracy3,
               Accuracy4
)

ERROR= rbind(error1,
             error2,
             error3,
             error4
)
SENSIBILIDAD=rbind(Sensitivity1,
                   Sensitivity2,
                   Sensitivity3,
                   Sensitivity4
)

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD)
rownames(resultado)=c('Logistico','SVM_Radial','SVM_lineal','SVM_sigmoidal')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado

