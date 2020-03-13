
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- ######
##########################################################################
######## Tema : Adaboost - GBM ######################################################
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
library(C50)
library(gbm)


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

#### -- 2) Arbol c5.0 (AdaBoost)

modelo2 <- C5.0(Loan_Status~.,data = train.data,trials = 55,rules= TRUE,tree=FALSE,winnow=FALSE)

##probabilidades
proba2=predict(modelo2, newdata=test.data,type="prob")[,2]

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED2 <-predict(modelo2, newdata=test.data,type="class")

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED2,test.data$Loan_Status,positive = "1")

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

#### -- 3) GBM

data.train.2=train.data
data.train.2[,2:ncol(data.train.2)]=lapply(data.train.2[,2:ncol(data.train.2)],as.numeric)
data.train.2$Loan_Status= as.character(data.train.2$Loan_Status)
data.train.2$Loan_Status= as.numeric(data.train.2$Loan_Status)

data.test.2=test.data
data.test.2[,2:ncol(data.test.2)]=lapply(data.test.2[,2:ncol(data.test.2)],as.numeric)

modelo3 = gbm(formula = Loan_Status ~ Credit_History+LoanAmount+Total_income+Amauntxterm,
              distribution = "bernoulli",
              data = data.train.2,
              n.trees = 1000,
              shrinkage = 0.1,
              interaction.depth=5,
              n.minobsinnode = 50)



proba3 = predict(object = modelo3,
                 newdata = data.test.2[,-5],
                 n.trees=1000,
                 type = "response")


# curva ROC
AUC3 <- roc(test.data$Loan_Status, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 2*(AUC3$auc) -1

# Calcular los valores predichos
PRED3 <-ifelse(proba3<=0.68,0,1)
PRED3=as.factor(PRED3)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED3,test.data$Loan_Status,positive = "1")

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
rownames(resultado)=c('Logistico','Adaboost','GBM')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado

