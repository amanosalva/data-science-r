
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- #######
##########################################################################
######## Tema : Logistico  - KNN #######################################
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


#### -- 1) Modelo de Regresion Logistica

## Cargar la data

Train=read.csv("data_loan_status_limpia.csv")

## factor de loan_status

Train$Loan_Status=as.factor(Train$Loan_Status)

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


# modelo 2.- KNN

# Utilizar libreria para ML, tratamiento de la data
library(mlr)
n=ncol(train.data)
data.train.2=train.data
data.train.2[,2:n]=lapply(data.train.2[,2:n],scale)

data.test.2=test.data
data.test.2[,2:n]=lapply(data.test.2[,2:n],scale)

#create a task
trainTask <- makeClassifTask(data = data.train.2,target = "Loan_Status", positive = "1")
testTask <- makeClassifTask(data = data.test.2, target = "Loan_Status")

# Modelado KNN

set.seed(1224)
knn <- makeLearner("classif.knn",prob = TRUE,k = 10)

qmodel <- train(knn, trainTask)
qpredict <- predict(qmodel, testTask)

response=as.numeric(qpredict$data$response[1:nrow(data.test.2)])
response=ifelse(response==2,1,0)
proba2=response

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-response

PRED=as.factor(PRED)

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


## --Tabla De Resultados ####

AUC=rbind(auc_modelo1,
          auc_modelo2
)
GINI=rbind(gini1,
           gini2
)
Accuracy=rbind(Accuracy1,
               Accuracy2
)

ERROR= rbind(error1,
             error2
)
SENSIBILIDAD=rbind(Sensitivity1,
                   Sensitivity2
)

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD)
rownames(resultado)=c('Logistico','KNN')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado

