
rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- ######
##########################################################################
######## Tema : Random Forest ############################################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

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
library(randomForest)

#### -- 2) Modelo de Arboles de Decision

## Cargar la data

Train=read.csv("data_loan_status_limpia.csv")

## Observar aleatoriamente 3 valores de la data

sample_n(Train, 3)

## Particion Muestral

set.seed(123)
training.samples <- Train$Loan_Status %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Train[training.samples, ]
test.data <- Train[-training.samples, ]

test.data$Loan_Status=as.factor(test.data$Loan_Status)

## Modelado


# modelo 1.- Arbol CHAID

modelo1<-ctree(Loan_Status~.,data = train.data, 
               controls=ctree_control(mincriterion=0.95))

plot(modelo1)

##probabilidades
proba1=sapply(predict(modelo1, newdata=test.data,type="prob"),'[[',1)

# curva ROC	
AUC1 <- roc(test.data$Loan_Status, proba1) 
auc_modelo1=AUC1$auc

# Gini
gini1 <- 2*(AUC1$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba1 <= 0.5 ,0,1)

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

# modelo 2.- RF

set.seed(1234)
modelo2 <- randomForest( Loan_Status~.,data = train.data,   # Datos a entrenar 
                          ntree=500,           # N?mero de ?rboles
                          mtry = 4,            # Cantidad de variables
                          importance = TRUE,   # Determina la importancia de las variables
                          replace=T) 

##probabilidades
proba2<-predict(modelo2, newdata=test.data,type="class")
proba2=as.numeric(proba2)

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo2,test.data,type="class")
PRED=ifelse(PRED<=0.5,0,1)
PRED = as.factor(PRED)
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
rownames(resultado)=c('CHAID',
                      'RF')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado


