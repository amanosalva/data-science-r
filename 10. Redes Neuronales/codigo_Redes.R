rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- ######
##########################################################################
######## Tema : Redes Neuronales- ######################################################
######## Autores: Jose Cardenas - Andre Chavez  ########################## 
##########################################################################

#### -- 1) Librerias a usar ####

library(ggplot2)
library(glmnet)
library(dplyr)
library(caret)
library(pROC)
library(e1071)
library(neuralnet)
library(MLmetrics)


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

#### -- 2) REDES NEURONALES

x=train.data %>% select(-Loan_Status) 
x[,1:ncol(x)]=lapply(x[,1:ncol(x)],as.numeric)
y=train.data %>% select(Loan_Status)
y=y$Loan_Status
y=as.numeric(as.character(y))
x = scale(x)

x1=test.data %>% select(-Loan_Status)
x1[,1:ncol(x1)]=lapply(x1[,1:ncol(x1)],as.numeric)
y1=test.data %>% select(Loan_Status)
y1=y1$Loan_Status
y1=as.numeric(as.character(y1))
x1 = scale(x1)

# Data Final

dtrain = data.frame(cbind(x,y))
dtest = data.frame(cbind(x1,y1))


## Modelado

formula = y~Credit_History+Total_income+Edu_Ma+log_LoanAmount
modelo_red=neuralnet(formula,data=dtrain, hidden=c(2,1), linear.output=FALSE, threshold=0.01 )
plot(modelo_red)

## indicadores
dtest <- dtest %>% select (Credit_History,Total_income,Edu_Ma,log_LoanAmount)
test_proba = compute(modelo_red, dtest)
proba2 = data.frame(test_proba$net.result)
## calcular el AUC
auc_modelo2=AUC(proba2,test.data$Loan_Status)
## calcular el GINI
gini2 <- 2*(auc_modelo2) -1
# Calcular los valores predichos
PRED=ifelse(proba2<=0.5,0,1)
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
rownames(resultado)=c('Logistico','RN')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado

