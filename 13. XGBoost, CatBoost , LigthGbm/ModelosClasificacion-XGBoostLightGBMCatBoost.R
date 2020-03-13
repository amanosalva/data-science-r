rm(list=ls())
##########################################################################
##### -- Programa de Especialización en Data Science - Nivel II -- ######
##########################################################################
######## Tema : XGBoost - LightGBM CatBoost- ######################################################
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
library(xgboost)
library(reticulate)


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

#### -- 2) Arbol XGboost

x=train.data %>% select(-Loan_Status) 
x[,1:ncol(x)]=lapply(x[,1:ncol(x)],as.numeric)
y=train.data %>% select(Loan_Status)
y=y$Loan_Status
y=as.numeric(as.character(y))

x1=test.data %>% select(-Loan_Status)
x1[,1:ncol(x1)]=lapply(x1[,1:ncol(x1)],as.numeric)
y1=test.data %>% select(Loan_Status)
y1=y1$Loan_Status
y1=as.numeric(as.character(y1))


dtrain = xgb.DMatrix(data =  as.matrix(x), label = y )
dtest = xgb.DMatrix(data =  as.matrix(x1), label = y1)

# tipo grid
watchlist = list(train=dtrain, test=dtest)

modelo2 = xgb.train(data = dtrain, 
                    max.depth = 8, 
                    eta = 0.3, 
                    nthread = 2, 
                    nround = 1000, 
                    watchlist = watchlist, 
                    objective = "binary:logistic", 
                    early_stopping_rounds = 50,
                    print_every_n = 500)


dtest = xgb.DMatrix(data =  as.matrix(x1))
proba2 = predict(modelo2, dtest)

# curva ROC
AUC2 <- roc(test.data$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <- ifelse(proba2<=0.5,0,1)
PRED <- as.factor(PRED)

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

# modelo 3.- lightgbm

nnn=ncol(train.data)

x=train.data[,2:(nnn)]
y=train.data$Loan_Status

x1=test.data[,2:(nnn)]
y1=test.data$Loan_Status

use_python("/usr/local/bin/python")

repl_python()
import lightgbm as lgb
train_data = lgb.Dataset(r.x, label=r.y)
test_data = lgb.Dataset(r.x1, label=r.y1)
exit


repl_python()
parameters = {
  'application': 'binary',
  'objective': 'binary',
  'metric': 'auc',
  'is_unbalance': 'false',
  'boosting': 'gbdt',
  'num_leaves': 32,
  'feature_fraction': 0.5,
  'bagging_fraction': 0.5,
  'bagging_freq': 50,
  'learning_rate': 0.001,
  'verbose': 0
}

model = lgb.train(parameters,
                  train_data,
                  valid_sets=test_data,
                  num_boost_round=5000,
                  early_stopping_rounds=100)
exit

repl_python()
x = r.x1
y = model.predict(x)
exit

proba3=py$y
head(proba3)


# curva ROC
AUC3 <- roc(y1, proba3) 
auc_modelo3=AUC3$auc

# Gini
gini3 <- 2*(AUC3$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba3<=0.6870,0,1)
PRED<- as.factor(PRED)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,y1,positive = "1")

# sensibilidad
Sensitivity3=as.numeric(tabla$byClass[1])

# Precision
Accuracy3=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error3=mean(PRED!=y1)

# indicadores
auc_modelo3
gini3
Accuracy3
error3
Sensitivity3


# modelo 4.- Catboost

repl_python()
from catboost import CatBoostClassifier

model = CatBoostClassifier(iterations=2, depth=2, learning_rate=1, loss_function='Logloss', logging_level='Verbose')
exit

repl_python()
x = r.x
y = r.y
exit

repl_python()
x1 = r.x1
y1 = r.y1
exit

# solo variables categoricas
repl_python()
model.fit(x, y, cat_features=[4,5,6,7])
exit

repl_python()
preds_class = model.predict(x1)
preds_proba = model.predict_proba(x1)

import pandas as pd

a=pd.DataFrame(preds_proba)
proba=a[a.columns[1]]
print(proba)
exit

proba4=py$proba
head(proba4)


# curva ROC
AUC4 <- roc(y1, proba4) 
auc_modelo4=AUC4$auc

# Gini
gini4 <- 2*(AUC4$auc) -1

# Calcular los valores predichos
PRED <-ifelse(proba4<=0.5,0,1)
PRED <- as.factor(PRED)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,y1,positive = "1")

# sensibilidad
Sensitivity4=as.numeric(tabla$byClass[1])

# Precision
Accuracy4=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error4=mean(PRED!=y1)

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
rownames(resultado)=c('Logistico','XGBoost','lightgbm','Catboost')
resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado

