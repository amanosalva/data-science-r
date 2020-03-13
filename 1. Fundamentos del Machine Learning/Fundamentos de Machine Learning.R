rm(list=ls())
#########################################################################
##### -- Programa de Especializaci贸n en Data Science - Nivel I -- #######
#########################################################################
######## Tema : Fundamentos de Machine Learning #########################
######## Autores: Jose Cardenas - Andre Chavez  ######################### 
#########################################################################

########## 1) Librer铆as a Utilizar ################# 

library(sqldf)
library(ggvis)
library(party)
library(Boruta)
library(pROC)
library(randomForest)
library(e1071)
library(caret)
library(glmnet)
library(mboost)
library(adabag)
library(xgboost)
library(ROCR)
library(C50)
library(mlr)
library(lattice)
library(gmodels)
library(gplots)
library(DMwR)
library(rminer)
library(polycor)
library(class)
library(neuralnet)

######### 2) Extraer la Data ################# 

##train<-read.csv("train.csv",na.strings = c(""," ",NA)) # leer la data de entrenamiento
train <- read.csv(file.choose(),na.strings = c(""," ",NA))
trainbk <- train
test<-read.csv(file.choose(),na.strings = c(""," ",NA))  # leer la data de Validacion 

names(train) # visualizar los nombres de la data
head(train)  # visualizar los 6 primeros registros
str(train)   # ver la estructura de la data

######### 3) Exploraci贸n de la Data ########### 

# tablas resumen
summary(train) # tabla comun de obtener
summarizeColumns(train) # tabla mas completa

resumen=data.frame(summarizeColumns(train))

## Graficos para variables cuantitativas

# histogramas y Cajas

#Veamos la variable ApplicantIncome
hist(train$ApplicantIncome, breaks = 100, main = "Applicant Income Chart",xlab = "ApplicantIncome",col="blue")

#Veamos la variable CoapplicantIncome
hist(train$CoapplicantIncome, breaks = 100, main = "Coapplicant Income Chart",xlab = "CoapplicantIncome",col="red")

#Veamos los Outliers
#Se visualizan valores atipicos

#Veamos la variable ###LoanAmount
bwplot(train$LoanAmount, layout = c(1, 1),main = "Loan Amount Chart",xlab = "LoanAmount", col="blue")

#Veamos la variable Loan_Amount_Term
bwplot(train$Loan_Amount_Term, layout = c(1, 1),main = "Loan Amount Term Chart",xlab = "Loan_Amount_Term", col="blue")

## Graficos para variables cuantitativas

#Veamos la variable Gender
CrossTable(train$Gender,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)

#Veamos la variable Married
CrossTable(train$Married,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)

#Veamos la variable Loan_Status
CrossTable(train$Loan_Status,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)

# Graficos de cualitativos
Tabla1=table(train$Gender)
Tabla2=table(train$Married)

par(mfrow=c(1,2))
balloonplot(t(Tabla1), main ="Tabla de Contingencia Gender",xlab ="Gender", label = FALSE, show.margins = FALSE)
balloonplot(t(Tabla2), main ="Tabla de Contingencia Married",xlab ="Married", label = FALSE, show.margins = FALSE)

# comentarios de la data

# 1. LoanAmount tiene (614 - 592) 22 valores perdidos.
# 2. Loan_Amount_Term tiene (614 - 600) 14 valores perdidos.
# 3. Credit_History tiene (614 - 564) 50 valores perdidos.
# 4. Nosotros podemos tambi?n observar que cerca del 84% de los solicitantes al pr?stamo 
# tienen un historial crediticio. ?C?mo? La media del campo Credit_History es 0.84 
# (Recordemos, Credit_History tiene o toma el valor 1 para aquellos que tienen 
#   historial crediticio y 0 en caso contrario).
# 5. La variable ApplicantIncome parece estar en l?nea con las espectativas al 
# igual que CoapplicantIncome.

######### 4) Imputaci贸n de la Data ################# 

# revisar valores perdidos

perdidos=data.frame(resumen$name,resumen$na,resumen$type)
colnames(perdidos)=c("Caracter韘tica","Valores faltantes","Tipo de dato")
perdidos

# recodificando Dependents
train$Dependents=ifelse(train$Dependents=="3+",3,
                                 ifelse(train$Dependents=="0",0,
                                        ifelse(train$Dependents=="1",1,
                                               ifelse(train$Dependents=="2",2,
                                                      train$Dependents))))
train$Dependents=as.factor(train$Dependents)

# convirtiendo en factor Credit_History
train$Credit_History <- as.factor(train$Credit_History)

# recodificando Dependents
test$Dependents=ifelse(test$Dependents=="3+",3,
                        ifelse(test$Dependents=="0",0,
                               ifelse(test$Dependents=="1",1,
                                      ifelse(test$Dependents=="2",2,
                                             test$Dependents))))
test$Dependents=as.factor(test$Dependents)

# convirtiendo en factor Credit_History
test$Credit_History <- as.factor(test$Credit_History)

# recodificando Loan_Status
train$Loan_Status=ifelse(train$Loan_Status=="N",0,1)
train$Loan_Status=as.factor(train$Loan_Status)

# partcionando la data en numericos y factores

numericos <- sapply(train, is.numeric) # variables cuantitativas
factores <- sapply(train, is.factor)  # variables cualitativas

train_numericos <-  train[ , numericos]
train_factores <- train[ , factores]

# APLICAR LA FUNCION LAPPLY PARA DISTINTAS COLUMNAS CONVERTIR A FORMATO NUMERICO
n1=min(dim(train_factores))
#train_factores[2:(n1-1)] <- lapply(train_factores[2:(n1-1)], as.numeric)
#train_factores[2:(n1-1)] <- lapply(train_factores[2:(n1-1)], as.factor)
train_factores[2:(n1)] <- lapply(train_factores[2:(n1)], as.numeric)
train_factores[2:(n1)] <- lapply(train_factores[2:(n1)], as.factor)

numericos <- sapply(test, is.numeric) # variables cuantitativas
factores <- sapply(test, is.factor)  # variables cualitativas

test_numericos <-  test[ , numericos]
test_factores <- test[ , factores]

# APLICAR LA FUNCION LAPPLY PARA DISTINTAS COLUMNAS CONVERTIR A FORMATO NUMERICO
n1=min(dim(test_factores))
test_factores[2:(n1)] <- lapply(test_factores[2:(n1)], as.numeric)
test_factores[2:(n1)] <- lapply(test_factores[2:(n1)], as.factor)

# Para train y test

train=cbind(train_numericos,train_factores[,-1])
test=cbind(test_numericos,test_factores[,-1])

## Imputacion Parametrica

#Podemos imputar los valores perdidos por la media o la moda

# data train
train_parametrica <- impute(train, classes = list(factor = imputeMode(), 
                                    integer = imputeMode(),
                                    numeric = imputeMean()),
              dummy.classes = c("integer","factor"), dummy.type = "numeric")
train_parametrica=train_parametrica$data[,1:min(dim(train))]

# data test
test_parametrica  <- impute(test, classes = list(factor = imputeMode(), 
                                    integer = imputeMode(),
                                    numeric = imputeMean()), 
               dummy.classes = c("integer","factor"), dummy.type = "numeric")
test_parametrica=test_parametrica$data[,1:min(dim(test))]

summary(train_parametrica)



######### 5) Creaci贸n y Transformaci贸n de Variables ################# 

train_parametrica$Total_income=train_parametrica$ApplicantIncome+train_parametrica$CoapplicantIncome
train_parametrica$log_LoanAmount=log(train_parametrica$LoanAmount)
train_parametrica$Amauntxterm=train_parametrica$Total_income/train_parametrica$LoanAmount

######### 6) Balanceo de Datos y Creaci贸n de Drivers #################

## Particonando la Data

set.seed(1234)
sample <- createDataPartition(train_parametrica$Loan_Status, p = .70,list = FALSE,times = 1)

data.train <- train_parametrica[ sample,]
data.prueba <- train_parametrica[-sample,]

# Balanceo de los datos
# Balanceo mediante UnderSampling

menorcero<-subset(data.train,Loan_Status=="0") 
mayoruno<-subset(data.train,Loan_Status=="1")

set.seed(1234)
sample<-sample.int(nrow(mayoruno),nrow(menorcero))
length(sample)
mayoruno.prueba<-mayoruno[sample,]

data.train=rbind(mayoruno.prueba,menorcero)
table(data.train$Loan_Status)

rm(mayoruno,menorcero,mayoruno.prueba)

## Seleccion de variables
## Mediante T茅cnicas de Machine Learning
## Utilizando Boruta

pdf("seleccion de variables.pdf")
Boruta(Loan_Status~.,data=data.train,doTrace=2)->Bor.hvo;
plot(Bor.hvo,las=3);
Bor.hvo$finalDecision



######### 7) Modelado de la Data #################

# data de entrenamiento
data.train.1=subset(data.train,select=c("Credit_History","LoanAmount","Total_income","Amauntxterm" ,"Loan_Status"))

# data de validacion
data.test.1=subset(data.prueba,select=c("Credit_History","LoanAmount","Total_income","Amauntxterm" ,"Loan_Status"))

m=min(dim(data.train.1))


# modelo 1.- Logistico

modelo1=glm(Loan_Status~.,data=data.train.1,family = binomial(link = "logit"))
summary(modelo1)

proba1=predict(modelo1, newdata=data.test.1,type="response")

AUC1 <- roc(data.test.1$Loan_Status, proba1)

## calcular el AUC
auc_modelo1=AUC1$auc

## calcular el GINI
gini1 <- 2*(AUC1$auc) -1

# Calcular los valores predichos
PRED <-predict(modelo1,data.test.1,type="response")
PRED=ifelse(PRED<=0.5,0,1)
PRED=as.factor(PRED)

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,data.test.1$Loan_Status,positive = "1")

# sensibilidad
Sensitivity1=as.numeric(tabla$byClass[1])

# Precision
Accuracy1=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error1=mean(PRED!=data.test.1$Loan_Status)

# indicadores
auc_modelo1
gini1
Accuracy1
error1
Sensitivity1

# modelo 2.- KNN

# Utilizar libreria para ML, tratamiento de la data

Credit_History=ifelse(data.train.1$Credit_History=="1",1,2)
data.train.2=data.frame(Credit_History,scale(data.train.1[2:4]),data.train.1$Loan_Status)
colnames(data.train.2)=names(data.train.1)

Credit_History=ifelse(data.test.1$Credit_History=="1",1,2)
data.test.2=data.frame(Credit_History,scale(data.test.1[2:4]),data.test.1$Loan_Status)
colnames(data.test.2)=names(data.test.1)

#create a task
trainTask <- makeClassifTask(data = data.train.2,target = "Loan_Status")
testTask <- makeClassifTask(data = data.test.2, target = "Loan_Status")

trainTask <- makeClassifTask(data = data.train.2,target = "Loan_Status", positive = "1")

# Modelado KNN

set.seed(1234)
knn <- makeLearner("classif.knn",prob = TRUE,k = 10)

qmodel <- train(knn, trainTask)
qpredict <- predict(qmodel, testTask)

response=as.numeric(qpredict$data$response[1:183])
response=ifelse(response==2,1,0)
proba2=response

# curva ROC
AUC2 <- roc(data.test.1$Loan_Status, proba2) 
auc_modelo2=AUC2$auc

# Gini
gini2 <- 2*(AUC2$auc) -1

# Calcular los valores predichos
PRED <-response

# Calcular la matriz de confusi?n
tabla=confusionMatrix(PRED,data.test.1$Loan_Status,positive = "1")

# sensibilidad
Sensitivity2=as.numeric(tabla$byClass[1])

# Precision
Accuracy2=tabla$overall[1]

# Calcular el error de mala clasificaci?n
error2=mean(PRED!=data.test.1$Loan_Status)

# indicadores
auc_modelo2
gini2
Accuracy2
error2
Sensitivity2



## --Tabla De Resultados ####

AUC=rbind(auc_modelo1,
          auc_modelo2)

GINI=rbind(gini1,
           gini2)

Accuracy=rbind(Accuracy1,
            Accuracy2)

ERROR= rbind(error1,
             error2)

SENSIBILIDAD=rbind(Sensitivity1,
                   Sensitivity2)

resultado=data.frame(AUC,GINI,Accuracy,ERROR,SENSIBILIDAD)
rownames(resultado)=c('Logistico',
                      'KNN')

resultado=round(resultado,2)
resultado

## Resultado Ordenado #####

# ordenamos por el Indicador que deseamos, quiza Accuracy en forma decreciente
Resultado_ordenado <- resultado[order(-Accuracy),] 
Resultado_ordenado
