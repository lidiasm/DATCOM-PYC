#Cargamos los paquetes
library(ISLR)
library(plyr)
library(moments)
library(nortest)
library(MASS)
library(kknn)
library(class)
library(readxl)
library(tidyverse)
library(mice)
library(VIM)
library(caret)
library(rpart)
library(ggplot2)
library(ggpubr)
library(missForest)
library(rpart.plot)
library(groupdata2)

#Cargamos los datos
datos = read.csv("datos_manual+missForest.csv")
train_labels = read.csv("training_set_labels.csv")
train =datos[1:length(train_labels[,1]),]
train = cbind(train,train_labels[,2:3])

# Convertimos a factor las últimas 15 columnas
train_factor = lapply(train[,22:37],as.factor)
train_factor = as.data.frame(train_factor)

train[,22:37]=train_factor


#Lo mismo para test
test = datos[(length(train_labels[,1])+1):dim(datos)[1],]
test_factor = lapply(test[,22:35],as.factor)
test_factor = as.data.frame(test_factor)
test[,22:35]=test_factor

####################
# Preprocesamiento 
#######################


# Eliminar las variables correlacionadas creando un 
# dataset sin variables relacionadas con h1n1 y otro sin variables relacionadas con seasonal
# y eliminando una columna que se asocie al trabajo


#names(train)[c(11,19:21,33)]

#train_h1n1 = train[,-c(11,19:21,33)]

#train_seas = train[,-c(10,16:18,33)]

#Upsampling
#train_h1n1_up = upsample(train_h1n1,"h1n1_vaccine")

#Downsampling
#train_h1n1_down = downsample(train_h1n1,"h1n1_vaccine")



###############
#rpart basico
################


#para h1n1


model_h1n1 <- train(h1n1_vaccine~., data = train[,-37], method = "rpart",
                    trControl = trainControl("cv", number = 10),
                    tuneLength = 20)
h1n1_predict = model_h1n1 %>% predict(test, "prob")
h1n1_predict = h1n1_predict[,2] 

#para seasonal

model_seasonal <- train(seasonal_vaccine~., data = train_sea[,c(selec_cols,37)], method = "rpart", 
                        trControl = trainControl("cv", number = 10), 
                        tuneLength = 20)
seasonal_predict = model_seasonal %>% predict(test, "prob")
seasonal_predict = seasonal_predict[,2]



#Escribimos el fichero
test_id =read.csv("test_set_features.csv")
test_id = test_id[,1]

rpart_basico = data.frame(test_id,h1n1_predict, seasonal_predict)
names(rpart_basico) = c("respondent_id","h1n1_vaccine","seasonal_vaccine")

write.csv(rpart_basico,"rpart_basico.csv",row.names=FALSE)

