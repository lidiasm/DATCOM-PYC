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


#Separamos train en función de si tienen la vacuna de h1n1 o no

train_h1n1_vac_T = train[which(train$h1n1_vaccine==1),]
train_h1n1_vac_F = train[which(train$h1n1_vaccine==0),]

#Separamos train en función de si tienen la vacuna de seasonal o no
train_seasonal_vac_T = train[which(train$seasonal_vaccine==1),]
train_seasonal_vac_F = train[which(train$seasonal_vaccine==0),]





##################
#Random Forest
##################

# Para la vacuna h1n1

ti =Sys.time()
mat_val_h1n1 = c()
for(i in 1:8000){
  val_ale_h1n1_T = sample(dim(train_h1n1_vac_T)[1],1000,replace = FALSE)
  val_ale_h1n1_F = sample(dim(train_h1n1_vac_F)[1],1000,replace = FALSE)
  selec_cols = sample(35,18,replace=FALSE)
  train_h1n1 = rbind(train_h1n1_vac_T[val_ale_h1n1_T,], train_h1n1_vac_F[val_ale_h1n1_F,])
  model_h1n1 <- train(h1n1_vaccine~., data = train_h1n1[,c(selec_cols,36)], method = "rpart",
                      trControl = trainControl("cv", number = 10),
                      tuneLength = 10)
  h1n1_predict = model_h1n1 %>% predict(test)
  h1n1_predict = as.numeric(as.character(h1n1_predict))
  mat_val_h1n1 = cbind(mat_val_h1n1,h1n1_predict)
}
tf=Sys.time()
print(tf-ti)



#Para la vacunas seasonal
ti =Sys.time()
mat_val_seasonal= c()
for(i in 1:8000){
  val_ale_sea_T = sample(dim(train_seasonal_vac_T)[1],1000,replace = FALSE)
  val_ale_sea_F = sample(dim(train_seasonal_vac_F)[1],1000,replace = FALSE)
  selec_cols = sample(35,18,replace=FALSE)
  train_sea = rbind(train_seasonal_vac_T[val_ale_sea_T,], train_seasonal_vac_F[val_ale_sea_F,])
  model_seasonal <- train(seasonal_vaccine~., data = train_sea[,c(selec_cols,37)], method = "rpart", 
                          trControl = trainControl("cv", number = 10), 
                          tuneLength = 10)
  seasonal_predict = model_seasonal %>% predict(test)
  seasonal_predict = as.numeric(as.character(seasonal_predict))
  mat_val_seasonal = cbind(mat_val_seasonal,seasonal_predict)
}
tf=Sys.time()
print(tf-ti)

#Escribimos el fichero
test_id =read.csv("test_set_features.csv")
test_id = test_id[,1]
val_seasonal = apply(mat_val_seasonal,1,mean)
val_h1n1 = apply(mat_val_h1n1,1,mean)
randforest = data.frame(test_id,val_h1n1,val_seasonal)
names(randforest) = c("respondent_id","h1n1_vaccine","seasonal_vaccine")

write.csv(randforest,"randomforest.csv",row.names=FALSE)





