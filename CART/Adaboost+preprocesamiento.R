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
#Adaboost
##################

ti = Sys.time()
total_arboles = 1000 # numero de arboles generado por adaboost
repeticiones = 600 #iteraciones del randomforest
mat_h1n1 = c() # Alamacenamos los resultado
for(iter in 1:repeticiones){
  #Selección aleatoria de filas
  val_ale_h1n1_T = sample(dim(train_h1n1_vac_T)[1],1000,replace = FALSE)
  val_ale_h1n1_F = sample(dim(train_h1n1_vac_F)[1],1000,replace = FALSE) 
  train_h1n1 = rbind(train_h1n1_vac_T[val_ale_h1n1_T,], train_h1n1_vac_F[val_ale_h1n1_F,])
  #pesos para el adaboost
  pesos = rep(1/dim(train_h1n1)[1],dim(train_h1n1)[1])
  trees = list()
  amount_arr = c()
  columnas = sample(35,18,replace = FALSE)
  for(i in 1:total_arboles){
    #rpart con los peso
    dt = rpart(h1n1_vaccine~., data = train_h1n1[,c(columnas,36)],maxdepth = 2, weights = pesos)
    pred = predict(dt, train_h1n1,"class")
    incorrect = c(pred != train_h1n1$h1n1_vaccine)
    total_error =sum(pesos[incorrect])
    amount_of_say = 0.5 * log((1-total_error)/total_error)
    amount_arr = c(amount_arr,amount_of_say)
    pesos[incorrect] = pesos[incorrect]*exp(amount_of_say) 
    pesos[!incorrect] = pesos[!incorrect]*exp(-amount_of_say) 
    #nuevos pesos
    pesos = pesos/sum(pesos)
    
    trees[[i]]=dt
  }
  #Predecimos para este adaboost
  vacunado = rep(0,dim(test)[1])
  NOvacunado = rep(0,dim(test)[1])
  vect = c()
  for(i in 1:total_arboles){
    pred = as.numeric(as.character(predict(trees[[i]], test, "class")))
    vacunado[which(pred==1)] = vacunado[which(pred==1)] + amount_arr[i]
    NOvacunado[which(pred == 0)] = NOvacunado[which(pred == 0)] + amount_arr[i]
    
  }
  vect[vacunado>NOvacunado] = 1
  vect[vacunado<=NOvacunado] = 0
  mat_h1n1 = cbind(mat_h1n1, vect)
  
}
tf = Sys.time()
print(tf-ti)



ti = Sys.time()
total_arboles = 1000
repeticiones = 600
amount_mat = c()
mat_seas = c()
for(iter in 1:repeticiones){
  val_ale_seasonal_T = sample(dim(train_seasonal_vac_T)[1],1000,replace = FALSE)
  val_ale_seasonal_F = sample(dim(train_seasonal_vac_F)[1],1000,replace = FALSE) 
  train_seas = rbind(train_seasonal_vac_T[val_ale_seasonal_T,], train_seasonal_vac_F[val_ale_seasonal_F,])
  
  pesos = rep(1/dim(train_seas)[1],dim(train_seas)[1])
  trees = list()
  amount_arr = c()
  columnas = sample(35,18,replace = FALSE)
  for(i in 1:total_arboles){
    dt = rpart(seasonal_vaccine~., data = train_seas[,c(columnas,37)],maxdepth = 2, weights = pesos)
    pred = predict(dt, train_seas,"class")
    incorrect = c(pred != train_seas$seasonal_vaccine)
    total_error =sum(pesos[incorrect])
    amount_of_say = 0.5 * log((1-total_error)/total_error)
    amount_arr = c(amount_arr,amount_of_say)
    pesos[incorrect] = pesos[incorrect]*exp(amount_of_say) 
    pesos[!incorrect] = pesos[!incorrect]*exp(-amount_of_say) 
    pesos = pesos/sum(pesos)
    
    trees[[i]]=dt
  }
  vacunado = rep(0,dim(test)[1])
  NOvacunado = rep(0,dim(test)[1])
  vect = c()
  for(i in 1:total_arboles){
    pred = as.numeric(as.character(predict(trees[[i]], test, "class")))
    vacunado[which(pred==1)] = vacunado[which(pred==1)] + amount_arr[i]
    NOvacunado[which(pred == 0)] = NOvacunado[which(pred == 0)] + amount_arr[i]
    
  }
  vect[vacunado>NOvacunado] = 1
  vect[vacunado<=NOvacunado] = 0
  mat_seas = cbind(mat_seas, vect)
  
}
tf = Sys.time()
print(tf-ti)

#Almacenaosmo el resultado
test_id =read.csv("test_set_features.csv")
test_id = test_id[,1]
val_seasonal = apply(mat_seas,1,mean)
val_h1n1 = apply(mat_h1n1,1,mean)
adaboost = data.frame(test_id,val_h1n1,val_seasonal)
names(adaboost) = c("respondent_id","h1n1_vaccine","seasonal_vaccine")
write.csv(adaboost,"adaboost.csv",row.names=FALSE)






