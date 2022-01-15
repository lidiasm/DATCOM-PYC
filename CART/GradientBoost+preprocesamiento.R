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


######################
#GradientBoost
#######################


y = as.numeric(as.character(train_h1n1$h1n1_vaccine))


ti =Sys.time()
learn_rate = 0.1 #Coeficiente de aprendizaje del modelo, cuanto más pequeño más depacio aprende
num_arb = 50 # número de árboles para gradientboost
max_prof = 3 #profundidad de los árboles
forest = list()#almacen de todos los predictore
iteraciones = 2000


for(iter in 1:iteraciones){
  #Inicializamos
  F0 = mean(y)
  Fm = F0
  trees = list()
  #Selección aleatoria de las filas
  muestra = sample(35,18,replace =FALSE)
  val_ale_h1n1_T = sample(dim(train_h1n1_vac_T)[1],50,replace = FALSE)
  val_ale_h1n1_F = sample(dim(train_h1n1_vac_F)[1],50,replace = FALSE) 
  train_h1n1 = rbind(train_h1n1_vac_T[val_ale_h1n1_T,], train_h1n1_vac_F[val_ale_h1n1_F,])
  y_prima = c(rep(1,50),rep(0,50))
  for (i in 1:num_arb){
    
    res = y-Fm
    train_h1n1$res = res
    dt = rpart(res~., data = train_h1n1[,muestra], maxdepth = max_prof)
    Fm = Fm + learn_rate*as.numeric(predict(dt,train_h1n1))
    
    trees[[i]] = dt
    
    
    
  }
  forest[[iter]] =trees
}
tf = Sys.time()
print(tf-ti)


#####
#Predecimos para h1n1

ti =Sys.time()
mat_h1n1 = c()

for (iter in 1:iteraciones){
  
  pred_h1n1 = rep(F0, dim(test)[1])
  for (i in 1:num_arb){
    pred_h1n1 = pred_h1n1 + learn_rate * as.numeric(predict(forest[[iter]][[i]],test))
    
  }
  mat_h1n1 = cbind(mat_h1n1,pred_h1n1)
}
tf = Sys.time()
print(tf-ti)

# Calculo de la probabilidad

val_h1n1 = apply(mat_h1n1,1,mean)


# Lo mismo para seasonal

y = as.numeric(as.character(train$seasonal_vaccine))

ti =Sys.time()
learn_rate = 0.1
num_arb = 50
max_prof = 3
forest = list()
iteraciones = 2000


for(iter in 1:iteraciones){
  y_prima = c(rep(1,50),rep(0,50))
  F0 = mean(y)
  Fm = F0
  trees = list()
  muestra = sample(35,18,replace =FALSE)
  val_ale_seasonal_T = sample(dim(train_seasonal_vac_T)[1],50,replace = FALSE)
  val_ale_seasonal_F = sample(dim(train_seasonal_vac_F)[1],50,replace = FALSE) 
  train_seasonal = rbind(train_seasonal_vac_T[val_ale_seasonal_T,], train_seasonal_vac_F[val_ale_seasonal_F,])
  y_prima = c(rep(1,50),rep(0,50))
  for (i in 1:num_arb){
    
    res = y_prima-Fm
    train_seasonal$res = res
    dt = rpart(res~., data = train_seasonal[,muestra], maxdepth = max_prof)
    Fm = Fm + learn_rate*as.numeric(predict(dt,train_seasonal))
    
    trees[[i]] = dt
    
    
    
  }
  forest[[iter]] =trees
}
tf = Sys.time()
print(tf-ti)

mat_seasonal = c()

for (iter in 1:iteraciones){
  
  pred_seasonal = rep(F0, dim(test)[1])
  for (i in 1:num_arb){
    pred_seasonal = pred_seasonal + learn_rate * as.numeric(predict(forest[[iter]][[i]],test))
    
  }
  mat_seasonal = cbind(mat_seasonal,pred_seasonal)
}

val_seasonal = apply(mat_seasonal,1,mean)

test_id =read.csv("test_set_features.csv")
test_id = test_id[,1]
gradientboost = data.frame(test_id,val_h1n1,val_seasonal)

names(gradientboost) = c("respondent_id","h1n1_vaccine","seasonal_vaccine")
write.csv(gradientboost,"gradientboost.csv",row.names=FALSE)















