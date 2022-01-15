library(ISLR)
library(plyr)
library(moments)
library(nortest)
library(MASS)
library(class)
library(readxl)
library(tidyverse)
library(RWeka)
library(pROC)
library(mlr)
library(ROSE)
library(caret)
library(e1071)
library(Hmisc)
library(factoextra)
library(smotefamily)
options(StringsAsFactors=T)
options(max.print=100)

dif = read.csv("C:/Users/javij/Desktop/training_clean.csv", stringsAsFactors = T)
res = read.csv("C:/Users/javij/Desktop/test_clean.csv")
id = dif[1]
dif = dif[,-1]
#eliminamos la columna id en la linea anterior y creamos dos dataset, cada uno con una de las variables objetivo
df2 = dif[,-34]
df= dif[,-33] #tiene  seasonal
idres = res[1]
res = res[,-1]

#cambiamos todas las variables a factor para balanceo
df[names(df)] <- lapply(df[names(df)] , factor)
df2[names(df2)] <- lapply(df2[names(df2)] , factor)
res[names(res)] <- lapply(res[names(res)] , factor)

#balanceamos el dataset con la variable h1n1 que estaba desbalanceada
df2$h1n1_vaccine <- factor(df2$h1n1_vaccine)
data_balanc2 <-upSample(df2,df2$h1n1_vaccine)
data_balanc2 = data_balanc2[,-34]

#separamos el conjunto de train en train y test para el entrenamiento y  validación
train=sample (1:(nrow(df)), trunc(nrow(df)*0.7))
test= df[-train ,]

train2=sample (1:(nrow(data_balanc2)), trunc(nrow(data_balanc2)*0.7))
test2= data_balanc2[-train2 ,]


#Random forest con 16 columnas
mat_val_seasonal= rep(0,nrow(res))
for(i in 1:1000){
  val_ale = sample(32,16,replace=FALSE)
  model_seasonal <- J48(seasonal_vaccine~., data=df[,c(val_ale,33)], subset=train)
  seasonal_predict = predict(model_seasonal, res,type = "probability")
  mat_val_seasonal = seasonal_predict[,2] + mat_val_seasonal
}
mat_val_seasonal = mat_val_seasonal / 1000
mat_val_seasonal

mat_val_h1n1 = rep(0,nrow(res))
for(i in 1:1000){
  val_ale = sample(32,16,replace=FALSE)
  model_h1n1 <- J48(data_balanc2$h1n1_vaccine~., data=data_balanc2[,c(val_ale,33)], subset=train2)
  h1n1_predict = predict(model_h1n1, res,type = "probability")
  mat_val_h1n1 = h1n1_predict[,2] + mat_val_h1n1
}
mat_val_h1n1 = mat_val_h1n1 / 1000
mat_val_h1n1

#Creamos el dataset final con los resultados
resultado = data.frame(idres,mat_val_h1n1,mat_val_seasonal)
colnames(resultado)[2] = "h1n1_vaccine"
colnames(resultado)[3] = "seasonal_vaccine"
resultado
names(resultado)
write.csv(resultado,"C:/Users/javij/Desktop/submission.csv", row.names = FALSE)