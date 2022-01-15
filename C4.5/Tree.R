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
df= dif[,-33] #tiene la seasonal
idres = res[1]
res = res[,-1]

#Análisis PCA para ver si es necesario eliminar alguna variable
train.nums.df <- data.frame(sapply(c(1:ncol(dif)), 
                                   function(x) as.numeric(dif[, x])))
colnames(train.nums.df) <- colnames(dif)
str(train.nums.df)
train.nums.df[is.na(train.nums.df)] <- 0
train.pca.vars <- princomp(train.nums.df %>% select(-seasonal_vaccine,h1n1_vaccine))
pca = get_eig(train.pca.vars)
train.pca.vars
pca
fviz_eig(train.pca.vars, addlabels=TRUE,ncp = 33)
#Estudiamos el aporte de cada variable a las primeras 19 componentes
fviz_contrib(train.pca.vars, choice = "var", axes = 1:19, top = 30)
#Probamos MCA para ver si hay alguna diferencia
train.mca <- MCA(train.factors.df %>% select(-h1n1_vaccine, seasonal_vaccine), graph=FALSE)
fviz_screeplot(train.mca, addlabels = TRUE)

fviz_contrib(train.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(train.mca, choice = "var", axes = 2, top = 15)

#Estudiamos la matriz de correlación
train.cormatrix <- cor(train.nums.df[,-35])
train.cormatrix[lower.tri(train.cormatrix)] <- 0
train.cormatrix <- data.frame(train.cormatrix)
train.cormatrix <- train.cormatrix[order(-train.cormatrix$seasonal_vaccine), ]
train.cormatrix

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

modelC4.5 = J48(seasonal_vaccine~., data=df, subset=train)
modelC4.5.pred = predict(modelC4.5, test,type = "probability")
modelC4.5.pred[,1]

modelC4.52 = J48(h1n1_vaccine~., data=data_balanc2, subset=train2)
modelC4.52.pred = predict(modelC4.52, test2,type = "probability")
modelC4.52.pred[,1]

#Predecimos con el conjunto de test
val1 = predict(modelC4.5, res,type = "probability")
val2 = predict(modelC4.52, res,type = "probability")

#Creamos el dataSet final de resultados y lo guardamos 
resultado = data.frame(idres,val2[,2],val1[,2])
colnames(resultado)[2] = "h1n1_vaccine"
colnames(resultado)[3] = "seasonal_vaccine"
resultado
names(resultado)
write.csv(resultado,"C:/Users/javij/Desktop/submission.csv", row.names = FALSE)


