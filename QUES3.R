#IRIS DATASET
View(iris)
summary(iris)
sapply(iris[,1:4],sd)
#install.packages("caret")
library(caret)
iris_pre_final<-preProcess(iris[,1:4],method=c("center","scale"))
iris_transformed_final<-predict(iris_pre_final,iris[,1:4])
summary(iris_transformed_final)
sapply(iris_transformed_final,sd)
#WINE DATASET
data_wine<-read.csv(file="C://Users/akanksha goel/OneDrive/Documents/program/wine.data",header = TRUE,sep=',')
data_wine
View(data_wine)
summary(data_wine)
sapply(data_wine,sd)
library(caret)
wine_pre_final<-preProcess(data_wine[,2:13],method=c("center","scale"))
wine_transformed_final<-predict(wine_pre_final,data_wine[,2:13])
summary(wine_transformed_final)
sapply(wine_transformed_final,sd)

