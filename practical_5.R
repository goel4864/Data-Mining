View(iris)
n=nrow(iris)
n
#install.packages(q"caret",dependencies=TRUE)
#install.packages("rpart.plot",dependencies=TRUE)
#install.packages("e1071",dependencies=TRUE)
library(caret)
library(rpart.plot)
library(rpart)
library(caTools)
library(e1071)
#for(x in 1:10){
#PART 1
#Training set = 75%
  #HOLD OUT METHOD
  set.seed(123)
  split<-sample.split(iris$Species,SplitRatio = 0.75)
  iris_train = subset(iris,split==TRUE)
  iris_test = subset(iris,split==FALSE)
  dim(iris_test)
  dim(iris_train)
  dtm<-rpart(Species~., iris_train, method='class')
  print(dtm)
  plot(dtm)
  text(dtm)
  print(dtm)
  rpart.plot(dtm)
  
  rpart.plot(dtm,type=4,extra=101)
  p<-predict(dtm,iris_test,type='class')
  p
  
  confusionMatrix(iris_test[,5],p)
  print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)
  
  #SAMPLE RESAMPLING
  for (x in 1:10){
    split<-sample.split(iris$Species,SplitRatio = 0.75)
    iris_train = subset(iris,split==TRUE)
    iris_test = subset(iris,split==FALSE)
    dtm<-rpart(Species~., iris_train, method='class')
    print(dtm)
    rpart.plot(dtm)
    p<-predict(dtm,iris_test, type='class')
    print(confusionMatrix(iris_test[,5],p))
    print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)
  }
  
  #CROSS VALIDATION creating a model with cross validation 10 folds
  split<-sample.split(iris$Species,SplitRatio = 0.75)
  iris_train<-subset(iris,split=TRUE)
  iris_test<-subset(iris,split=FALSE)
  model = train(iris_train[,c(1,2,3,4)],iris_train$Species,method='rpart',trControl=trainControl(method='cv', number=10))
  p<-predict(model,iris_test, method='class')
  print(confusionMatrix(iris_test[,5],p))
  print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)

  #NORMALISE
  normalise<-function(x){return((x-min(x))/(max(x)-min(x)))}
  iris_n<-as.data.frame(lapply(iris[,c(1,2,3,4)],normalise))
  str(iris_n)
  summary(iris_n)
  
  
  #K NEAREST NEIGHBOURS
  iris_train_target<-iris_train[,5]
  iris_test_target<-iris_test[,5]
  k=sqrt(nrow(iris))
  modelKNN<-knn(iris_train[,c(1,2,3,4)],iris_test[,c(1,2,3,4)],iris_train_target,k)
  print(confusionMatrix(iris_test[,5],modelKNN))
  print(confusionMatrix(iris_test[,5],modelKNN)$overall['Accuracy']*100)
  
  #NAIVE BAYES THEOREM
  modelNB<-naiveBayes(Species~.,data=iris_train)
  pNB<-predict(modelNB,iris_test)
  print(confusionMatrix(iris_test[,5],pNB))
  print(confusionMatrix(iris_test[,5],pNB)$overall['Accuracy']*100)
  
#PART 2
#Training set = 66.6%  
  
  #HOLD OUT METHOD
  set.seed(123)
  split<-sample.split(iris$Species,SplitRatio = 0.666)
  iris_train = subset(iris,split==TRUE)
  
  iris_test = subset(iris,split==FALSE)
  dim(iris_test)
  dim(iris_train)
  dtm<-rpart(Species~., iris_train, method='class')
  print(dtm)
  plot(dtm)
  text(dtm)
  print(dtm)
  rpart.plot(dtm)
  rpart.plot(dtm,type=4,extra=101)
  p<-predict(dtm,iris_test,type='class')
  p
  confusionMatrix(iris_test[,5],p)
  print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)
  
  #SAMPLE RESAMPLING
  for (x in 1:10){
    split<-sample.split(iris$Species,SplitRatio = 0.666)
    iris_train = subset(iris,split==TRUE)
    iris_test = subset(iris,split==FALSE)
    dtm<-rpart(Species~., iris_train, method='class')
    print(dtm)
    rpart.plot(dtm)
    p<-predict(dtm,iris_test, type='class')
    print(confusionMatrix(iris_test[,5],p))
    print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)
  }
  
  #CROSS VALIDATION creating a model with cross validation 10 folds
  model = train(iris_train[,c(1,2,3,4)],iris_train$Species,'rpart',trControl=trainControl(method='cv', number=10))
  p<-predict(model,iris_test, type='class')
  print(confusionMatrix(iris_test[,5],p))
  print(confusionMatrix(iris_test[,5],p)$overall['Accuracy']*100)
  
  #NORMALISE
  normalise<-function(x){return((x-min(x))/(max(x)-min(x)))}
  iris_n<-as.data.frame(lapply(iris[,c(1,2,3,4)],normalise))
  str(iris_n)
  summary(iris_n)
  
  #K NEAREST NEIGHBOURS
  iris_train_target<-iris_train[,5]
  iris_test_target<-iris_test[,5]
  k=sqrt(nrow(iris))
  modelKNN<-knn(iris_train[,c(1,2,3,4)],iris_test[,c(1,2,3,4)],iris_train_target,k)
  print(confusionMatrix(iris_test[,5],modelKNN))
  print(confusionMatrix(iris_test[,5],modelKNN)$overall['Accuracy']*100)
  
  #NAIVE BAYES THEOREM
  modelNB<-naiveBayes(Species~.,data=iris_train)
  pNB<-predict(modelNB,iris_test)
  print(confusionMatrix(iris_test[,5],pNB))
  print(confusionMatrix(iris_test[,5],pNB)$overall['Accuracy']*100)
  
#---------------------BREAST CANCER----------------  
  cc= read.csv("C:/Users/akanksha goel/OneDrive/Documents/program/breast-cancer-wisconsin.data", header=TRUE)
  names(cc)=c("code_number","clump thickness","cell size","cell shape","marginal adhesion","epithelial cell size","bare nuclei","bland chromatin","normal nucleoli","mitoses","Class")
  cc <- na.omit(cc)
  head(cc)
  
  cancer = as.data.frame(lapply(cc, function(x){
    if(any(is.infinite(x))){
      which(is.infinite(x))
      x[is.infinite((x))] = 0
    }
    return(as.numeric(x))
  }))
  cancer = na.omit(cancer)
  
  library(caret)
  library(rpart.plot)
  library(rpart)
  library(caTools)
  library(e1071)
  #for(x in 1:10){
  #PART 1
  #Training set = 75%
  #HOLD OUT METHOD
  set.seed(124)
  split<-sample.split(factor(cancer[,11]),SplitRatio = 0.75)
  b_train = subset(cancer,split==TRUE)
  b_test = subset(cancer,split==FALSE)
  dim(b_test)
  dim(b_train)
  dtm<-rpart(Class~., b_train, method='class')
  print(dtm)
  plot(dtm)
  text(dtm)
  print(dtm)
  rpart.plot(dtm)
  rpart.plot(dtm,type=4,extra=101)
  pb<-predict(dtm,b_test,type='class')
  pb
  confusionMatrix(factor(b_test[,11]),pb)
  confusionMatrix(factor(b_test[,11]),pb)$overall['Accuracy']*100
    
  #SAMPLE RESAMPLING
  for (x in 1:10){
    split<-sample.split(factor(cancer[,11]),SplitRatio = 0.75)
    b_train = subset(cancer,split==TRUE)
    b_test = subset(cancer,split==FALSE)
    dim(b_test)
    dim(b_train)
    dtm<-rpart(Class~., b_train, method='class')
    print(dtm)
    rpart.plot(dtm)
    pb<-predict(dtm,b_test,type='class')
    pb
    confusionMatrix(factor(b_test[,11]),pb)
    confusionMatrix(factor(b_test[,11]),pb)$overall['Accuracy']*100
  }
  
  #CROSS VALIDATION creating a model with cross validation 10 folds
  model = train(b_train[,c(2:10)],factor(b_train[,11]),'rpart',trControl=trainControl(method='cv', number=10))
  p<-predict(model,b_test[,c(2:10)])
  confusionMatrix(factor(b_test[,11],c('2','4')),p)$overall['Accuracy']*100
  
  
  #NORMALISE
  normalise<-function(x){return((x-min(x))/(max(x)-min(x)))}
  cancer_n<-as.data.frame(lapply(cancer[,2:10],normalise))
  str(cancer_n)
  summary(cancer_n)
  
  #K NEAREST NEIGHBOURS
  b_train_target<-b_train[,11]
  b_test_target<-b_test[,11]
  k=sqrt(nrow(cc))
  knn_p<-knn(b_train[,c(2:10)],b_test[,c(2:10)],b_train_target,k)
  confusionMatrix(factor(b_test[,11],c('2','4')),knn_p)$overall['Accuracy']*100
  
  #NAIVE BAYES THEOREM
  modelNB<-naiveBayes(Class~.,data=b_train)
  pNB<-predict(modelNB,b_test)
  print(confusionMatrix(factor(b_test[,11],c('2','4')),pNB))
  print(confusionMatrix(factor(b_test[,11],c('2','4')),pNB)$overall['Accuracy']*100)
  
  #PART 2
  #Training set = 66.6%  
  
  #HOLD OUT METHOD
  set.seed(124)
  split<-sample.split(factor(cancer[,11]),SplitRatio = 0.666)
  b_train = subset(cancer,split==TRUE)
  b_test = subset(cancer,split==FALSE)
  dim(b_test)
  dim(b_train)
  dtm<-rpart(Class~., b_train, method='class')
  print(dtm)
  plot(dtm)
  text(dtm)
  print(dtm)
  rpart.plot(dtm)
  rpart.plot(dtm,type=4,extra=101)
  pb<-predict(dtm,b_test,type='class')
  pb
  confusionMatrix(factor(b_test[,11]),pb)
  confusionMatrix(factor(b_test[,11]),pb)$overall['Accuracy']*100
  
  #SAMPLE RESAMPLING
  for (x in 1:10){
    split<-sample.split(factor(cancer[,11]),SplitRatio = 0.666)
    b_train = subset(cancer,split==TRUE)
    b_test = subset(cancer,split==FALSE)
    dim(b_test)
    dim(b_train)
    dtm<-rpart(Class~., b_train, method='class')
    print(dtm)
    rpart.plot(dtm)
    pb<-predict(dtm,b_test,type='class')
    pb
    confusionMatrix(factor(b_test[,11]),pb)
    confusionMatrix(factor(b_test[,11]),pb)$overall['Accuracy']*100
  }
  
  #CROSS VALIDATION creating a model with cross validation 10 folds
  model = train(b_train[,c(2:10)],factor(b_train[,11]),'rpart',trControl=trainControl(method='cv', number=10))
  p<-predict(model,b_test[,c(2:10)])
  confusionMatrix(factor(b_test[,11],c('2','4')),p)$overall['Accuracy']*100
  
  
  #NORMALISE
  normalise<-function(x){return((x-min(x))/(max(x)-min(x)))}
  cancer_n<-as.data.frame(lapply(cancer[,2:10],normalise))
  str(cancer_n)
  summary(cancer_n)
  
  #K NEAREST NEIGHBOURS
  b_train_target<-b_train[,11]
  b_test_target<-b_test[,11]
  k=sqrt(nrow(cc))
  
  knn_model<-knn(b_train[,c(2:10)],b_test[,c(2:10)],b_train_target,k)
  confusionMatrix(factor(b_test[,11],c('2','4')),knn_model)$overall['Accuracy']*100
  
  #NAIVE BAYES THEOREM
  modelNB<-naiveBayes(Class~.,data=b_train)
  pNB<-predict(modelNB,b_test)
  print(confusionMatrix(factor(b_test[,11],c('2','4')),pNB))
  print(confusionMatrix(factor(b_test[,11],c('2','4')),pNB)$overall['Accuracy']*100)
  
  