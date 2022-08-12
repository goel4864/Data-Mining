data_iris<-read.csv(file="C://Users/akanksha goel/OneDrive/Documents/program/dirty_iris.csv",header = TRUE)
data_iris
summary(data_iris)
str(data_iris)

#information of cases that are complete i.e rows that have value for each column
complete_cases<-complete.cases(data_iris)
complete_cases
#as.numeric(complete_cases)
sum_cc<-sum(complete_cases)#96
percent_cc<-100*sum_cc/length(complete_cases)
cat("COMPLETE CASES in dirty iris is ",percent_cc,"%")

#Special value(infinity) is replaced with na
any(is.na(data_iris))
which(is.na(data_iris$Sepal.Width))
is.na(data_iris)<-sapply(data_iris,is.infinite)
data_iris

library(editrules)
(Rules<-editfile(file="C://Users/akanksha goel/OneDrive/Documents/program/efques2.txt"))
voilated<-violatedEdits(Rules,data_iris)
voilated
summary(voilated)
plot(voilated)
boxplot(data_iris$Sepal.Length,horizontal = TRUE)
summary(data_iris$Sepal.Length)
boxplot.stats(data_iris$Sepal.Length)
