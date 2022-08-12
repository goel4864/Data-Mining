#install.packages('arules',dependencies = TRUE)
#install.packages('arulesViz',dependencies = TRUE)
library(arules)
data("Groceries")
?Groceries
View(Groceries)
str(Groceries)
summary(Groceries)
head(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules<-apriori(Groceries,parameter = list(supp=0.001,conf=0.8))
#inspect(rules)
inspect(head(rules))
inspect(rules[1:10])
rules<-sort(rules,by="confidence",decreasing = T)
rules<-apriori(Groceries,parameter = list(supp=0.0006,conf=0.8))
inspect(rules[1:10])
library(arulesViz)
#There are many method, plotting engines and all of them have different control parameters. Use
# "help" to get help. List available methods for the object rules:
plot(rules, method = "graph")
# List the available engines for method "scatterplot"
plot(rules, method = "scatterplot", engine = "ggplot2")
plot(rules[1:5],method="graph",shading=NULL)
plot(rules[1:5],method="graph",engine='interactive',shading=NULL)


#b part       
receipt_df<-read.csv("C:/Users/akanksha goel/OneDrive/Documents/program/1000i.csv",header = F)
#receipt_df
#Applying Column Names
names(receipt_df)<-c("Receipt_Number","Food","Quantity")
#Applying data preprocessing ,creating a dataframe each containing each item and its corresponding id
id<-c(1:5)
food<-c("milk","sugar","chocolate","apples","curd")
df<-data.frame(id,food)
receipt_df$Food<-df$food[match(receipt_df$Food,df$id)]
#After preprocessing
head(receipt_df)
typeof(receipt_df)
#Converting list type to basket format to run in apriori
test_df<-receipt_df[,c("Receipt_Number","Food","Quantity")]
df_trans<-as(split(test_df$Food,test_df$Receipt_Number),"transactions")
rules<-apriori(df_trans,parameter = list(supp=0.002,conf=.1))
inspect(head(rules))
#PLOTS
plot(rules, method = "graph")
# List the available engines for method "scatterplot"
plot(rules, method = "scatterplot", engine = "ggplot2")
plot(rules[1:5],method="graph",shading=NULL)
plot(rules[1:5],method="graph",engine='interactive',shading=NULL)

