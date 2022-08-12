#install.packages("editrules")
library(editrules)
people=read.table(file="C://Users/akanksha goel/OneDrive/Documents/program/people.txt",header=TRUE)
people
str(people)
summary(people)
E<-editset(expression(
                      AGE >= 0,
                      AGE < 150,
                      AGE > YEARSMARRIED,
                      STATUS %in% c('single','married','widowed'),
                      if(AGE < 18) AGEGROUP %in% c('child'),
                      if(AGE >= 18 && AGE <=65) AGEGROUP %in% c('adult'),
                      if(AGE >65) AGEGROUP %in% c('elderly')
                      ))
E
voilated<-violatedEdits(E,people)
people
voilated
summary(voilated)
?plot
plot(voilated)
plot(E)
plot(E,layout=layout.circle,type="people")
plot(E,type="l")
#install.packages("tidyverse")
#library(tidyverse)
#library(ggplot2)
#ggplot(p)