#Hirales Lazareno Raymundo
#Galaviz Lona Oscar Eduardo

install.packages ("caret")
library(e1071)
library (caret)


# Importing the dataset
dataset <- read.csv('Social_Network_Ads.csv')


t.ids <- createDataPartition(dataset$Purchased, p=0.67, list=F)
mod <- naiveBayes(Purchased ~ .,data = dataset[t.ids,])
mod