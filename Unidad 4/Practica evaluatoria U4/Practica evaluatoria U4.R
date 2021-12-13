getwd()
setwd("C:/Users/Carlos Bojorquez/Desktop/Noveno semestre/Datos Masivones/iris-master")
getwd()
dataset = read.csv('iris.csv')
dataset = dataset[1:4]
install.packages("cluster")
library(cluster)
set.seed(101)
irisCluster <- kmeans(df[,1:4], center=3, nstart=20)
irisCluster

library(cluster)
clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)
tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  irisCluster <- kmeans(df[,1:4], center=i, nstart=20)
  tot.withinss[i] <- irisCluster$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)