#data <- read.table("iris.txt")
#data <- iris
data <- read.table("dataset_393_1.txt", header=F)
X <- as.matrix(data[,1:4])
meansX <- colMeans(X)
maxX <- apply(X,2,max)
minX <- apply(X,2,min)
NormalizedX <- t(apply(X,1,function(line){(line-meansX)/(maxX-minX)}))

results <- kmeans(NormalizedX, centers=NormalizedX[c(1,51,101),1:4], iter.max=10)
#results <- kmeans(NormalizedX, centers=NormalizedX[c(1,6,11),1:4], iter.max=6)
#table(results$cluster, iris$Species)
round(results$centers,4)
