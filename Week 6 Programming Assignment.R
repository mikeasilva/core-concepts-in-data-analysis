#d <- read.csv("pa4.csv", header=F)
d <- read.table("dataset_391_1.txt", header=F)
x <- as.matrix(d[,1:4])
xsvd <- svd(x)
#xsvd$v[,1]
ds <- sum(x * x)

#slide 57
y <- apply(x,2, function(x){ x-mean(x)})
y <- scale(x,scale=F) 

# Slide 52
x.eig <- eigen(t(x) %*% x)
mu <- sqrt(x.eig$values)
Xc <- (x %*% x.eig$vectors) 
z <- t(apply(Xc,1,function(x) x/mu))
zi <- sqrt(mu[1]) * -z[,1]
cv <- sqrt(mu[1]) * -x.eig$vectors[,1]

# Slide 56
mu1 <- xsvd$d[1] ^2
percant <- mu1 / ds 

# Slide 65
b <- t(y) %*% y/dim(x)[1]
b.eig <- eigen(b)
l1 <- b.eig$values[1]
c1 <- b.eig$vectors[,1]

#Sample Output
#4.1458 2.1055 2.8513 0.9619 96.4547
solution <- c(round(cv*-1,4),round((mu1 / ds )*100,4))
solution


