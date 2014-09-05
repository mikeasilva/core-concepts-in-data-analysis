"
Question 3
"
df <- data.frame(snow=c(12,5731),no.snow=c(987,1123634))
row.names(df) <- c("falling","no.falling")
source('~/Quetelet Index.R')
q_index(df)*100
chisq.test(df)

df <- iris[1:4]
sum(scale(df, scale=F)) > 0
svd.df <- svd(df)
svd.center.df <- svd(scale(df, scale=F))

c.df <- -svd.df$v[,1]
c.center.df <- -svd.center.df$v[,1]

"
Question 5
"

m <- matrix(rnorm(300), ncol=3)
svd.m <- svd(m)
svd.m$v

c1 <- c(0.2673, 0.5345, 0.8018)
c2 <- c(0.3015, -0.3015, -0.9045)
c1 * c2
"
Question 8
pg 228 of the book
"

c.prime <- c(0.2673, 0.5345, 0.8018)
mu <- 35
w <- c.prime/mu
f<-c(70,40,60)
sum(f*w)

"
Question 9
"
df <- data.frame(student = c(1:8), mark = c(50,75,80,60,55,40,35,50), it.occupation.it=c(rep(1,3),rep(0,5)))
center.data <- T
col.ranges <- apply(df, MARGIN= 2, range)
scale.data <- diff(col.ranges)
X <- scale(df, center=center.data, scale=scale.data)
apply(df, MARGIN= 2, mean)
"
build_cluster_step <- function (x, grps, start) {
  cluster_stage <- max(grps) + 1
  gain = 100 #any old number to start
  while (gain > 0) {
    unassigned <- length(grps[grps==0])
    x2 <- scale(x, center=start, scale=rep(1,ncol(x)))
    euc2distfromzero <- rowSums(x^2)
    euc2distfromnew <- rowSums(x2^2)
    grps[euc2distfromnew < euc2distfromzero & grps==0] <- cluster_stage
    gain <- unassigned - length(grps[grps==0])
    if (length(grps[grps == cluster_stage]) == 1){ #hack for colMeans needing multiple entries 
      start <- x[grps == cluster_stage,]
    } else{
      start <- colMeans(x[grps == cluster_stage,])
    }
  }
  return(grps)
}
#source('~/ikmeans.R')
X <-scale(df, center=TRUE, scale=rep(1,ncol(df)))
mygroups <- integer(nrow(X))
euc2distfromzero <- rowSums(X^2)
while (length(mygroups[mygroups==0]) >1){
  new_cluster_base <- X[euc2distfromzero == max(euc2distfromzero[mygroups==0]) & mygroups==0,]
  mygroups <- build_cluster_step(X,mygroups,new_cluster_base)
}

print(table(mygroups))
"