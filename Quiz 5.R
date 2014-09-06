"
Question 1
Take the Student data over Age, SE, OOP, CI (find the dataset in the Dataset 
section). Find the PCA hidden factor behind it. Norm the factor loadings to 
the square root of the maximum singular value. What are the resulting factor 
loadings?
"
names <- c(t(read.table("https://d396qusza40orc.cloudfront.net/datan/datasets/studn.var")))
df <- read.table("https://d396qusza40orc.cloudfront.net/datan/datasets/studn.dat", col.names = names)
df <- df[c("Age","SoftEngineering", "OOProgramming","CompIntelligence")]


svd.df <- svd(df)
# Factor Loadings
c <- -svd.df$v[,1]
# Hiden Factor
z <- -svd.df$u[,1]
# Maximum Singular Value
mu <- svd.df$d[1]

norm.factor <- sqrt(mu)
c*norm.factor

"
(10.0542, 17.8887, 19.0649, 17.2950)

"

#df <- df[c("SoftEngineering", "OOProgramming","CompIntelligence")]
#df <- df[1:6,]

#ds <- sum(df*df)
#df ~ mu*z*c
#alpha <- 1/sum(c)
#df$Avg <- rowMeans(df)

#df$PC.z <- alpha * as.matrix(df[,1:ncol(df)-1]) %*% as.matrix(c)

"
Question 2
Vizualization of the data. 
Take the same data and standardize by subtracting the feature means and, 
afterwards, dividing the features by their ranges. Norm the two first 
principal components to the square roots of the corresponding singular 
values. What are the first and the second loading vectors?
"
names <- c(t(read.table("https://d396qusza40orc.cloudfront.net/datan/datasets/studn.var")))
df <- read.table("https://d396qusza40orc.cloudfront.net/datan/datasets/studn.dat", col.names = names)
df <- df[c("Age","SoftEngineering", "OOProgramming","CompIntelligence")]
df.colmeans <- c(colMeans(df))
df.colMax <- c(apply(df, MARGIN=c(2), max))
df.colMin <- c(apply(df, MARGIN=c(2), min))
df.range <- df.colMax - df.colMin
#df.range <- c(apply(df, 2, function(x){max(x)-min(x)}))
df.norm <- t((t(df) - df.colmeans)/df.range)
X <- as.data.frame(df.norm)
svd.X <- svd(X)

c1 <- -svd.X$v[,1]
mu.sqrt1 <- sqrt(svd.X$d[1])
c2 <- -svd.X$v[,2]
mu.sqrt2 <- sqrt(svd.X$d[2])

c1*mu.sqrt1
c2*mu.sqrt2

#df.norm <- scale(df)
"
The first loading vector is (-1.081, 0.0632, 1.0794, 0.9985 )
The second loading vector is (0.8843, 1.2196, 0.1595, 0.7079)

NOT Answer:
The first loading vector is (-0.5920, 0.0346, 0.5910, 0.5468) 
The second loading vector is (0.5289, 0.7294, 0.0954, 0.4233)
"

"
Question 3
Output the total contribution of the 2D data model of Question 2 to the data 
scatter, per cent. Output one floating number without % sign. The answer 
format is xx.xxxx, for example, 11.1234.
"

z <- -svd.X$u[,1]
mu <- svd.X$d[1]
data.scater <- sum(X*X)
contribution <- ((mu*mu)/data.scater)*100
contribution
