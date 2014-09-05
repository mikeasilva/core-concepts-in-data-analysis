"
Question 1
A binary feature is presented with two mutually complementary columns over the set of six objects as shown in the table below.
X  Y
1	1	0
2	0	1
3	0	1
4	1	0
5	0	1
6	1	0
Can you estimate the correlation coefficient between X and Y?
"
X <- c(1, 0, 0, 1, 0, 1)
Y <- c(0, 1, 1, 0, 1, 0)
df <- data.frame(X=X, Y=Y)
cor(df$X,df$Y)


"
Question 2
Let the correlation coefficient between X and Y be equal to 0.5.Can you estimate the proportion of the variance of Y taken into account by the linear regression y=ax+b.
"
0.5^2
"
Question 4
The standard deviation of Y is 1. The standard deviation of X is 3.The correlation coefficient between X and Y is 0.9. What is the value of the slope in the linear regression equation Y=aX+b.
"
stdY <- 3
stdX <- 2
rho <- 0.6
slope <- rho ??? stdY / stdX
slope

"
Question 6
Two features, X and Y, are presented in the table below. Find the coefficients of the linear regression equation Y=aX+b, the correlation coefficient and the determinacy coefficient. Estimate the value of Y at an object with X=1.5. Output in the following order: a, b, the correlation coefficient, the estimated value of Y. (There must be four numbers separated by spaces.)
X  Y
1	1.1333	4.9
2	-3.8	-2.5
3	2.6667	7.2
4	3.4	8.3
5	1.3333	5.2
6	1.9333	6.1
7	1.8667	6.0
8	-0.9333	1.8
9	1.1333	4.9
10	-2.7333	-0.9

"
X <- c(1.1333, -3.8, 2.6667, 3.4, 1.3333, 1.9333, 1.8667, -0.9333, 1.1333, -2.733)
Y <- c(4.9, -2.5, 7.2, 8.3, 5.2, 6.1, 6.0, 1.8, 4.9, -0.9)
df <- data.frame(X=X, Y=Y)
#plot(df$X, df$Y)

cor(df$X,df$Y)

reg <- lm(df$Y ~ df$X)
summary(reg)

