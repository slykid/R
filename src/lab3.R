install.packages("ISLR")
install.packages("boot")

library(ISLR)
library(boot)

Auto <- Auto
data <- Auto[,c("mpg", "horsepower")]
attach(data)

lm.fit <- lm(mpg ~ poly(horsepower,1), data=data)
names(lm.fit)

test_MSE<-array(0L, c(10, length(mpg)))
MSE_loocv<-rep(0,10)

for(i in 1:10) {
  for (k in 1:length(mpg)) {
    train <- (1:length(mpg))[-k]
    poly.fit <- lm(mpg ~ poly(horsepower, i, raw=T), data = Auto, subset = train)
    test_MSE[i, k] <- mean((mpg - predict(poly.fit, Auto))[-train]^2)
  }
  MSE_loocv[i] <- mean(test_MSE[i,])
}
plot(1:10, MSE_loocv, type='b', xlab="Degree of Polynominal", ylab="MSE")

lm.fit <- lm(mpg ~ poly(horsepower, 1, raw=T), data=data)
###############################################################3
install.packages("MASS")
library(MASS)

mvrnorm(n=1000)
