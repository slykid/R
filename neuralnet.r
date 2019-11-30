setwd("D:/workspace/R")

#install.packages("neuralnet")
library("neuralnet")

data <- read.csv("Data/concrete.csv")
str(data)

summary(data)
boxplot(data, main="Concrete components")

normalize <- function(x) {
  return( (x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(data, normalize))
boxplot(concrete_norm, main="Concrete components applied norm")

train <- concrete_norm[1:773, ]
test <- concrete_norm[774:1030, ]

model <- neuralnet(strength ~ . , data = train)
result <- compute(model, test[1:8])

length(result$neurons)
print(result$neurons)
print(result$neurons[0])
print(result$neurons[1])

pred <- result$net.result
plot(model)

cor(pred, test$strength)

model2 <- neuralnet(strength ~ . , data = train, hidden = 5)
plot(model2)
result2 <- compute(model2, test[,1:8])
pred2 <- result2$net.result
cor(pred2, test$strength)
