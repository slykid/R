setwd("D:/workspace/R")

#install.packages("kernlab")
library(kernlab)

data <- read.csv("Data/letterdata.csv", header=T)
str(data)

# ������ ���� �н� : �׽�Ʈ = 4 : 1 ������ ������.
train <- data[1:16000,]
test <- data[16001:20000,]

# kernlab ��Ű���� ksvm()
model_ksvm <- ksvm(letter ~ ., data=train, kernel="vanilladot")
model_ksvm  # ���� ���� ����� �� �˷� ���� ����.

y_pred_ksvm <- predict(model_ksvm, test)
head(y_pred_ksvm)

table(y_pred_ksvm, test$letter)

performance_ksvm <- y_pred_ksvm==test$letter
table(performance_ksvm)
prop.table(table(performance_ksvm))

model_ksvm <- ksvm(letter ~ ., data=train, kernel="rbfdot")
y_pred_ksvm <- predict(model_ksvm, test)

performance_ksvm <- y_pred_ksvm==test$letter
table(performance_ksvm)
prop.table(table(performance_ksvm))
