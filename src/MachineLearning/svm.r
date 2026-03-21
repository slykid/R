setwd("D:/workspace/R")

#install.packages("kernlab")
library(kernlab)

data <- read.csv("Data/letterdata.csv", header=T)
str(data)

# 데이터 셋을 학습 : 테스트 = 4 : 1 비율로 나눈다.
train <- data[1:16000,]
test <- data[16001:20000,]

# kernlab 패키지의 ksvm()
model_ksvm <- ksvm(letter ~ ., data=train, kernel="vanilladot")
model_ksvm  # 실제 모델의 결과를 잘 알려 주진 않음.

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
