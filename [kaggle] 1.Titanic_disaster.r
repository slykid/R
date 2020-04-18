setwd("D:/workspace/R")

train <- read.csv("Data/titanic/train.csv", stringsAsFactors = F)
test <- read.csv("Data/titanic/test.csv", stringsAsFactors = F)

# 데이터 탐색
str(train)
str(test)

# 데이터 전처리
# Survived, Pclass, Sex, Embarked 4개 컬럼은 범주형 변수이므로 factor 형으로 변환해준다.
train$Survived <- factor(train$Survived, levels=c(0, 1), labels=c("dead", "survived"))
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

str(train)
str(test)

# 팩터형으로 변환된 Embarked 에  "" 값이 존재하는 것을 확인
# label 값인 "" 을 NA 로 변환 필요
levels(train$Embarked)[1] <- NA
table(train$Embarked, useNA="always")
#   C    Q    S   <NA> 
#  168   77  644    2

# Embarked 뿐만 아니라 cabin 에도 "" 값이 존재하기 때문에 처리해준다.
train$Cabin <- ifelse(train$Cabin=="", NA, train$Cabin)
str(train$Cabin)
# chr [1:891] NA "C85" NA "C123" NA NA "E46" NA NA ...

# 테스트 데이터도 동일하게 적용
# Survived 는 없으므로 해당 과정만 제외
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

str(test)

test$Cabin <- ifelse(test$Cabin=="", NA, test$Cabin)


# 생존-사망 비중 확인
prop.table(table(train$Survived))
#     dead  survived 
#0.6161616 0.3838384

summary(train)

# 테스트 데이터 변환
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
levels(test$Embarked)[1] <- NA
test$Cabin <- ifelse(test$Cabin == "", NA, test$Cabin)

# 데이터 요약 확인
#install.packages("Hmisc")
library(Hmisc)

names(train)
summary(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="reverse")


#install.packages("caret", "ellipse")
library(caret)
library(ellipse)
train.complete <- train[complete.cases(train), ]
table(train.complete[, c("Survived")])
featurePlot(train.complete[
  ,sapply(names(train.complete),
          function(n) { is.numeric(train.complete[,n]) })], 
  train.complete[, c("Survived")], 
  "ellipse"
)

mosaicplot(Survived ~ Pclass + Sex, data=train, color=TRUE, main="Pclass & Sex")
round(xtabs(Survived=="survived" ~ Sex + Pclass, data=train) / xtabs(~ Sex + Pclass, data=train), 2)

# 모델링
#install.packages("rpart", "rpart.plot")
library(rpart)
library(rpart.plot)

model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train)
pred <- predict(model, newdata = test, type="class")
head(pred)
rpart.plot(model)


result <- data.frame(test$PassengerId, ifelse(pred=="dead", 0, 1))
colnames(result) <- c("PassengerId", "Survived")
write.csv(result, "Data/submission.csv",row.names = F)

# 성능 개선
## 1) randomForest
#install.packages("randomForest")
library(randomForest)

model2 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                       data = na.exclude(train), ntree=500, importance=TRUE)
model2
pred2 <- predict(model, newdata = test)
result <- data.frame(test$PassengerId, ifelse(pred=="dead", 0, 1))
colnames(result) <- c("PassengerId", "Survived")
write.csv(result, "Data/submission3.csv",row.names = F)
