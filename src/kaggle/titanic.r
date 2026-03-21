# 목표 : 승객의 생존 여부를 결정하는 ML 모델 생성

setwd("C:/workspace/R")

train <- read.csv("Data/titanic/train.csv", stringsAsFactors = F)
test <- read.csv("Data/titanic/test.csv", stringsAsFactors = F)

str(train)
str(test)

# 범주형 변수 처리
train$Survived <- factor(train$Survived, levels=c(0, 1), labels = c("dead", "survived"))
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

str(train)

## Embarked 변수 라벨 수정
levels(train$Embarked)
table(train$Embarked)
#    C   Q   S 
#2 168  77 644 

levels(train$Embarked)[1] <- NA
table(train$Embarked, useNA = "always")
#  C     Q   S   <NA> 
#  168   77  644    2 

## Cabin 변수 값 중 공백문자 NA로 변환
unique(train$Cabin)
# [1] "" 

train$Cabin <- ifelse(train$Cabin == "", NA, train$Cabin)
str(train)
unique(train$Cabin)[1]
# [1] NA

str(train)

summary(train)

# 테스트 데이터 변환
#test$Survived <- factor(test$Survived, levels=c(0, 1), labels = c("dead", "survived"))
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
levels(test$Embarked)[1] <- NA
test$Cabin <- ifelse(test$Cabin == "", NA, test$Cabin)

# 데이터 요약 확인
install.packages("Hmisc")
library(Hmisc)

names(train)
summary(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="reverse")


install.packages("caret", "ellipse")
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
install.packages("rpart", "rpart.plot")
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
install.packages("randomForest")
library(randomForest)

model2 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                       data = na.exclude(train), ntree=500, importance=TRUE)
model2
pred2 <- predict(model, newdata = test)
result <- data.frame(test$PassengerId, ifelse(pred=="dead", 0, 1))
colnames(result) <- c("PassengerId", "Survived")
write.csv(result, "Data/submission3.csv",row.names = F)



