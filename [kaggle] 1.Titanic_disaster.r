setwd("D:/workspace/R")

train <- read.csv("Data/titanic/train.csv", stringsAsFactors = F)
test <- read.csv("Data/titanic/test.csv", stringsAsFactors = F)

# ������ Ž��
str(train)
str(test)

# ������ ��ó��
# Survived, Pclass, Sex, Embarked 4�� �÷��� ������ �����̹Ƿ� factor ������ ��ȯ���ش�.
train$Survived <- factor(train$Survived, levels=c(0, 1), labels=c("dead", "survived"))
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)

str(train)
str(test)

# ���������� ��ȯ�� Embarked ��  "" ���� �����ϴ� ���� Ȯ��
# label ���� "" �� NA �� ��ȯ �ʿ�
levels(train$Embarked)[1] <- NA
table(train$Embarked, useNA="always")
#   C    Q    S   <NA> 
#  168   77  644    2

# Embarked �Ӹ� �ƴ϶� cabin ���� "" ���� �����ϱ� ������ ó�����ش�.
train$Cabin <- ifelse(train$Cabin=="", NA, train$Cabin)
str(train$Cabin)
# chr [1:891] NA "C85" NA "C123" NA NA "E46" NA NA ...

# �׽�Ʈ �����͵� �����ϰ� ����
# Survived �� �����Ƿ� �ش� ������ ����
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

str(test)

test$Cabin <- ifelse(test$Cabin=="", NA, test$Cabin)


# ����-��� ���� Ȯ��
prop.table(table(train$Survived))
#     dead  survived 
#0.6161616 0.3838384

summary(train)

# �׽�Ʈ ������ ��ȯ
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
levels(test$Embarked)[1] <- NA
test$Cabin <- ifelse(test$Cabin == "", NA, test$Cabin)

# ������ ��� Ȯ��
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

# �𵨸�
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

# ���� ����
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
