# 필요한 패키지 로드
library(ada)
library(rpart)
library(caret)
library(dplyr)

# 데이터 로드
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
dataset <- read.csv(url, header = FALSE)
colnames(dataset) <- c("Class label", "Alcohol", "Malic acid", "Ash", "Alcalinity of ash", 
                       "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoids phenols", 
                       "Proanthocyanins", "Color intensity", "Hue", 
                       "OD280/OD315 of diluted wines", "Proline")

# 클래스 1을 제외
dataset <- filter(dataset, `Class label` != 1)

# x와 y 정의
y <- as.factor(dataset$`Class label`)
x <- dataset[, c("Alcohol", "OD280/OD315 of diluted wines")]

# 데이터 분할
set.seed(1)
trainIndex <- createDataPartition(y, p = .8, list = FALSE, times = 1)
x_train <- x[trainIndex, ]
y_train <- y[trainIndex]
x_test <- x[-trainIndex, ]
y_test <- y[-trainIndex]

# 의사결정나무 모델
tree <- rpart(y_train ~ ., data = data.frame(x_train, y_train), method = "class", 
              parms = list(split = "information"), control = rpart.control(maxdepth = 1))

# 모델 학습 (의사결정나무)
y_train_pred_tree <- predict(tree, newdata = data.frame(x_train), type = "class")
y_test_pred_tree <- predict(tree, newdata = data.frame(x_test), type = "class")

tree_train <- mean(y_train_pred_tree == y_train)
tree_test <- mean(y_test_pred_tree == y_test)
cat(sprintf("의사결정나무의 훈련정확도 / 테스트정확도 : %.3f / %.3f\n", tree_train, tree_test))

# AdaBoost 모델
ada_model <- ada(y_train ~ ., data = data.frame(x_train, y_train), 
                 iter = 500, nu = 0.1, control = rpart.control(maxdepth = 1))

# 모델 학습 (AdaBoost)
y_train_pred_ada <- predict(ada_model, newdata = data.frame(x_train))
y_test_pred_ada <- predict(ada_model, newdata = data.frame(x_test))

ada_train <- mean(y_train_pred_ada == y_train)
ada_test <- mean(y_test_pred_ada == y_test)
cat(sprintf("AdaBoosting의 훈련정확도 / 테스트정확도 : %.3f / %.3f\n", ada_train, ada_test))
