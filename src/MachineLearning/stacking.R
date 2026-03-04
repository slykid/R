# 필요한 패키지 로드
library(caret)
library(caretEnsemble)
library(randomForest)
library(rpart)
library(kknn)
library(ada)

# 데이터 로드 및 분할
data(BreastCancer, package = "mlbench")
dataset <- na.omit(BreastCancer)

X_data <- dataset[, 2:10]
y_label <- as.factor(dataset$Class)

set.seed(0)
trainIndex <- createDataPartition(y_label, p = .8, list = FALSE)
X_training <- X_data[trainIndex, ]
y_training <- y_label[trainIndex]
X_testing <- X_data[-trainIndex, ]
y_testing <- y_label[-trainIndex]

# Phase 1. 개별 모델 생성 및 학습
# 개별 ML 모델을 위한 리스트 생성
control <- trainControl(method = "cv", number = 5, savePredictions = "final", classProbs = TRUE)

# 개별 모델들
models <- caretList(
  X_training, y_training,
  trControl = control,
  methodList = c("kknn", "rf", "rpart", "ada")
)

# 각 모델의 정확도 측정
results <- resamples(models)
summary(results)

# 각 모델의 테스트 데이터에 대한 예측
knn_pred <- predict(models$kknn, X_testing)
rf_pred <- predict(models$rf, X_testing)
dt_pred <- predict(models$rpart, X_testing)
ada_pred <- predict(models$ada, X_testing)

cat(sprintf("KNN 정확도: %.4f\n", mean(knn_pred == y_testing)))
cat(sprintf("랜덤 포레스트 정확도: %.4f\n", mean(rf_pred == y_testing)))
cat(sprintf("결정 트리 정확도: %.4f\n", mean(dt_pred == y_testing)))
cat(sprintf("에이다부스트 정확도: %.4f\n", mean(ada_pred == y_testing)))

# Phase 2. 최종 분류 모델 학습 및 예측
# 스태깅을 위한 모델 스택 생성
stack_control <- trainControl(method = "cv", number = 5, savePredictions = "final", classProbs = TRUE)
stack <- caretStack(models, method = "glm", metric = "Accuracy", trControl = stack_control)

# 최종 메타 모델의 예측
final_pred <- predict(stack, newdata = X_testing)
cat(sprintf("최종 메타 모델의 예측 정확도: %.4f\n", mean(final_pred == y_testing)))
