# 참고자료
# - https://www.kaggle.com/redhorse93/r-titanic-data
# - https://velog.io/@suzin/R-%EB%8D%B0%EC%9D%B4%ED%84%B0-%ED%83%90%EC%83%89-3.-Missing-Value%EA%B2%B0%EC%B8%A1%EC%B9%98-NA
# - https://wsyang.com/2014/02/introduction-to-dplyr/

# 사용할 라이브러리
#install.packages("dplyr","ggplot2","naniar", "VIM","sqldf", "randomForest", "cvTools")

library(plyr)
library(dplyr)
library(ggplot2)
library(naniar)
library(VIM)
library(sqldf)
library(rpart)
library(randomForest)
library(cvTools)
library(foreach)
library(caret)

# 1. 원본 데이터 로드
train_data <- read.csv("Data/titanic/train.csv")
test_data <- read.csv("Data/titanic/test.csv")

## 데이터 확인
str(train_data)
str(test_data)

## 데이터 통합 작업
## - train, test 모두 전처리가 필요할 경우를 대비하여 한 번에 수행하기 위한 통합데이터 생성
## - test 의 경우 예측변수인 Survived 가 없기 때문에 Survived 변수를 만들어 주는 대신 NA로 채워준다.
##   방법론
##   - test_data 쪽에 Survived 변수를 만들고 NA를 채워준 후 학습 데이터와 병합한다.
Survived <- rep(NA, nrow(test_data))
test_data_prep <- cbind(test_data, Survived)
data <- rbind(train_data, test_data_prep)
str(data)
summary(data)

##   - dplyr 라이브러리의 bind_rows()를 사용하면 없는 변수에 대해서는 NA를 자동으로 채워줌
data <- dplyr::bind_rows(train_data, test_data)
str(data)
summary(data)


# 2. 데이터 전처리
## 변수 속성 변환
data$Survived <- factor(data$Survived)
data$Pclass <- factor(data$Pclass, ordered = T)
data$Sex <- factor(data$Sex)
data$Cabin <- factor(data$Cabin)
data$Embarked <- factor(data$Embarked)

str(data)

# 3. EDA(탐색적 데이터 분석)
str(data)
names(data)

## 1) 결측지 여부 확인
gg_miss_var(data[,-(which(names(data)=='Survived'))], show_pct=T)
aggr(data[,-(which(names(data)=='Survived'))], prop=FALSE, combined=TRUE, numbers=TRUE, 
     sortVars=TRUE, sortCombs=TRUE)

## 2) Survived 와의 관계 확인
### (1) Age
### - 특이사항 : NA 값 있음
summary(data[,"Age"])
ggplot(data = data, aes(Age)) + 
  geom_histogram(breaks=seq(0, 80, by=1), col='red', 'fill'= 'skyblue', alpha=.5) +
  ggtitle('Titanic Passenger`s age histogram') +
  theme(plot.title=element_text(face='bold', hjust=0.5, size=15, color='black'))

ggplot(data=data[!is.na(data$Survived),], aes(Age,fill=Survived)) +
  geom_density(alpha=.3) + 
  ggtitle("Titanic Passenger`s age density plot") +
  theme(plot.title=element_text(face='bold', hjust=0.5, size=15, color='black'))

### (2) Pclass
data_pclass_cnt <- sqldf("select Pclass, Survived, count(*) as cnt 
                          from data 
                          where Survived not like 'NA' 
                          group by Survived, Pclass
                          order by 1, 2")
data_pclass_cnt

ggplot(data=data_pclass_cnt, aes(x=Pclass, y=cnt, fill=Survived)) +
  geom_bar(alpha=.5, stat='identity') +
  geom_text(aes(label=cnt), size=5, position=position_stack(vjust=0.5)) + 
  ggtitle('Titanic Passenger`s seat class') +
  theme(plot.title = element_text(face='bold', hjust=0, color='black'))
rm(data_pclass_cnt)

### (3) Sex
data_sex_cnt <- sqldf("select Sex, Survived, count(*) as cnt 
                       from data
                       where Survived not like 'NA' 
                       group by Survived, Sex
                       order by 1, 2")
data_sex_cnt
ggplot(data=data_sex_cnt, aes(x=Sex, y=cnt, fill=Survived)) +
  geom_bar(alpha=.5, stat='identity') +
  geom_text(aes(label=cnt), size=5, position=position_stack(vjust=0.5)) + 
  ggtitle('Titanic Passenger`s rate by Sex') +
  theme(plot.title = element_text(face='bold', hjust=0, color='black'))
rm(data_sex_cnt)

### (4) Fare
ggplot(data[!is.na(data$Survived),], aes(Survived, Fare)) +
  geom_jitter(col='gray') +
  geom_boxplot(alpha=.5) +
  ggtitle("Boxplot of passenger`s Fare") +
  theme(plot.title = element_text(face="bold", hjust=0.5, size=15))

## 3) 전처리
summary(data)

### (1) Fare
### - Fare 변수의 경우 na 값이 존재하는 데 금액이라는 수치형 변수이므로 NA를 0으로 변경
data$Fare <- replace(data$Fare, which(is.na(data$Fare)), 0)

### (2) Embarked
### - Embarked 변수의 경우 2개의 NA 값이 있으며, 나머지 범주 중에서 최빈값으로 포함시킴
names(table(data$Embarked)[which.max(table(data$Embarked))])
data$Embarked <- replace(data$Embarked, which(is.na(data$Embarked)), 
                         names(table(data$Embarked)[which.max(table(data$Embarked))]))
data$Embarked <- factor(data$Embarked, levels = c("C", "Q", "S"))
str(data)

### (3) Age
### - 사람의 나이가 없는 경우는 없기 때문에 NA인 경우에 대해서는 
data$Age <- ifelse(is.na(data$Age), round(mean(data$Age, na.rm = T), 0), data$Age)

### (4) 전처리 결과 확인
summary(data)


# 4, 모델링
##  1) train - test 데이터 분리
train <- data[1:891,]
test <- data[892:1309, ]

## 2) submission 생성을 위한 test$PassengerId 추출
ID <- test$PassengerId

## 3) 모델링에 사용할 샘플 추출
### 사용 컬럼 : Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Survived
### test_sample 은 survived 변수 제외하고 생성
train_sample <- train %>% select("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Survived")
#test_sample <- test %>% select("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")

## 4) 사용모델 : 의사결정나무 (rpart)
model_rpart <- rpart(formula = Survived ~ ., data=train_sample)
model_rpart
model_rpart$variable.importance
pred <- predict(model_rpart, newdata = test, type="class")

## 5) submission 생성
submission1 <- data.frame(ID, pred)
names(submission1) <- c("PassengerId", "Survived")
write.csv(submission1, "Result/titanic/submission1.csv", row.names = F)  # Accuracy : 0.77033
rm(submission1)

# 5. 성능개선
## 1) 추가 전처리
### 아무리 성능이 좋은 모델이더라도, 입력되는 데이터가  별로라면 결과도 별로인 것으로 나옴
### 데이터의 전처리가 중요하다!
### 한 번에 처리하기 위해 통합데이터인 data를 사용한다.

### 앞서 살펴 본 EDA를 기반으로 추가적인 유추를 한다.
### 유추1. 탑승객의 티켓이 동일한 경우, 이름 중 일부(ex. 성)가 같은 경우 가족일 가능성이 높다.
### 유추2. 남자보다 여자의 생존율이 높기 때문에 이름을 통해 해당 승객이 남자인지 여자인지 파악할 수 있다.
### 유추3. 나이에 따라 생존여부를 봤을 때, 특정 구간에서 사망자수에 비해 생존자 수가 많은 경우가 존재하기 때문에
###        나이에 따른 그룹을 생성하여 생존여부를 파악할 수 있다.

### 추가 전처리 계획
### (1) 탑승객의 가족 수에 따른 구분 변수 생성(티켓별, 가족구성 수 별)
### (2) 나이에 대한 그룹 생성
### (3) 성별 및 직급인 단어만 추출하기


### (1) 탑승객의 가족 구성 수에 따른 탑승객 규모 변수 생성
### - 0보다 큰 값의 개수 확인
sum(ifelse(data$Parch>0, 1, 0)) # 가족끼리 탑승한 승객 수: 307
sum(ifelse(data$Parch>0 & data$SibSp>0, 1, 0)) # 가족 중 형제가 있는 승객 수: 206

### - 탑승객의 가족 수를 계산해 가족 규모를 산출
### - 1인 : Single / 2 ~ 4인 : Small / 5인 이상 : Big 으로 구분
FamilySize <- data$Parch + data$SibSp + 1
data$FamilySize <- case_when(FamilySize == 1 ~ "Single",
                             FamilySize >= 2 & FamilySize < 5 ~ "Small",
                             FamilySize >= 5 ~ "Big")
data$FamilySize <- factor(data$FamilySize, levels = c("Single", "Small", "Big"))
str(data)
rm(FamilySize)


### (2) 티켓 수에 따른 탑승객 규모 변수 생성
### - 동일 티켓이라면 가족 혹은 일행일 확률이 높음
### - 1인 : Single / 2 ~ 4인 : Small / 5인 이상 : Big 으로 구분
head(sqldf("select ticket, count(1) cnt
       from data
       where Parch = 0 and SibSp = 0
       group by 1
       order by 2 desc
      "))

ticketCnt <- rep(0, nrow(data))
ticket_kwd <- unique(data$Ticket)
for(kwd in ticket_kwd) {
  idx <- which(data$Ticket == kwd)
  for(i in idx) {
    ticketCnt[i] <- length(idx)
  }
}
summary(ticketCnt)

data$TicketSize <- case_when(ticketCnt == 1 ~ "Single",
                             ticketCnt >= 2 & ticketCnt < 5 ~ "Small",
                             ticketCnt >= 5 ~ "Big")
data$TicketSize <- factor(data$TicketSize, levels = c("Single", "Small", "Big"))
rm(ticketCnt)
rm(ticket_kwd)
rm(kwd)
rm(idx)
rm(i)


### (3) 나이에 대한 그룹 생성
### - 앞서 수행한 가족 구성원 수에 대한 그룹을 만든 것과 유사하게 수치형 변수인 
###   나이에 대해서도 그룹으로 나눠보자
### - 외국의 경우임을 감안해 그룹은 13세 미만을 kid, 13~17세를 teenage, 18~59세를 adult, 60세 이상은 elder 로 
###   구분하며, Factor 형으로 선언한다.
data$AgeClass <- case_when(data$Age < 13 ~ "kid",
                           data$Age >= 13 & data$Age < 18 ~ "teenage",
                           data$Age >= 18 & data$Age < 60 ~ "adult",
                           data$Age >= 60 ~ "elder")
data$AgeClass <- factor(data$AgeClass, levels = c("kid", "teenage", "adult", "elder"))
str(data)


### (4) 성별 및 직급인 단어만 추출하기
### - 앞서 본 EDA 에서 성별 중 여성의 경우가 남성의 경우보다 생존율이 높았다는 것과, 
###   이름에 대한 내용 중 Mr., Ms., Captain 등 성별 및 직급에 대한 단어가 포함되어 있다는 것을 확인했다.
passenger_name <- data$Name
passenger_name
name_keyword <- gsub("^.*, (.*?)\\..*$", "\\1", passenger_name)
# \\1 ~ \\9: BackReference(역참조)를 의미하며, 정규표현식에서 그룹이 존재하는 경우 해당 그룹에 대해 일치하는 
#            문자열 일부를 의미한다. 
# 위의 예시에서 그룹은 (.*?)이며, 문자열에서 , 와 .사이에 존재하는 문자열을 가리킨다. 
unique(name_keyword)
# "Mr"           "Mrs"          "Miss"         "Master"       "Don"          "Rev"          "Dr"          
# "Mme"          "Ms"           "Major"        "Lady"         "Sir"          "Mlle"         "Col"         
# "Capt"         "the Countess" "Jonkheer"     "Dona" 

# 위의 결과 중에서 하나로 합칠 수 있는 것들은 치환해주는 작업을 수행한다.
name_keyword <- ifelse(name_keyword %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", name_keyword)  # 미혼인 여성(Mlle : Miss 와 동일 / Dona, Lady : 영애 및 귀부인를 호칭함)
name_keyword <- ifelse(name_keyword == "Mme", "Mrs", name_keyword) # 기혼인 여성 (Mme : 부인을 의미)
name_keyword <- ifelse(name_keyword %in% c("Master", "Don", "Rev", "Dr", "Major", "Sir", "Col", "Capt", "the Countess", "Jonkheer"), "Rank", name_keyword) # 직급에 해당하는 호칭칭
unique(name_keyword) # "Mr"   "Mrs"  "Miss" "Rank"
data$name_keyword <- name_keyword
data$name_keyword <- factor(data$name_keyword, levels = c("Mr", "Mrs", "Miss", "Rank"))
str(data)
rm(name_keyword)
rm(passenger_name)

### (5) 추가 작업에 따른 학습용 샘플 수정
train <- data[1:891,]
test <- data[892:1309, ]

### 사용 변수 : Survived, Pclass, Sex, Embarked, FamilySize, TicketSize, AgeClass, name_keyword
### test_sample 의 경우 Survived 는 제외함
train_sample <- train %>% select("Survived", "Pclass", "Sex", "Embarked", "FamilySize", "TicketSize", "AgeClass", "name_keyword")
test_sample <- test %>% select("Pclass", "Sex", "Embarked", "FamilySize", "TicketSize", "AgeClass", "name_keyword")

### 사용모델 : 의사결정나무 (rpart)
model_rpart2 <- rpart(formula = Survived ~ ., data=train_sample)
pred2 <- predict(model_rpart2, newdata = test_sample, type="class")

### (6) submission 생성
submission2 <- data.frame(ID, pred2)
names(submission2) <- c("PassengerId", "Survived")
write.csv(submission2, "Result/titanic/submission2.csv", row.names = F)  # Accuracy : 0.77990
rm(submission2)


## 2) 학습 모델 변경
### 의사결정나무의 단점
### - 노이즈에 취약함

### 사용모델 : RandomForest
### 사용 데이터는 이전과 동일
set.seed(1234)

model_rf <- randomForest(Survived ~ ., data=train_sample, ntree=500, importance=T)
model_rf

### 변수중요도 확인
importance(model_rf) 
varImpPlot(model_rf)

### 예측결과 생성
pred3 <- predict(model_rf, newdata = test_sample, type="class")

### submission 생성
submission3 <- data.frame(ID, pred3)
names(submission3) <- c("PassengerId", "Survived")
write.csv(submission3, "Result/titanic/submission3.csv", row.names = F)  # Accuracy : 0.79904
rm(submission3)



## 3) 모델 파라미터 수정
### 모델 자체에 대한 성능을 향상시키는 방법은 다음과 같다.
### 모델 성능에 대해 영향도가 높은 변수 선정
### 모델의 하이퍼파라미터 최적화

### (1) 모델 성능에 대한 영향도가 높은 변수 선정
### - 마지막으로 생성한 랜덤포레스트 모델의 변수 중요도를 살펴보자
### - 변수가 정확도와 노드 불순도에 얼마나 기여하는 가를 확인
confusionMatrix(model_rf$predicted, train_sample$Survived) # Accuracy : 0.8339
(variable_importance <- importance(model_rf))
varImpPlot(model_rf)

### 확인한 결과 Pclass, TicketSize, name_keyword, Sex 가 중요변수이기 때문에 4개 변수는 반드시 포함되어야한다.

### (2) 모델의 하이퍼파라미터 최적화
### - 최적의 파라미터를 확인하기 위해 교차검증(Cross Validation)을 수행한다.
### - 10-Folds Validation 수행하며 데이터는 7:3 으로 나눈다.
set.seed(719)

# Cross Validation 설정
cv <- cvFolds(NROW(train_sample), K = 10, R = 3)

# 하이퍼파라미터의 조합 생성
grids<-expand.grid(ntree=c(10,100,500,1000,2000),mtry=c(3,4,5))
grids

# 10 Folds Cross Validation 수행
result_cv_rf<- foreach(g=1:NROW(grids), .combine = rbind) %do% {
  foreach(r=1:3, .combine = rbind) %do%   {
    foreach(k=1:10, .combine = rbind) %do%   {
      validation_idx <- cv$subsets[which(cv$which == k),r]
      train <- train_sample[-validation_idx,]
      validation <- train_sample[validation_idx,]
      #Train model
      rf_fix<-randomForest(Survived ~ .,
                           data=train_sample,
                           ntree=grids[g,"ntree"],
                           mtry=grids[g,"mtry"])
      #predict
      predicted<-predict(rf_fix,newdata=validation)
      
      #check performance
      precision<-sum(predicted==validation$Survived) / NROW(predicted)
      return(data.frame(g=g,precision=precision))
    }
  }
}

grid_result <- ddply(result_cv_rf,.(g), summarize,mean_precision=mean(precision))
grid_result <- sqldf("select * from grid_result order by 2 desc")
grid_result

### 확인 결과 15번의 설정이 0.8608531 의 수치로 가장 좋게 나타났다. 
### 그에 해당하는 파라미터는 다음과 같다.
grids[15,]
#   ntree mtry
#15  2000    5

### (3) 튜닝 결과 반영하기
model_rf2 <- randomForest(Survived ~ Pclass + TicketSize + name_keyword + Sex, data=train_sample,
                          ntree=2000, mtry=5, importance=T)
confusionMatrix(train_sample$Survived ,model_rf2$predicted) # Accuracy : 0.8339
pred4 <- predict(model_rf2, newdata = test_sample, type="class")

### (4) submission 생성
submission4 <- data.frame(ID, pred4)
names(submission4) <- c("PassengerId", "Survived")
write.csv(submission4, "Result/titanic/submission4.csv", row.names = F)  # Accuracy : 0.78947
rm(submission4)

### 정확도가 떨어진 이유는 앞선 모델보다 변수의 수가 줄어들었기 때문에 0.01 정도 하락했지만, 
### 4개의 변수를 사용해서 학습시켜도 정확도가 이전과 유사하다는 점에서 선방했다고 볼 수 있다.

### 4) 정리하기
### - train, test 데이터 수정
###   - 사용컬럼 : Survived, Pclass, Sex, Embarked, Fare, FamilySize, TicketSize, AgeClass, name_keyword
train <- data[1:891,]
test <- data[892:1309,]
train_sample <- train %>% select("Survived", "Pclass", "Sex", "Embarked", "FamilySize", "TicketSize", "AgeClass", "name_keyword")
test_sample <- test %>% select("Pclass", "Sex", "Embarked", "FamilySize", "TicketSize", "AgeClass", "name_keyword")

### 하이퍼파라미터 재설정
set.seed(1901)

# Cross Validation 설정
cv <- cvFolds(NROW(train_sample), K = 10, R = 3)

# 하이퍼파라미터의 조합 생성
grids<-expand.grid(ntree=c(10,100,500,1000,2000),mtry=c(3,4,5))
grids

# 10 Folds Cross Validation 수행
result_cv_rf<- foreach(g=1:NROW(grids), .combine = rbind) %do% {
  foreach(r=1:3, .combine = rbind) %do%   {
    foreach(k=1:10, .combine = rbind) %do%   {
      validation_idx <- cv$subsets[which(cv$which == k),r]
      tr <- train_sample[-validation_idx,-c(4,9,11)]
      val <- train_sample[validation_idx,-c(4,9,11)]
      #Train model
      rf_fix<-randomForest(Survived ~ .,
                           data=tr,
                           ntree=grids[g,"ntree"],
                           mtry=grids[g,"mtry"])
      #predict
      predicted<-predict(rf_fix,newdata=val)
      
      #check performance
      precision<-sum(predicted==val$Survived) / NROW(predicted)
      return(data.frame(g=g,precision=precision))
    }
  }
}

grid_result <- ddply(result_cv_rf,.(g), summarize,mean_precision=mean(precision))
grid_result <- sqldf("select * from grid_result order by 2 desc")
grid_result

grids[grid_result[1,"g"],]

### - 하이퍼파라미터는 model_rf2와 동일하며, 사용변수는 train_sample의 모든 변수 사용
model_rf3 <- randomForest(Survived ~., data=train_sample, ntree=grids[grid_result[1,"g"],][,1], mtry=grids[grid_result[1,"g"],][,2], importance=T)
confusionMatrix(train_sample$Survived ,model_rf3$predicted) # Accuracy : 0.8339
pred5 <- predict(model_rf3, newdata = test, type = "class")

submission5 <- data.frame(ID, pred5)
names(submission5) <- c("PassengerId", "Survived")
submission5
write.csv(submission5, "Result/titanic/submission5.csv", row.names = F)  # Accuracy : 
rm(submission5)



#####################################################
### child & mother 변수 생성
### EDA 에서 영유아의 생존 비율이 높았고, 여성의 비율이 높기 때문에,
### 아이의 생존율이 높다면 엄마의 생존율도 높을 것이라고 판단

data$Child[data$Age < 18] <- 'Child'
data$Child[data$Age >= 18] <- 'Adult'
data$Child <- factor(data$Child)
str(data)

data$Mother <- 'Not Mother'
data$Mother[data$Sex == 'female' & data$Parch > 0 & data$Age > 18 & data$name_keyword != 'Miss'] <- 'Mother'
data$Mother <- factor(data$Mother)
str(data)

train <- data[1:891,]
test <- data[892:1309,]

model_rf6 <- randomForest(factor(Survived) ~ Pclass + Sex + 
                           Embarked + name_keyword + TicketSize + AgeClass +
                           FamilySize + Child + Mother,
                         data = train)
confusionMatrix(train$Survived ,model_rf6$predicted)
pred6 <- predict(model_rf6, newdata = test, type="class")

submission6 <- data.frame(ID, pred6)
names(submission6) <- c("PassengerId", "Survived")
submission6
write.csv(submission6, "Result/titanic/submission6.csv", row.names = F)  # Accuracy : 0.80861
rm(submission6)

####################################################################################

train_sample <- train %>% select("Survived", "Pclass", "Sex", "Embarked", "name_keyword", "TicketSize", "AgeClass", "FamilySize", "Child", "Mother")
test_sample <- train %>% select("Pclass", "Sex", "Embarked", "name_keyword", "TicketSize", "AgeClass", "FamilySize", "Child", "Mother")

grids<-expand.grid(ntree=c(10,100,500,1000,2000),mtry=c(3,4,5))
grids

# 10 Folds Cross Validation 수행
result_cv_rf<- foreach(g=1:NROW(grids), .combine = rbind) %do% {
  foreach(r=1:3, .combine = rbind) %do%   {
    foreach(k=1:10, .combine = rbind) %do%   {
      validation_idx <- cv$subsets[which(cv$which == k),r]
      tr <- train_sample[-validation_idx,]
      val <- train_sample[validation_idx,]
      #Train model
      rf_fix<-randomForest(Survived ~ .,
                           data=tr,
                           ntree=grids[g,"ntree"],
                           mtry=grids[g,"mtry"])
      #predict
      predicted<-predict(rf_fix,newdata=val)
      
      #check performance
      precision<-sum(predicted==val$Survived) / NROW(predicted)
      return(data.frame(g=g,precision=precision))
    }
  }
}

grid_result <- ddply(result_cv_rf,.(g), summarize,mean_precision=mean(precision))
grid_result <- sqldf("select * from grid_result order by 2 desc")
grid_result

grids[grid_result[1,"g"],]

model_rf7 <- randomForest(factor(Survived) ~ .,
                          data = train_sample,
                          ntree=grids[grid_result[1,"g"],][,1], 
                          mtry=grids[grid_result[1,"g"],][,2],
                          importance=T)
confusionMatrix(train$Survived ,model_rf7$predicted)
pred7 <- predict(model_rf7, newdata = test, type="class")

submission7 <- data.frame(ID, pred7)
names(submission7) <- c("PassengerId", "Survived")
submission7
write.csv(submission7, "Result/titanic/submission7.csv", row.names = F)  # Accuracy : 
rm(submission7)