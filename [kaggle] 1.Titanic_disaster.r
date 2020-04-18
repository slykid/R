# 참고자료
# - https://www.kaggle.com/redhorse93/r-titanic-data
# - https://velog.io/@suzin/R-%EB%8D%B0%EC%9D%B4%ED%84%B0-%ED%83%90%EC%83%89-3.-Missing-Value%EA%B2%B0%EC%B8%A1%EC%B9%98-NA
# - https://wsyang.com/2014/02/introduction-to-dplyr/

# 사용할 라이브러리
#install.packages("dplyr","ggplot2","naniar", "VIM","sqldf")

library(dplyr)
library(ggplot2)
library(naniar)
library(VIM)
library(sqldf)

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


# 3. EDA(탐색적 데이터 분석)
str(data)
names(data)

## 1) 결측지 여부 확인
gg_miss_var(data[,-(which(names(data)=='Survived'))])
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



