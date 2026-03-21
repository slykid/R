# 1. 간단한 시계열 데이터
help(ts)

ts(1:10, frequency = 4, start=c(2021, 2))
# 값의 범위: 1 ~ 10
# frequency: 주기
# start: 시작 시기 (2021년 2분기)

#      Qtr1 Qtr2 Qtr3 Qtr4
# 2021         1    2    3
# 2022    4    5    6    7
# 2023    8    9   10

dd <- matrix(c(1342, 1442, 1252, 1343,
               1425, 1364, 1256, 1272,
               1432, 1523, 1623, 1156,
               1265, 1734, 1923, 1045))

dd.ts <- ts(data=dd, start=c(2016, 1), frequency=4)
dd.ts
#      Qtr1 Qtr2 Qtr3 Qtr4
# 2016 1342 1442 1252 1343
# 2017 1425 1364 1256 1272
# 2018 1432 1523 1623 1156
# 2019 1265 1734 1923 1045

# 2. 시계열데이터의 주요 형태
## 2-1. 우연변동 시계열
random1 = matrix(c(1342, 1442, 1252, 1343,
                   1425, 1364, 1256, 1272,
                   1432, 1523, 1623, 1156,
                   1265, 1734, 1923, 1045))
random1.ts = ts(data=random1, start=c(2016, 1), frequency=4)
random1.ts
plot(random1.ts, main='Random Variation Time Series')

## 2-2. 계절변동(Seasonality) 시계열
season1.ts = ts(data=season1 <- matrix(c(1142, 1242, 1452, 1543,
                                         1125, 1262, 1456, 1572,
                                         1143, 1269, 1462, 1553,
                                         1121, 1258, 1472, 1546,
                                         1154, 1249, 1477, 1548)), start=c(2016, 1), frequency=4)
season1.ts
plot(season1.ts, main='Seasonal Variation Time Series')


## 2-3. 추세변동 시계열
## - high/low point 들 간의 패턴을 확인
trend1.ts <- ts(trend1 <- c(1342, 1442, 1252, 1343,
                            1425, 1364, 1256, 1272,
                            1343, 1459, 1412, 1453,
                            1432, 1523, 1623, 1156,
                            1265, 1734, 1923, 1045), start=c(2016, 1), frequency=4)
plot(trend1.ts, main='Trend Variation Time Series')

## 2-4. 계절적 추세변동 시계열
st1.ts <- ts(data=st1 <- c(1142, 1242, 1452, 1543,
                           1225, 1362, 1556, 1672,
                           1343, 1459, 1662, 1753,
                           1421, 1558, 1772, 1846,
                           1554, 1649, 1877, 1948), c(2016, 1), frequency=4)
plot(st1.ts, main='Season-Trend Variation Time Series')

par(mfrow=c(4, 1))
plot(random1.ts, main='Random Variation Time Series')
plot(season1.ts, main='Seasonal Variation Time Series')
plot(trend1.ts, main='Trend Variation Time Series')
plot(st1.ts, main='Season-Trend Variation Time Series')


# 3. 시계열 데이터 시각화
## 3-1. Month Plot
par(mfrow=c(1, 1))
monthplot(random1.ts, main='EDA: Random Variation Series', xlab="Quarter: 2016-2020", ylab="Sales")
monthplot(trend1.ts, main='EDA: Trend Variation Series', xlab="Quarter: 2016-2020", ylab="Sales")

monthplot(season1.ts, main="EDA: Seasonal Variation Series", xlab="Quarter: 2016-2020", ylab="Sales")
monthplot(st1.ts, main="EDA: Seasonal-Trend Variation Series", xlab="Quarter: 2016-2020", ylab="Sales")

## 3-2. 계절성을 제외한 시각화
# install.packages('TSA', 'forecast')
library(TSA)
library(forecast)

data(airpass)
plot(airpass, amin='Air Passengers -- Seasonal Adjustment')
lines(seasadj(decompose(airpass)), col=2, lwd=2)

data(milk)
plot(milk, main='Milk Sales -- Seasonal Adjustment')
lines(seasadj(decompose(milk)), col=2, lwd=2)


## 3-3. Seasonal Plot
seasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)
ggseasonplot(AirPassengers, year.labels=TRUE, continuous=TRUE)

seasonplot(milk, col=rainbow(12), year.labels=TRUE)
ggseasonplot(milk, year.labels=TRUE, continuous=TRUE)


# 4. 차분(Difference)
