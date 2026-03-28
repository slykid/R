install.packages("readxl")

library(readxl)
library(ggplot2)

# Q1
portfolio <- read_xls(path="Data/TimeSeries/Portfolio_hw.xls", skip=2)

## 1.1 This Year와 Last Year의 상관계수(r)를 구하시오.
(r <- cor(portfolio$`Last Year`, portfolio$`This Year`))  # 0.5313237

## 1.2 위에서 구한 상관계수(r)이 통계적으로 유의한지를 유의수준 1%에서 p-value를 이용해 검정하시오.
(cor.test(portfolio$`Last Year`, portfolio$`This Year`, conf.level = 0.99, alternative = "two.sided"))
# Pearson's product-moment correlation
# 
# data:  portfolio$`Last Year` and portfolio$`This Year`
# t = 2.429, df = 15, p-value = 0.02818
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#  -0.09613377  0.85659334
# sample estimates:
#       cor 
# 0.5313237  

## 모집단의 상관계수에 대해, 귀무가설은 "모집단의 상관계수는 0이다.", 대립가설은 "모집단의 상관계수는 0이 아니다." 로 정의가 되며,
## 위의 검정결과를 볼 때, 유의수준 1%에 대해 p-value 값은 0.02818로 0.01 보다 큰 값이다. 
## 따라서, 귀무가설이 채택되므로, 모집단의 상관계수는 0이므로 두 변수는 서로 선형관계가 없다.


# Q2
bodyfat <- read_xls(path="Data/TimeSeries/BodyFat_hw.xls")

## 2.1 산포도를 그리시오 (X: Girth, Y: Fat)
plot(x=bodyfat$Girth, y=bodyfat$Fat, col="blue", pch=19, xlab="Girth", ylab="Fat")

## 2.2 ~ 2.9
model <- lm(Fat ~ Girth, data=bodyfat)
(summary(model))

# Call:
#   lm(formula = Fat ~ Girth, data = bodyfat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.9392 -3.8714 -0.0414  3.6328  9.8672 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -36.23970    5.66898  -6.393 6.28e-08 ***
#   Girth         0.59053    0.05975   9.883 3.71e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.086 on 48 degrees of freedom
# Multiple R-squared:  0.6705,	Adjusted R-squared:  0.6636,  
# F-statistic: 97.68 on 1 and 48 DF,  p-value: 3.714e-13

## 2.2 표본의 상관계수와 결정계수를 구하시오.
## 결정계수: 0.6705(수정된 값: 0.6636), 상관계수: sqrt(결정계수) = 0.8188406

## 2.3 상관계수와 결정계수의 관계를 설명하시오.
##

## 2.4. 절편과 기울기의 OLS 추정값을 적으시오.
## Intercept: -36.23970, beta1_hat = 0.59053

## 2.5 추정한 기울기 값의 의미를 적으시오.
## Girth 가 한 단위 증가함에 따라 Fat 이 0.59053배 증가한다.

## 2.6 기울기의 유의성을 검정하려고 한다. 이를 위한 귀무가설(H0)와 대립가설(H1)을 적으시오
## H0: Girth가 한 단위 증가함에 따라 Fat은 0.59053배 만큼 증가하지 않는다.
## H1: Girth가 한 단위 증가함에 따라 Fat이 0.59053배 만큼 증가한다.

## 2.7 기울기의 유의성을 유의수준 5%에서 p-value를 이용해 t 검정을 하고 결과를 보고하시오. 
## 모델 해석 결과를 확인한 결과 Girth 변수에 대한 t검정의 p-value 값은 3.71e-13이며, 0.001보다 작은 값이다. 
## 이는 유의수준 5% 미만의 값이기 때문에, 대립가설을 채택하게 되므로, 기울기는 유의미하다.

## 2.8 F검정에 대한 귀무가설(H0)와 대립가설(H1)을 작성하시오.
## H0: 모든 변수들은 Fat에 대해 무의미하다.
## H1: 적어도 하나의 변수는 Fat에 대해 유의미한 영향을 준다.

## 2.9 F검정을 유의수준 5%에서 p-value를 이용해 검정하고 결과를 보고하시오. 
## 위의 모델해석 결과에서 나온 p-value 값은 3.714e-13 이며, 이는 유의수준 5% 미만의 값이기 때문에 
## 대립가설을 채택하게 되고, 적어도 하나의 변수는 Fat에 대해 유의미한 영향을 준다.

## 2.10 잔차(residual) 그림을 그리시오.
par(mfrow=c(2,2))
plot(model)

## 2.11 Leverage point를 이용하여 이상치가 있는지 확인해보고 있다면 몇 번째 값인지 보고하시오.
par(mfrow=c(1,1))
plot(model, which=5)

leverage <- hatvalues(model)
sort(leverage, decreasing=TRUE)[1:5]
threshold <- 2 * (length(coef(model)) / nrow(bodyfat))
which(leverage > threshold)

## 5, 45, 50번째 값이 이상치에 해당한다.

## 2.12 studentized deleted residuals을 이용하여 이상치가 있는지 확인하고, 있다면 몇 번째 값인지 보고하시오.
stu_del_residual <- rstudent(model)
outliers <- which(abs(stu_del_residual) > 3)
outliers

