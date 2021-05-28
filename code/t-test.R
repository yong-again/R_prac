?t.test
install.packages("pwr")

library(pwr)
?pwr.t.test

install.packages("moonBook")


library(moonBook)
library(ggplot2)
library(dplyr)
library(plyr)
?acs

head(acs)
str(acs)

## 가설설정

### 두 집단(남성과 여성)의 나이 차이를 알고 싶다.
#### 귀무가설: 남성과 여성의 평균 나이에 대해 차이가 없다 .
#### 대립가설: 남성과 여성의 평균 나이에 대해 차이가 있다.


mean.male <- mean(acs$age[acs$sex == "Male"])
mean.female <- mean(acs$age[acs$sex == "Female"])

cat(mean.male, mean.female)

# 정규분포 테스트
## ggplot 을 쓸 경우 데이터 가공이 필요
## moonBook 패키지에 손쉽게 그래프를 도출할 수 있는 함수 내재

moonBook::densityplot(age ~ sex, data=acs)

# 귀무가설: 정규분포가 맞다.(p-value > 0.05)
# 대립가설: 정규분포가 아니다.

#  남성의 정규분포
shapiro.test(acs$age[acs$sex == "Male"])

# 여성의 정규분포
shapiro.test(acs$age[acs$sex == "Female"])

# 등분산 테스트
var.test(age ~ sex, data=acs)

# 정규분포 X -> MWW 검정
wilcox.test(age~ sex, data=acs)
## 대립가설채택


# t-test 방식
t.test(age ~ sex, data=acs, var.test=T, alt= "two.sided")

# welch's test
t.test(age ~ sex, data=acs, var.test=F, alt="two.sided")





# dummy : 0은 군을 나타내고, 1은 시를 나타냄
# 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보려고 한다.
# 귀무가설: 차이가 없다.
# 대립가설: 차이가 있다.

mydata <- read.csv("C:/r_wrok/data/independent.csv")
View(mydata)

mean.dummy0 <- mean(mydata$birth_rate[mydata$dummy==0])
mean.dummy0

mean.dummy1 <- mean(mydata$birth_rate[mydata$dummy==1])
mean.dummy1

cat(mean.dummy0, mean.dummy1)

# 정규분포여부
shapiro.test(mydata$birth_rate[mydata$dummy==0])
shapiro.test(mydata$birth_rate[mydata$dummy==1])

# dummy0: p-value = 0.009702
# dummy1: p-value = 0.001476

# MWW
wilcox.test(birth_rate ~ dummy, data=mydata)
# p-value = 0.04152

# t-test
t.test(birth_rate ~ dummy, data=mydata)

# 결론: 시와 군에서의 출산율의 차이는 유의미한 결과를 보인다.




str(mtcars)
head(mtcars)

# am : 0 = auto, 1 = manual
# mpg : 연비
# 오토나 수동에 따른 연비 차이 
# 귀무가설: 오토의 수동에 따라 연비는 같다.
# 대립가설: 오토와 수동에 따라 연비는 다르다.

mean.auto <- mean(mtcars$mpg[mtcars$am == 0])
mean.m <- mean(mtcars$mpg[mtcars$am == 1])

cat(mean.auto, mean.m)

# 정규분포여부
shapiro.test(mtcars$mpg[mtcars$am == 0]) # p-value = 0,8987
shapiro.test(mtcars$mpg[mtcars$am == 1]) # p-value = 0.5363

# 등분산 여부 
var.test(mpg ~ am, data=mtcars) # p-value = 0.06691
var.test(mtcars[mtcars$am==1, 1], mtcars[mtcars$am==0, 1])

# t- test 
t.test(mpg ~ am, data=mtcars, var.test=T, alt='less')
#  p-value = 0.0006868

# 결론: auto 와 manual에 따른 연비차이가 있음
