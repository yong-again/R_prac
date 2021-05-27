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
