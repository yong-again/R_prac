library(moonBook)
View(acs)

# LDLC : 저밀도 콜레스테롤 수치: 종속변수
# Dx(진단 결과): STEMI(급성심근경색), NSTEMI(만성심근경색), unstable angina(협심증) : 독립변수

moonBook::densityplot(LDLC ~ DX, data=acs)

# 정규분포
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"]))
#W = 0.89996, p-value = 1.56e-08

with(acs, shapiro.test(LDLC[Dx=="STEMI"]))
# W = 0.99574, p-value = 0.6066

with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"]))
# W = 0.96889, p-value = 2.136e-07

# ANOVA 함수
# reidual: 잔차=평균으로부터 얼마나 떨어져 있는지 나타내는 값
out = aov(LDLC ~ Dx, data=acs)
out

shapiro.test(resid(out))
# W = 0.97137, p-value = 1.024e-11

# 등분산
bartlett.test(LDLC ~ Dx, data=acs)
# Bartlett's K-squared = 3.3668, df = 2, p-value = 0.1857

# 정규분포이고 등분산일 경우
out = aov(LDLC ~ Dx, data=acs)
summary(out)

# 연속변수가 아니거나 정규분포가 아닌 경우
kruskal.test(LDLC ~ Dx, data=acs)

# 등분산이 아닐경우
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)

# 사후 검정
## aov() -> TukeyHSD()
TukeyHSD(out)


## krusal.test()
install.packages("pgirmess")
library(pgirmess)

kruskal.test(acs$LDLC, acs$Dx)

##########################################

str(InsectSprays)
View(InsectSprays)

moonBook::densityplot(count ~ spray, data=InsectSprays)


#########################

head(iris)

# 품종별로 Sepal.Width의 평균 차이가 있는가? 만약 있다면
# 어느 품종과 차이가 있을까?

moonBook::densityplot(Sepal.Length ~ Species, data=iris)

# long 형으로 변환
a_iris <- gather(iris, key="GROUP", value="width",
                 -c(Sepal.Length, Petal.Length, Petal.Width, Species))
# library(tidyr)

a_iris

# 정규분포 여부
with(a_iris, shapiro.test(width[Species == "setosa"]))
# p-value = 0.2715

with(a_iris, shapiro.test(width[Species == "versicolor"]))
# p-value = 0.338

with(a_iris, shapiro.test(width[Species == "virginica"]))
# p-value = 0.1809

# 등분산 여부
bartlett.test(a_iris$width ~ a_iris$Species)
# p-value = 0.3515

# ANOVA 검정
out_iris = aov(width ~ Species, data=a_iris)
summary(out_iris)

# 결론
# 품종별로 Sepal.Width의 평균차이가 존재하고
# virginica 와 setosa, versicolor 와 setosa간에 평균차이가 존재 한다.