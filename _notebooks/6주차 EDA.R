
##6주차 과제

##1번
set.seed(26)
chiq3=rchisq(100,3)
qqnorm(chiq3)
qqline(chiq3, lwd=1, col="red")
## 데이터와 분포를 비교해 이론적으로 성립해야 하는 직선 관계를 그린다.
## qqline은 최소제곱선인가?
## qqline은 Q-Q plot의 Q1, Q3를 이어주는 직선이랍니다.

#2번
light_speed= morley$Speed
 qqnorm(light_speed)
 qqline(light_speed, col="red")
 shapiro.test(light_speed)
 
 ##3번
 qqnorm(tree_volume)
 qqline(tree_volume, col="red")
 shapiro.test(tree_volume)
 
 hist(tree_volume, breaks = 12)
 hist(log(tree_volume), breaks = 12)

 qqnorm(log(tree_volume), main=" 로그 변환 후 Q-Q plot ") 
 qqline(log(tree_volume), col= " red ")
 shapiro.test( log(tree_volume))
 
 ##4번
 data=c( 7.19, 6.31, 5.89, 4.5, 3.77, 4.25, 5.19, 5.79, 6.79)
  qqnorm(data)
  qqline(data, col= " red ")
  shapiro.test(data)
 