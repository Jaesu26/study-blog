
## 회귀식

set.seed(2021)

x = seq(1,10)
y = 2.5 + 0.5 * x + rnorm(10,0,1)
y[5] = -20
y

plot(y~x, ylim = c(-20,10))
library(MASS)

## LSM추정량

LSM = lm(y~x)
deviance(LSM)
plot(y~x, ylim = c(-20,10), main = "LSM추정량")
abline(LSM, col = "red")

## M추정량

M = rlm(y~x)
deviance(M)

plot(y~x, ylim = c(-20,10), main = "M추정량")
abline(M, col = "red")

## LMS추정량

LMS = lqs(y~x , method = "lms")
deviance_LMS = sum((LSM$residuals) ^ 2); deviance_LMS

plot(y~x, ylim = c(-20,10), main = "LMS추정량")
abline(LMS, col = "red")

## LTS default추정량

LTS1 = lqs(y~x , method = "lts")
deviance_LTS1 = sum((LTS1$residuals) ^ 2); deviance_LTS1

plot(y~x, ylim = c(-20,10), main = "LTS default추정량")
abline(LTS1, col = "red")

## 8개 자료를 이용한 LTS 추정량

LTS2 = lqs(y~x , method = "lts", quantile = 8)
deviance_LTS2 = sum((LTS2$residuals) ^ 2); deviance_LTS2

plot(y~x, ylim = c(-20,10), main = "8개 자료를 이용한 LTS 추정량")
abline(LTS2, col = "red")



