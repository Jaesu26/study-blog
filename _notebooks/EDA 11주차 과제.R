
## 데이터셋
Data = read.csv("C:/Users/한재수/Downloads/EDAdata.csv")
library(MASS)

## 선형회귀모형
m1 <- lm(y~., data = Data)
m2 <- rlm(y~., data = Data)
m3 <- lqs(y~., data = Data, method = "lms")
m4 <- lqs(y~., data = Data, method = "lts", quantile=8)


## LSM의 잔차와 적합치 plot

plot(m1$residuals~m1$fitted.values, pch=16, xlab = "적합치", ylab = "잔차", main = "LSM의 잔차와 적합치 plot")
abline(h=0, col = "red")

## M의 잔차와 적합치 plot

plot(m2$residuals~m2$fitted.values, pch=16, xlab = "적합치", ylab = "잔차", main = "M의 잔차와 적합치 plot")
abline(h=0, col = "red")

## LMS의 잔차와 적합치 plot

plot(m3$residuals~m3$fitted.values, pch=16, xlab = "적합치", ylab = "잔차", main = "LMS의 잔차와 적합치 plot")
abline(h=0, col = "red")

## LTS의 잔차와 적합치 plot

plot(m4$residuals~m4$fitted.values, pch=16, xlab = "적합치", ylab = "잔차", main = "LTS의 잔차와 적합치 plot")
abline(h=0, col = "red")

## m1,m2,m3,m4 잔차의 상자그림

boxplot(m1$residuals, m2$residuals, m3$residuals, m4$residuals, ylab = "residuals", 
        xlab = "LSM-M-LMS-LTS REgression", main = "m1,m2,m3,m4 잔차의 상자그림")

## LTS회귀에 기반한 특이점 제거

Data[abs(m4$residuals) >= 10,]

Data=Data[abs(m4$residuals) < 10,]

m5 <- lm(y~., data = Data)
m6 <- rlm(y~., data = Data)

## 특이점 제거 후 LSM,M모델 분석

summary(m5)

summary(m6)

