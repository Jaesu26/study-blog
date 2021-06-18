data1=c(569, 416, 422, 565, 484, 520, 573, 518, 501, 505, 468, 382, 310,
        334, 359, 372, 439, 446, 349, 395, 461, 511, 583, 590, 620, 578,
        534, 631, 600, 438, 516, 534, 467, 457, 392, 467, 500, 493, 410,
        412, 416, 403, 422, 459, 467, 512, 534, 552, 545)
 

##3RSSH, twice 평활기법

A3RSSH2=function(x){
  n=length(x)
  x3rss = smooth(x, kind="3RSS")
  x3rssh <- vector("numeric", n)
  x3rssh2 <- vector("numeric", n)
  for (i in 2:(n-1)) x3rssh[i] <- x3rss[i-1]/4 + x3rss[i]/2 + x3rss[i+1]/4
  x3rssh[1] <- x3rss[1]; x3rssh[n] <- x3rss[n]
  rough=x-x3rssh
  
  x3rss2=smooth( rough, kind="3RSS")
  
  for (i in 2:(n-1)) x3rssh2[i] <- x3rss2[i-1]/4 + x3rss2[i]/2 + x3rss2[i+1]/4
  x3rssh2[1] <- x3rss2[1]; x3rssh2[n] <- x3rss2[n]
  end=x3rssh+x3rssh2
  return(end)
}

data_3rssH_T=A3RSSH2(data1) ##데이터 3RSSH, twice 복합평활

plot(data1, type= "l", lty ="dotted" , ylim = c(300, 700)) ; par(new = T)

plot(data_3rssH_T, type = "l" , col = "blue" , ylim= c(300, 700), ylab= "", main = "3RSSH, twice")


##4235H, twice 시계열 그래프

data_4235H_T = sleek(data1)

plot(data1, type= "l", lty ="dotted" , ylim = c(300, 700)) ; par(new = T)

plot(data_4235H_T, type = "l" , col = "red" , ylim= c(300, 700), ylab= "", main = "4235H, twice")

##2020년 삼성전자 월평균 종가

 data = read.csv("C:/Users/한재수/Downloads/data_1341.csv")
 data = data$종가
 data = rev(data)
 
 plot(data, type = "l" , ylim = c(46000, 83000), ylab = "" )
 
 par(new = T)
 
 ##4235H, twice 평활 기법
 data1=sleek(data)
 plot(data1, type = "l", main = "2020년 삼성전자 종가", col = "blue")
 
 ##3RSSH, twice 평활 기법
 data2=A3RSSH2(data)
 plot(data2, type = "l", main = "2020년 삼성전자 종가", col = "red" , ylab = "종가" , ylim = c(46000, 83000))


 ####10주차 EDA
 
 set.seed(2021)
 
 x = seq(1,10)
 y = 2.5 + 0.5 * x + rnorm(10,0,1)
 y[10] = -100
 y
 
 plot(y~x, ylim=c(-20,10))
 library(MASS)
 
 ##LSM추정량
 
 LSM = lm(y~x)
 deviance(LSM)
 
 ##M추정량
 M = rlm(y~x)
 deviance(M)
 
 ##LMS추정량
 LMS = lqs(y~x , method = "lms")
 deviance_LMS = sum((LSM$residuals) ^ 2)
 
 ##LTS default추정량
 LTS1 = lqs(y~x , method = "lts")
 deviance_LTS1 = sum((LTS1$residuals) ^ 2)

 ##LTS 75%추정량
 LTS2 = lqs(y~x , method = "lts", quantile = 7.5)
 deviance_LTS2 = sum((LTS2$residuals) ^ 2)
   
   
   
   
