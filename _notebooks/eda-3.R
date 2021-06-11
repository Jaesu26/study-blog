
###eda4주차 과제
 

##첨도와 문자값전시
numstr=function(x){
  x1=quantile(x,c(15/16, 7/8, 3/4, 1/2, 1/4, 1/8, 1/16), type = 8)
  mid=x1[4]
  sprE=round(x1[2]-x1[6],3)
  sprH=round(x1[3]-x1[5],3)
  sprD=round(x1[1]-x1[7],3)
  kurtoE_H=((sprE/sprH)-1.704)
  kurtoD_H=((sprD/sprH)-2.274)
  비고=c('M','H','E','D','two_tail');아래경첩=c(round(c(mid, x1[5], x1[6], x1[7], min(x)), 3))
  위경첩=c(round(c(mid, x1[3], x1[2], x1[1], max(x)), 3))
  중앙mid=c(round(c(x1[4], (x1[3]+x1[5])/2, (x1[2]+x1[6])/2, (x1[1]+x1[7])/2, (max(x)+min(x))/2), 3))
  spr=c('*', sprH, sprE, sprD, round(max(x)-min(x), 3))
  data5=data.frame(비고, 아래경첩, 위경첩, 중앙mid, spr)
  print(c(kurtoE_H=kurtoE_H, kurtoD_H=kurtoD_H))
  return(data5)
}


set.seed(26)##seed넘버
x=rnorm(100)##표준정규분포
hist(x, main = "표준정규분포")
numstr(x)##표준정규분포 문자값전시 

set.seed(26) ##seed넘버
y=rchisq(100,2)##자유도2인 카이제곱분포
hist(y, main = "자유도2인 카이제곱분포")
numstr(y)##자유도2 카이제곱분포 문자값 전시

 ##beta(10,2)분포
set.seed(26)##seed넘버
z=rbeta(100,10,2)
hist(z, main = "beta(10,2)분포")
numstr(z)

##표본의 이상점 구하기
  outliernumber=function(x){
    x2=quantile(x,c(3/4,1/4),type = 8)
    sprH=x2[1]-x2[2]
    IF_L=x2[2]-1.5*sprH
    IF_U=x2[1]+1.5*sprH
    outlier=x[x>IF_U|x<IF_L]
  return(length(outlier))
    }
    
  
  ##균일분포
  set.seed(26)
  u=runif(100,0,1)
  hist(u,main = "균일분포(0,1)")
  outliernumber(u)
  
  ##이중지수분포,평균이0 람다가1
  library(nimble)
  set.seed(26)
  dexp1=rdexp(100)
  hist(dexp1, main = "u=0, lamda=1인 이중 지수분포")
  outliernumber(dexp1)
##length는 변수값 개수 반환, nchar는 변수 길이 반환
  
  ##전라북도 시와군에 대한 박스플랏
  ##주소:https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1YL20651E&conn_path=I2
  JBpop=read.csv("C:/Users/한재수/Downloads/jbpop1.csv") ;JBpop
  ##첫번째 행에 불필요한 문자X제거##이렇게 가공 안할거임
names(JBpop)[2:30]=substr(names(JBpop)[2:30],2,nchar(names(JBpop)[2:30])) ;JBpop
##연도별로 봄
boxplot(JBpop[,c(2:30)])
##새로운 시작
##바로 망한듯 내가 한건 시계열자료고 익산시 부속 읍면동을 다 합쳐서 상자그림 그려야 하는듯
##1992~2020까지의 평균 인구 분포를 알고 싶은걸로 생각하면 괜찮지 않을까?응 아니야
##자료가 없어
##아니면 연도별로 볼까?
 
year=paste(1992:2020,"년")
시군=c("전주시","군산시", "익산시", "정읍시", "남원시", "김제시","완주군", "진안군", "무주군", "장수군", "임실군" ,"순창군" ,"고창군", "부안군")
colnames(JBpop)=NULL
rownames(JBpop)=NULL
JBpop=JBpop[,-1]
colnames(JBpop)=year
rownames(JBpop)=시군
JBpop=t(JBpop)
JBpop=as.data.frame(JBpop)
 
boxplot(JBpop[,c(1:6)]) ##시별 인구 비교 

boxplot(JBpop[,c(7:14)]) ##군별 인구 비교

boxplot(JBpop[,c(1:14)],main=c("전라북도 시와 군의 인구 상자그림")) ##시와 군 따로따로 보면 안돼. 같이 봐야 돼

##아시아,유럽,라틴아메리카,북아메리카,오세아니아,아프리카 각 나라들의 수도에 대한 인구 박스플랏.
##라틴아메리카와 북아메리카는 아메리카로 통합함.

##나라별 수도 https://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_2KAA203&conn_path=I2

capital=read.csv("C:/Users/한재수/Downloads/capital.csv");capital
continent1=rep(c('asia','N_america','latin_America','europe','afica','oceania'),c(30,3,18,23,23,2))

capital=cbind(continent1,capital)      

## 이건 낭비임.names(capital)[3]=substr(names(capital)[3],2,nchar(capital))
names(capital)[3]='2020년'
library(ggplot2); library(dplyr) ## 패키지 설치
capital%>%ggplot(aes(continent1,`2020년`))+ geom_boxplot() ##대륙별 인구 상자그림
 ##아시아 인구
capital%>%filter(continent1=='asia')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##남아메리카 인구
capital%>%filter(continent1=='N_america')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##아프리카 인구
capital%>%filter(continent1=='afica')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##유럽 인구
capital%>%filter(continent1=='europe')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##라틴아메리카 인구
capital%>%filter(continent1=='latin_America')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##오세아니아 인구
capital%>%filter(continent1=='oceania')%>%ggplot(aes(reorder(국가.도시별., `2020년`),`2020년`))+geom_bar(stat = "identity")+coord_flip()

##5주차과제
data=c(778, 355, 248, 200, 167, 94, 94, 88, 76, 75, 74, 74, 70, 68, 63 )

## 문자값 전시 함수

numstr1=function(x){
  x1=quantile(x,c(15/16, 7/8, 3/4, 1/2, 1/4, 1/8, 1/16), type = 8)
  mid=x1[4]
  
  sprE=round(x1[2]-x1[6],3)
  sprH=round(x1[3]-x1[5],3)
  sprD=round(x1[1]-x1[7],3)
   
  비고=c('M','H','E','D','two_tail');아래경첩=c(round(c(mid, x1[5], x1[6], x1[7], min(x)), 3))
  위경첩=c(round(c(mid, x1[3], x1[2], x1[1], max(x)), 3))
  
  중앙mid=c(round(c(x1[4], (x1[3]+x1[5])/2, (x1[2]+x1[6])/2, (x1[1]+x1[7])/2, (max(x)+min(x))/2), 3))
  
  spr=c('*', sprH, sprE, sprD, round(max(x)-min(x), 3))
  data5=data.frame(비고, 아래경첩, 위경첩, 중앙mid, spr)
  
  return(data5)
}

numstr1(data) ##문자값 전시


hist(data, breaks = 10, freq = F) ## 기존 데이터의 히스토그램 
lines(density(data))

##분포를 대칭화 하기 위한 변환의 차수를 구하는 함수
tran= function(x){
  x1= quantile(x, probs = c(1/4, 1/2, 3/4), type = 8)
  H_L= x1[1]
  H_U= x1[3]
  Me= x1[2]
  U=((H_L+ H_U)/2)- Me
  L=((H_L- Me)^2+(H_U- Me)^2)/(4*Me)
  p=1-U/L
  return(p)
}

 tran(data)
hist((data)^-0.5, breaks = seq(0,0.2,0.02),freq = F) ##차수 변환후 히스토그램
 lines(density(data^-0.5))

 ##거리와 이미지크기
 
 dis=seq(12,22,1)
 size=c(12, 9.4, 7.2, 6.2, 5.2, 4.5, 4.0, 3.6, 3.2, 3.0, 2.7)
 
 plot(size~dis, main="기존 데이터 플롯")
 m1=lm(size~dis)
 summary(m1)
 
 
 
 ##역변환
 plot(size^-0.5~dis, main="역변환 후 데이터 플롯")
 m=lm(size^-0.5~dis)
 summary(m)
 
 ##군산과 전주의 작년1월~12월 일산화탄소 데이터
 ##군산
 신풍동=c(0.7,	0.5,	0.4,	0.3,	0.3,	0.3,	0.3,	0.3,	0.5,	0.5,	0.5,	0.5)
 소룡동=c(0.5,	0.4	,0.4,	0.3	,0.3	,0.3	,0.3	,0.3	,0.3	,0.4	,0.4	,0.4)
 비응도=c(0.5,	0.4,	0.4,	0.3,	0.4,	0.4,	0.3,	0.3	,0.4	,0.5	,0.5,	0.5)
 개정동=c(0.6,	0.6,	0.3,	0.3,	0.3,	0.2	,0.3,	0.2,	0.4,	0.5,	0.5	,0.5)
 옥산면=c(0.6,	0.6,	0.4,	0.3,	0.3,	0.3,	0.3,	0.3,	0.4,	0.4	,0.5	,0.5)
 
 ##전주
 노송동=c(0.6,	0.6,	0.4,	0.4,	0.4,	0.4,	0.4,	0.3,	0.5,	0.5,	0.6,	0.6)
 삼천동=c(0.4,	0.3,	0.3	,0.3,	0.3,	0.4,	0.4,	0.3,	0.4,	0.4	,0.4	,0.5)
 서신동=c(0.7	,0.6,	0.6,	0.4,	0.4,	0.3,	0.3,	0.2,	0.4,	0.4,	0.5	,0.4)
 송천동=c(0.5,	0.5,	0.4,	0.3,	0.3,	0.3,	0.3,	0.3,	0.4,	0.4	,0.4,	0.4)
 팔복동=c(0.6	,0.5,	0.6,	0.5,	0.3,	0.2,	0.3,	0.4,	0.3	,0.3,	0.3,	0.6)
 혁신동=c(NA,0.2,	0.4,	0.4,	0.2,	0.3,	0.4,	0.2,	0.4,	0.4,	0.5,	0.4)
 
 gun=data.frame(신풍동,소룡동,비응도,개정동,옥산면)
 gun1=apply(gun,1,mean)
 
 
 jun=data.frame(노송동,삼천동,서신동,송천동,팔복동,혁신동)
 jun1=apply(jun,1,mean,na.rm=T)

 
 boxplot(gun1,jun1,main="전주와 군산 2020년 일산화탄소", names=c("군산","전주"))
 boxplot(sqrt(gun1),sqrt(jun1), main="제곱근 변환 후 전주와 군산2020년 일산화탄소", names=c("군산","전주"))
 boxplot(gun1^(1/3), jun1^(1/3), main="세제곱근 변환 후 전주와 군산 2020년 일산화탄소", names=c("군산","전주"))
 boxplot(gun1^(1/4), jun1^(1/4), main="네제곱근 변환 후 전주와 군산2020년 일산화탄소", names=c("군산","전주"))
 boxplot(log(gun1), log(jun1), main="로그 변환 후 전주와 군산2020년 일산화탄소", names=c("군산","전주"))
 num1= fivenum(jun1)
 num2= fivenum(gun1)
 data= data.frame(num1, num2) ; data
 m1= data[3,1] ; m2=data[3,2]
 medtran= c(log10(m1), log10(m2))
 spr1= data[4,1]-data[2,1] ; spr2= data[4,2]-data[2,2]
 sprtran= c(log10(spr1), log10(spr2))
 out=lm(sprtran~medtran) ; out
 
 
 
 
 
 
   
   
 
 
 