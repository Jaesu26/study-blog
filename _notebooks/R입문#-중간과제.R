Question1-1
set.seed(1)
dice=sample(x=1:6,1000,replace=T);table(dice)

Question1-2
barplot(table(dice))

set.seed(1)
Dice3<-function(x){
n=0
for(i in 1:x){
i=sample(1:6,1)
if(i==3)n<-n+1
}
return(n/x)
}
Dice3(10000)

Question1-3
set.seed(1)
rnorm(500,0,1)
hist(rnorm(500,0,1))

Question2.
matrix01<-function(x){
  M<-apply(x,2,max)
  N<-apply(x,2,min)
  k=M-N
  x.1<-t(apply(x,1,"-",N))
  x.2<-t(apply(x.1,1,"/",k))
  return(x.2)
}
matrix4=matrix(1:16,4,4)
matrix4
matrix01(matrix4)

Question3-1
Car1=tapply(Cars93$Price,Cars93$Origin,mean);Car1
Car2=aggregate(Cars93$Price,list(Cars93$Origin),mean);Car2

Question3-2
summary1=function(x){
  mean.sd=c(m=mean(x), s=sd(x))
  IQR_median=c(IQR=IQR(x), median=median(x))
  list(mean.sd=mean.sd, IQR_median=IQR_median)
}
A=1:100
summary1(A)

Question4
f=c()
f[1]=1
f[2]=1
i=1
repeat{
  if(f[i]+f[i+1]>300)break
  f[i+2]=f[i+1]+f[i] 
  i=i+1
};f

Question5-1
set.seed(5)
position=5
repeat{
  x<-sample(c(0,1),1,prob=c(0.4,0.6))
  print(position)
  if(position<=0||position==10)break
  position=position+ifelse(x==1,1,-2)
}

Question5-2
set.seed(5)
position=5
lose=0;win=0
repeat{
  x<-sample(c(0,1),1,prob=c(0.4,0.6))
  position=position+ifelse(x==1,1,-2)
  if(position<=0){
    lose=lose+1
    position=5
  }
  if(position==10){
    win=win+1
    position=5
  }
  if(lose+win==100)break
}
print(lose/(lose+win));lose;win

Question6-1
xdata <- as.data.frame(Titanic)
sum(xdata$Freq)

Question6-2
tapply(xdata$Freq, xdata$Class, sum)

Question6-3
xdata[xdata$Class=='3rd'&xdata$Age=='Child',]

Question6-4
Yes=subset(xdata, xdata$Survived=='Yes')
tapply(Yes$Freq, Yes$Class, sum)/tapply(xdata$Freq, xdata$Class, sum)
tapply(Yes$Freq, Yes$Age, sum)/tapply(xdata$Freq, xdata$Age, sum)

Question7-1
g=function(x){
  return((1-x)^(1/3))
}
x=c()
x[1]=0.2
i=1
repeat{
  x[i+1]=g(x[i]);print(x[i])
  if((x[i+1]-x[i])<(10^-8)&(x[i+1]-x[i])>(-(10^-8)))break
  i=i+1
}
y=function(z){
  return(z^3+z-1)
}
infinityfx=x[i]
y(infinityfx)

Question7-2
a=0;b=1;i=1;k=c()
repeat{
  c=(a+b)/2;print(c)
  if((b-a)<(10^-8)&(b-a)>(-(10^-8)))break
  if(y(c)*y(a)>0){a<-c}
  if(y(c)*y(b)>0){b<-c}
}
y=function(z){
  return(z^3+z-1)
}
infinityfy=c
y(infinityfy)