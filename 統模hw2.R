library(dplyr)
sim <- function(x) {
  N <- NULL
  for(i in 1:x){
    s <- 0
    j <- 0
    while(s<1){
      s <- sum(s,runif(1))
      j <- j+1
    }
    N <- c(N,j)
  }
  return(N)
}

test <- sapply(list(1000,2000,5000,10000,100000), sim)
m <- sapply(test , mean)
s <- sapply(test , sd)
result <- cbind(m,s) %>% as.data.frame()
colnames(result) <- c("平均數","S.E")
rownames(result) <- paste0(c(1000,2000,5000,10000,100000))

################################################
#2
b <- c(0.252,0.305,0.299,0.303,0.285,0.191,0.283,0.272,0.310,0.266,0.215,0.211,0.244,0.320)
h <- c(12,6,4,15,2,2,16,6,8,10,0,3,6,7)
z <- NULL
for (i in 1:10000) {
  sb <- sample(b,14,F)
  sh <- sample(h,14,F)
  z <- c(z,sum(sb*sh))
}
hist(z,breaks = 17)
p_value <- sum(z>sum(b*h))/10000
p_value*2






#3
install.packages("bootstrap")
library(bootstrap)
truecor=cor(law82[,2],law82[,3])
truecor

###bootstrap function n為replicate幾次 bm為一開始要取幾個樣本
bt <- function(bm){
  sample(82,bm,F)
}
boot <- function(n,m){
  bb <- law82[b,]
  lg <- NULL
  for(i in 1:n){
    a <- sample(m,m,T)
    aa <- bb[a,]
    clg <- cor(aa[,2],aa[,3])
    lg <- c(lg,clg)
  }
  return(lg)
}
bn <- seq(from = 50 , to = 10000 ,by = 50) #從n=50 ~ 10000
bm <- c(10,15,20,25) #樣本數從10 ~ 25
var <- NULL
for (i in bm) {
  v <- NULL
  b <- bt(i)
  for (j in bn) {
    v <- c(v,var(boot(j,i)))
  }
  var <- rbind(var,v)
}
colnames(var) <- paste0("n=",bn)
rownames(var) <- bm
par(mfrow = c(2,2))
plot(var[1,],xlab = "replicate",ylab = "variance",main = "bootstraps of 10");plot(var[2,],xlab = "replicate",ylab = "variance",main = "bootstraps of 15");plot(var[3,],xlab = "replicate",ylab = "variance",main = "bootstraps of 20");plot(var[4,],xlab = "replicate",ylab = "variance",main = "bootstraps of 25")


write.csv(var,"~/Desktop/var.csv",fileEncoding = "big5")
#####################




#4



library(boot)
length(sunspot.year)
#set training data & testing data
train.data=sunspot.year[1:(length(sunspot.year)-9)]
test.data=sunspot.year[(length(sunspot.year)-9):length(sunspot.year)]

#∆q
ran=train.data[-1]-train.data[-length(train.data)]
#10 consecutive numbers
rang=function(m){
  ran[c(m:(m+9))]
}

A1=matrix(0,nrow=1000,ncol=10,byrow=T)
#start block bootstrap 
for(i in 1:1000){
 m=sample(1:269,1)
 A1[i,]=rang(m)
 A=apply(A1,2,sort)
 }
#predict
xx=apply(A,2,median)
z3=c()
for(i in 1:10){
 z3[i]=train.data[279]+sum(xx[i])
 }
z3
# The statistic T
sum(abs(test.data-z3))
#time series(arima)
library(TSA)
library(forecast)
par(mfrow = c(3,1))
ts.plot(train.data)
acf(train.data)
pacf(train.data)

model=auto.arima(train.data)
summary(model)

z.predict=predict(model,n.ahead=10)
sum(abs(c(z.predict$pred)-test.data))

md=c(z.predict$pred)
minz=min(md,test.data);maxz=max(md,test.data)
plot(280:289,test.data,ylim=c(minz,maxz),type="l")
lines(280:289,md,lty=3)#Dotted line is the prediction data,and the solid line is the real data
minz1=min(z3,test.data);maxz1=max(z3,test.data)
plot(280:289,test.data,ylim=c(minz1,maxz1),type="l")
lines(280:289,z3,lty=3)#Dotted line is the prediction data,and the solid line is the real data
























######################
#5
dis <- function(n,m){
  ddd <- NULL
  for(j in 1:n){
  d <- NULL
  dd <- NULL
  x <- runif(m)
  y <- runif(m)
  for (i in 1:m) {
    d <- min(sqrt(c(x[-i]-x[i])^2+c(y[-i]-y[i])^2))
    dd <- c(dd,d)
  }
  ddd <- c(ddd,min(dd))
  }
  return(ddd)
}
a <- list(c())
a <- dis(100,100)
hist(999000*(a^2))
ks.test(999000*(a^2),"pexp",pi/2)







#############################
library(foreach)
library(doSNOW)
library(dplyr)
library(RCurl)
library(tm)
library(tmcn)
library(rvest)
library(magrittr)
library(tidyr)
library(ggplot2)

dist <- function(){
  d <- NULL
  dd <- NULL
  ddd <- NULL
  for(j in 1:1000){
    x <- runif(1000)
    y <- runif(1000)
    for (i in 1:1000) {
      d <- min(sqrt(c(x[-i]-x[i])^2+c(y[-i]-y[i])^2))
      dd <- c(dd,d)
    }
    ddd <- c(ddd,min(dd))
  }
  return(ddd)
}
# 
# time2<-c()
# for(i in 1:1){
#   time2[i]<-system.time({cl<-makeCluster(4) 
#   registerDoSNOW(cl)
#   final2<-foreach(j=1:1,.export = "dist") %dopar% {dist()}
#   stopCluster(cl)}) 
# }
# hist(final2[[1]])
# time2






