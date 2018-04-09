library(dplyr)
#set.seed
y <- 49463089
y <- as.numeric(substr(x,2,4))^2
r <- NULL
for(i in 1:10000){
  y <- as.numeric(substr(y,3,6))^2
  for (j in 1:5) {
    if(nchar(y) < 8){
      y <- paste0("0",y)
    }
  }
  as.numeric(y) 
  r <- c(r,y)
}
hist(as.numeric(r)/999999)
ks.test(as.numeric(r) ,"punif")
#########################################
1.

midSquareRand <- function(seed, length) {
  randvector <- NULL
  for(i in 1:length) {
    value <- seed * seed 
    seed <- (value %/% 1000) %% 1000000
    randvector <- c(randvector, seed)
  }   
  return(randvector)
}

random <- midSquareRand(123457, 10000)
ks.test(random , "punif")
#############################
# x <- 3
# y <- 67
# z <- 4451
###神秘數字###

x <- 3
y <- 67
z <- 4453
u <- NULL
for(i in 1:10000){
  x <- (x*171)%%30269
  y <- (y*172)%%30307
  z <- (z*170)%%30323
  u <- c(u,((x/30269)+(y/30307)+(z/30323))%%1)
}
hist(u)
ks.test(u ,"punif")
chisq.test( u , p =rep(1/length(u),length(u)))

#

U <- function(u){
  ((pi+u)^5)%%1
}

u <- 10
r <- NULL
for (i in 1:10000) {
  u <- U(u)
  r <- c(r,u)
}
hist(r)
ks.test(r ,"punif")

library(data.table)
library(dplyr)
#2
library(nortest)
r <- NULL
r[[1]] <- rnorm(10)
r[[2]] <- rnorm(50)
r[[3]] <- rnorm(100)
r[[4]] <- rt(10,10)
r[[5]] <- rt(50,10)
r[[6]] <- rt(100,10)
r[[7]] <- rt(10,20)
r[[8]] <- rt(50,20)
r[[9]] <- rt(100,20)

ks <- NULL
ad <- NULL
cvm <- NULL
lillie <- NULL
pearson <- NULL
sf <- NULL
p_value <- NULL
for(i in 1:9){
  ks <- ks.test(r[[i]],"pnorm")$p.value
  ad <- ad.test(r[[i]])$p.value
  cvm <- cvm.test(r[[i]])$p.value
  lillie <- lillie.test(r[[i]])$p.value
  pearson <- pearson.test(r[[i]])$p.value
  sf <- sf.test(r[[i]])$p.value
  p_value <- rbind(p_value,c(ks,ad,cvm,lillie,pearson,sf))
}
colnames(p_value)[c(1:6)] <- c("ks.test","ad.test","cvm.test","lillie.test","pearson.test","sf.test")
rownames(p_value)[c(1:9)] <- c("n=10,N(0,1)","n=50,N(0,1)","n=100,N(0,1)","n=10,t(10)","n=50,t(10)","n=100,t(10)","n=10,t(20)","n=20,t(20)","n=100,t(20)")
View(p_value)
#power

nfun <- function(n){
  c(ks.test(rnorm(n),"pnorm")$p.value,
    ad.test(rnorm(n))$p.value,
    cvm.test(rnorm(n))$p.value,
    lillie.test(rnorm(n))$p.value,
    pearson.test(rnorm(n))$p.value,
    sf.test(rnorm(n))$p.value)
}
tfun <- function(n,df){
  c(ks.test(rt(n,df),"pnorm")$p.value,
    ad.test(rt(n,df))$p.value,
    cvm.test(rt(n,df))$p.value,
    lillie.test(rt(n,df))$p.value,
    pearson.test(rt(n,df))$p.value,
    sf.test(rt(n,df))$p.value)
}
q01 <- c(0,0,0,0,0,0);q02 <- c(0,0,0,0,0,0);q03 <- c(0,0,0,0,0,0);q11 <- c(0,0,0,0,0,0); q12 <- c(0,0,0,0,0,0) ;q13 <- c(0,0,0,0,0,0) ; q21 <- c(0,0,0,0,0,0) ; q22 <- c(0,0,0,0,0,0) ; q23 <-c(0,0,0,0,0,0)
for(i in 1:1000){
  q01 <- q01+(1*nfun(10)<0.05)
  q02 <- q02+(1*nfun(50)<0.05)
  q03 <- q03+(1*nfun(100)<0.05)
  q11 <- q11+(1*(tfun(10,10)<0.05))
  q12 <- q12+(1*(tfun(50,10)<0.05))
  q13 <- q13+(1*(tfun(100,10)<0.05))
  q21 <- q21+(1*(tfun(10,20)<0.05))
  q22 <- q22+(1*(tfun(50,20)<0.05))
  q23 <- q23+(1*(tfun(100,20)<0.05))
}
power <- rbind(q01/1000,q02/1000,q03/1000,q11/1000,q12/1000,q13/10000,q21/1000,q22/1000,q23/1000) 
colnames(power)[c(1:6)] <- c("ks.test","ad.test","cvm.test","lillie.test","pearson.test","sf.test")
rownames(power)[c(1:9)] <- c("n=10,N(0,1)","n=50,N(0,1)","n=100,N(0,1)","n=10,t(10)","n=50,t(10)","n=100,t(10)","n=10,t(20)","n=20,t(20)","n=100,t(20)")
View(power)

############ 3.
#gap
set.seed(10)
x <- runif(1000)
r <- runif(2)
x1 <- which( x >= min(r) & x < max(r))
length(x1)
x2 <- x1[-1]-x1[-length(x1)]
k <- table(x2) %>% as.data.frame() %>% .[,2]
kk <- k[c(1:9)] 
kk[10] <- sum(k[c(10:length(k))])
kkk <- kk
thm <- function(k){
  (max(r)-min(r))*(1-(max(r)-min(r)))^k
}
gap <- NULL
for(i in 1:length(k)){
  gap <- c(gap,thm(i))
}
gap1 <- gap[c(1:9)]
gap1[10] <- 1-sum(gap1)
sum(((kkk-gap1*sum(kkk))^2)/(gap1*sum(kkk)))
qchisq(0.95,9)
#run

set.seed(9)
r <- runif(1000)
r1 <- 1*(r[-1]>r[-1000] ) #變號位置
sum(r1[-1] != r1[-999]) + 1 # 共有幾個run
r2 <- which(r1[-1] != r1[-999]) #run的位置
r3 <- table(r2[-1]-r2[-length(r2)]) #看run＝n的個數
r3
Z <- (sum(r3)-(2*1000-1)/3)/((16*1000-29)/90)^0.5


#permutation
# a <- c(33.0, 31.0, 34.5, 34.0)
# b <- c(29.5, 32.0, 32.9, 31.5)
# p <- matrix(combn(c(a,b),4),4)
# p1 <- apply( p , MARGIN = 2, sum)
# # p2 <- p1[1,]*100 + p1[2,]*10 + p1[3,]
# # table(p2)
# # sum((table(p2)-100/6)^2/(100/6))


p <- matrix(runif(1600),4,400)
p1 <- apply(p, 2, rank)
per <- p1[1,]*1000+p1[2,]*100+p1[3,]*10+p1[4,]
x <- table(per) %>% as.data.frame() %>% .[,2]
Q <- sum(((x-(400/24))^2)/400/24)
qchisq(0.95,23)
#6

boxplot(c(7,6,8,4,5,6,8,9,5,7,6,7,8,7,-1))


add=function(a){
  (1997/(2+a))-(906/(1-a))-(904/(1-a))+(32/(a))
}

add2=function(a){
  (-1997/((2+a)^2))-(1810/((1-a)^2))-(32/(a^2))}

newton=function(f,fp,start){
  i=0
  new=start
  r=c(i,new,f(new))
  while(abs(f(new)>10^(-7))){
    i=i+1
    new=new-(f(new)/fp(new))
    r=rbind(r,c(i,new,f(new)))}
  r}
newton(add,add2,0.5)















#
r <- NULL
for (i in 1:10000) {
  u <- runif(12)
  n <- sum(u)-6 
  r <- c(r,n)
}
hist(r)
ks.test(r ,"pnorm")
####################4











####################
#5
#bisection
f <- function(x){
  exp(1)-(1/(3.5+x))
}
g <- function(x){
  (exp(-x)/sqrt(1+x^2))-0.5
}
Ei=function(x){
  (2-x)*(4-x)*(6-x)+60+60-16*(4-x)-9*(6-x)-25*(2-x)
}
bis <- function(f,a,b){
  z <- abs(b-a)
  i <- 0
  print(z)
  while (z > 0.000000001) {
    c <- (a+b)/2
    ifelse(f(c)<0, a <- c , b <- c)
    z <- abs(b-a)
    i <- i+1
    
  }
  print(c(c,i))
}
curve(g,0,1);abline(h=0)
bis(f,-3.2,-3)
bis(g,0.6,0.4)
bis(Ei,-0.5,-0.4)
bis(Ei,-0.4,0)
bis(Ei,13,12)
#false positions
f <- function(x){
  exp(1)-(1/(3.5+x))
}
g <- function(x){
  (exp(-x)/sqrt(1+x^2))-0.5
}
Ei=function(x){
  (2-x)*(4-x)*(6-x)+60+60-16*(4-x)-9*(6-x)-25*(2-x)
}
fal <- function(f,a,b){
  z <- abs(b-a)
  i <- 0
  while(z >0.000001){
    c <- b-(f(b)*((b-a)/(f(b)-f(a))))
    if(f(b)*f(c)>0) b <- c
    z <- f(b)
    i <- i+1
    
  }
  print(c(c,i))
}
curve(f,-5,0)
abline(h=0)
fal(f,-3.4,-3)
fal(g,0.4,0.6)
fal(Ei,-0.5,-0.4)
fal(Ei,-0.4,0)
fal(Ei,13,12)

curve(Ei,-1,0.01);abline(h=0)

#
#secant
f <- function(x){
  exp(1)-(1/(3.5+x))
}
g <- function(x){
  (exp(-x)/sqrt(1+x^2))-0.5
}
Ei=function(x){
  (2-x)*(4-x)*(6-x)+60+60-16*(4-x)-9*(6-x)-25*(2-x)
}
sec <- function(f,a,b){
  z <- abs(f(b)-f(a))
  i <- 0
  while (z > 0.000001) {
    c <- b-(f(b)*((b-a)/(f(b)-f(a))))
    a <- b
    b <- c
    z <- abs(f(b)-f(a))
    i <- i+1
    
  }
  print(c(c,i))
}
sec(f,-3.2,-3)
sec(g,0.4,0.6)
sec(Ei,-0.5,-0.4)
sec(Ei,-0.4,0)
sec(Ei,13,12)










#
Ai=function(x){
  (2-x)*(4-x)*(6-x)+60+60-16*(4-x)-9*(6-x)-25*(2-x)
}
curve(Ai,12,13);abline(h=0,v=0)
eigen(matrix(c(2,3,4,3,4,5,4,5,6),ncol=3))
str(uniroot(f3,c(12,13)));str(uniroot(f3,c(-1,-0.4)));str(uniroot(f3,c(-0.4,0)))











##############################
5