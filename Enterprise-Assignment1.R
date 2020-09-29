#------------------------------------------------------------------------------------------------------------------
#Problem 1
#---------------------------------------------------------------------------------------------------------------
library(lattice)
library(MASS)
library(psych)
r<-runif(1000,min=0,max = 1)

X<- - log(r)
summary(X)
describe(X)

histogram(X,type = "percent", main = "Relative frequency histogram",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

fit<-fitdistr(X,"exponential")
ks.test(X, "pexp", fit$estimate)


hist(X, freq = FALSE, breaks = 100, xlim = c(0, quantile(X, 0.99)),main = "Probability Distribution Plot",xlab = "Values", ylab = 'Frequency')
curve(dexp(x, rate = fit$estimate), from = 0, col = "red", add = TRUE)


prob.exp<-dexp(X)
chisq.test(X,prob.exp,rescale.p = T)

#---------------------------------------------------------------------------------------------------------------
#Problem 2
#-------------------------------------------------------------------------------------------------------------

r1<-runif(10000,min=0,max=1)  
r2<-runif(10000,min=0,max=1)  
r3<-runif(10000,min=0,max=1)  
  
x_p2<- -log(r1*r2*r3)  
describe(x_p2)

p1<-histogram(x_p2,type = "percent", main = "Relative frequency histogram",  xlab = "Values", ylab = 'Frequency', col = "Blue")
p1
library(fitdistrplus)
fg<-fitdist(x_p2,distr = "gamma")
denscomp(fg)


chisq.test(x_p2)
prob.exp_p2<-dgamma(x_p2,shape = .83)
chisq.test(x_p2,p=prob.exp_p2,rescale.p = T)

#---------------------------------------------------------------------------------------------
#Problem 3
#---------------------------------------------------------------------------------------------

r1_p3<-runif(1000,min=0,max = 1)
r2_p3<-runif(1000,min=0,max = 1)

x1_p3<- -log(r1_p3)
x2_p3<- -log(r2_p3)


y=c()
cnt=0
for (i in 1:length(x2_p3)) {
  k<-(x1_p3[i]-1)^2/2
  if(x2_p3[i]>=k){
    cnt=cnt+1
    r3_p3<-runif(cnt)
    if (r3_p3[length(r3_p3)]>0.5){
        y=c(y,x1_p3[i])
    } else if (r3_p3[length(r3_p3)]<=0.5){
        y=c(y,-x1_p3[i])
    }
  } else {
    y=c(y,NA)
  }
}

selct.y<-y

y<-y[!is.na(y)]

summary(y)
describe(y)
histogram(y,type = "percent", main = "Relative frequency histogram",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

nlm<-fitdistr(y,"normal")
para <- nlm$estimate

hist(y, prob = TRUE,breaks = 15)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

library(nortest)
pearson.test(y)

#---------------------------------------------------------------------------------------------------
#Problem 4
#---------------------------------------------------------------------------------------------------
m<-1000

# for (i in 1:length(x2_p3)){
#     k<-(x1_p3[i]-1)^2/2
#     if(x2_p3[i]>=k){
#       n<-n+1
#     } else {
#       z<-i/n
#       w<-c(w,z)
#     }
# }

w=c()
finding_N<-function(x1_p3,x2_p3){
 n=0
 z=0
   for (i in 1:length(x2_p3)){
    k<-(x1_p3[i]-1)^2/2
    if(x2_p3[i]>=k){
      n<-n+1
    }

}
  z=m/n
}


for (j in 1:100) {
  R1 <- runif(1000)
  R2 <- runif(1000)
  
  X1 <- -log(R1)
  X2 <- -log(R2)
  w[j]<-finding_N(X1,X2)
  
}

View(w)

histogram(w,type = "percent", main = "Relative frequency histogram",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

hist(w, prob = TRUE)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

w<-w[w>0]
summary(w)
describe(w)

ndist<-fitdistr(w,"normal")
para <- ndist$estimate
para

#chisq.test(w)
pearson.test(w)

m_p4<-c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)
n_p4<-c()
for (i in m_p4) {
    n_p4<-c(n_p4,i/(i-sum(selct.y[1:i],na.rm = T)))
}

n_p4
