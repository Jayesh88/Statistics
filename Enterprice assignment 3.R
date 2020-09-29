#-----------------------------------------------------------------------------------------------
#Problem 1 - A
#------------------------------------------------------------------------------------------------


library("anytime")
library(bsts)
library("car")
library("caret")
library("forecast")
library("keras")
library("MCMCpack")
library("smooth")
library("tensorflow")
library("tseries")
library("TTR")
library(xts)

rd_honeywell<-read.csv(file.choose())
View(rd_honeywell)

rd<-rd_honeywell
rd_honeywell<-rd
class(rd_honeywell)
str(rd_honeywell)
#rd_honeywell$Date <- as.factor(gsub("-", "/", rd_honeywell$Date))

as.Date(rd_honeywell$Date,format="%d/%m/%y")
#str(rd_honeywell)

#install.packages("lubridate")
#library(lubridate)
#rd_honeywell$Date <- ymd_hms(rd_honeywell$Date)



sum(is.na(rd_honeywell))

ts_rd_honeywell<-as.ts(rd_honeywell)

ts_rd_honeywell_diff<-diff(ts_rd_honeywell)
plot(ts_rd_honeywell)
frcst_pr<-forecast(ts_rd_honeywell)


hltwin_1<-HoltWinters(ts_rd_honeywell[,3],
                    alpha=0.15,
                    beta=FALSE,
                    gamma=FALSE)
plot(hltwin_1,main = "Exponential Smoothing (alpha=0.15)")

fr1<-forecast(hltwin_1)

calculate_alpha<-function(alph,alt){
  al<-vector()
  a<-0
  for (i in 1:length(ts_rd_honeywell[,3])) {
    if(i==1){
      al<-c(al,ts_rd_honeywell[,3][i])
    }else if(i>=2) {
      a<-(alph*(ts_rd_honeywell[,3][i-1]))+(1-alph)*al[i-1]
      al<-c(al,a)
    }
  }
  alt<-al
}

hlt_1<-calculate_alpha(0.15,alt)
MSE(ts_rd_honeywell[,3],hlt_1)


hltwin_2<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.35,
                      beta=FALSE,
                      gamma=FALSE)
plot(hltwin_2,main = "Exponential Smoothing (alpha=0.35)")
fr2<-forecast(hltwin_2)

hlt_2<-calculate_alpha(0.35,alt)
MSE(ts_rd_honeywell[,3],hlt_2)

hltwin_3<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.55,
                      beta=FALSE,
                      gamma=FALSE)
plot(hltwin_3,main = "Exponential Smoothing (alpha=0.55)")
fr3<-forecast(hltwin_3)

hlt_3<-calculate_alpha(0.55,alt)
MSE(ts_rd_honeywell[,3],hlt_3)

hltwin_4<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.75,
                      beta=FALSE,
                      gamma=FALSE)
plot(hltwin_4,main = "Exponential Smoothing (alpha=0.75)")
fr4<-forecast(hltwin_4)

hlt_4<-calculate_alpha(0.75,alt)
MSE(ts_rd_honeywell[,3],hlt_4)

#-----------------------------------------------------------------------------------------------
#Problem 1 - B
#------------------------------------------------------------------------------------------------

calculate_Adjusted<-function(bet,beeta){
 bte<-vector()
  b<-0
  adj<-vector()
  a<-0
  for (i in 1:length(ts_rd_honeywell[,3])) {
    bte[1]<-0
    if(i>=2){
      b<-(bet*(hlt_4[i]-hlt_4[i-1]))+(1-bet*bte[i-1])
      bte<-c(bte,b)
    }
  }
  
  for (j in 1:length(ts_rd_honeywell[,3])) {
    a<-sum(hlt_4[j],bte[j])
    adj<-c(adj,a)
  } 
beeta<-adj
}

hltwin_1<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.75,
                      beta=0.15,
                      gamma=FALSE)

plot(hltwin_1,main = "Exponential Smoothing (Beta=0.15)")
fr<-forecast(hltwin_1)

hlt_5<-calculate_Adjusted(0.15,beeta)
MSE(ts_rd_honeywell[,3],hlt_5)


hltwin_2<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.75,
                      beta=0.25,
                      gamma=FALSE)
plot(hltwin_2,main = "Exponential Smoothing (Beta=0.25)")
  fr<-forecast(hltwin_2)
  
hlt_6<-calculate_Adjusted(0.25,beeta)
  MSE(ts_rd_honeywell[,3],hlt_6)

hltwin_3<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.75,
                      beta=0.45,
                      gamma=FALSE)
plot(hltwin_3,main = "Exponential Smoothing (Beta=0.45)")
fr<-forecast(hltwin_3)

hlt_7<-calculate_Adjusted(0.45,beeta)
MSE(ts_rd_honeywell[,3],hlt_7)

hltwin_4<-HoltWinters(ts_rd_honeywell[,3],
                      alpha=0.75,
                          beta=0.85,
                      gamma=FALSE)
plot(hltwin_4,main = "Exponential Smoothing (Beta=0.85)")
fr<-forecast(hltwin_4)

hlt_8<-calculate_Adjusted(0.85,beeta)
MSE(ts_rd_honeywell[,3],hlt_8)

#-----------------------------------------------------------------------------------------------
#Problem 2
#------------------------------------------------------------------------------------------------

Helicopter.Number<-c(1,2,3,4,5,6,7,8)
Labour.Hours<-c(2000,1400,1238,1142,1075,1029,985,957)

heli_dicision<-data.frame(Helicopter.Number,Labour.Hours)

lin_reg<-lm(Labour.Hours~.,data =heli_dicision )
summary(lin_reg)

pred<-predict(lin_reg,newdata = heli_dicision)
pred

plot(lin_reg)

#get the residuals 
resid(lin_reg)

with(heli_dicision,plot( Helicopter.Number,Labour.Hours, main="Labour hours Vs Helicopter number"))
plot(resid(lin_reg),main = "Residual Vs Helicopter number")

#-----------------------------------------------------------------------------------------------
#Problem 3
#------------------------------------------------------------------------------------------------

rd_new_car_sale<-read.csv(file.choose())
str(rd_new_car_sale)

rd_new_car_sale$Month<-as.character(rd_new_car_sale$Month)
rd_new_car_sale$Units <- gsub(",", "", rd_new_car_sale$Units)
rd_new_car_sale$Units<-as.integer(rd_new_car_sale$Units)

#install.packages("dummies")
#library(dummies)
#rd_new_car<-data.frame(rd_new_car_sale,dummy(rd_new_car_sale$Month,sep="_"))

mul_reg<-lm(Units~Month+Year,data =rd_new_car_sale)
summary(mul_reg)

pred_mul<-predict(mul_reg,newdata = rd_new_car)


chisq.test(rd_new_car$Units,pred_mul)


#anova
anova(mul_reg)
#get the residuals 
residduals<-resid(mul_reg)

library(lattice)
histogram(residduals,type = "percent", main = "Histogram of Residuals ",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

library(MASS)
library(psych)
nlm<-fitdistr(residduals,"normal")
para <- nlm$estimate

hist(residduals, prob = TRUE,breaks = 10)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

library(nortest)
pearson.test(residduals)
