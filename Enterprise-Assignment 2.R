#----------------------------------------------------------------------------------------------------------------
# Problem 1
#----------------------------------------------------------------------------------------------------------------

library(lattice)
library(MASS)
library(psych)
#install.packages("triangle")
library(triangle)

r<-runif(5000,min=0,max=1)
class(r)
Simulation_T<-rtriangle(r,20,300,80)

histogram(Simulation_T,type = "percent", main = "Triangular Distribution",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

Average_victims<-function(value,hospital){
  hospital_name<-c()
  c<-0
  for (i in 1:length(Simulation_T)) {
    c<-(Simulation_T[i]*value)
    hospital_name<-c(hospital_name,c)
  }
  hospital<-hospital_name
}

Beth.Israel.Medical<-Average_victims(0.30,hospital)
Tufts.Medical<-Average_victims(0.15,hospital)
Massachusetts.General<-Average_victims(0.20,hospital)
Boston.Medical<-Average_victims(0.25,hospital)
Brigham.and.Womens<-Average_victims(0.10,hospital)

Avg_victims<-data.frame(Beth.Israel.Medical,Tufts.Medical,Massachusetts.General,
                     Boston.Medical,Brigham.and.Womens)


Average_time<-function(value,nam){
  hospital_name<-c()
  c<-0
  for (i in 1:length(Simulation_T)) {
    c<-(value*nam[i])/60
    hospital_name<-c(hospital_name,c)
  }
  tim<-hospital_name
}

Beth.Israel.Medical_time<-Average_time(7,Beth.Israel.Medical)
Tufts.Medical_time<-Average_time(10,Tufts.Medical)
Massachusetts.General_time<-Average_time(15,Massachusetts.General)
Boston.Medical_time<-Average_time(15,Boston.Medical)
Brigham.and.Womens_time<-Average_time(20,Brigham.and.Womens)

Avg_time<-data.frame(Beth.Israel.Medical_time,Tufts.Medical_time,Massachusetts.General_time,
                     Boston.Medical_time,Brigham.and.Womens_time)

Avg_time_per_victim<-c()
d<-0
for (i in 1:length(Simulation_T)) {
    d<-sum(Beth.Israel.Medical_time[i]+Tufts.Medical_time[i]+Massachusetts.General_time[i]+Boston.Medical_time[i]+Brigham.and.Womens_time[i])*60/mean(Simulation_T)
    Avg_time_per_victim<-c(Avg_time_per_victim,d)
}


#Average of victims and time
beth_avg_vic<-mean(Avg_victims$Beth.Israel.Medical)
beth_avg_transport<-mean(Avg_time$Beth.Israel.Medical_time)


a<-describe(Avg_time$Beth.Israel.Medical_time)


#law of large numbers
roll <- function(n) {
  mean(sample(Avg_victims$Beth.Israel.Medical, size = n, replace = TRUE))
}

plot(sapply(1:5000, roll), type = "l", xlab = "Number of observations", ylab = "average", main="Beth Israel Medical-Law of large numbers")
abline(h = beth_avg_vic, col = "red")

#total transport time
total_transport_time<-c()
for (i in 1:length(Simulation_T)) {
  v<-Beth.Israel.Medical[i]*Beth.Israel.Medical_time[i]
  total_transport_time<-c(total_transport_time,v)
}

#calulate confidence interval
confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt((interval + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- c("lower" = vec_mean - error, "upper" = vec_mean + error)
  return(list(result,"margin_of_error"=error))
}

confidence_interval(Avg_time$Beth.Israel.Medical_time,0.95)

histogram(Avg_time$Beth.Israel.Medical_time,type = "percent", main = "Probability Distribution plot",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

chisq.test(total_transport_time,Simulation_T,rescale.p = T)

#average transport time per victim
avg_transport_time<-vector()
for (i in 1:length(Simulation_T)) {
  avg_transport_time[i]<-(Beth.Israel.Medical_time[i]+Tufts.Medical_time[i]+Massachusetts.General_time[i]+Boston.Medical_time[i]+Brigham.and.Womens_time[i])/60
}

confidence_interval(avg_transport_time,0.95)

histogram(avg_transport_time,type = "percent", main = "Probability Distribution plot",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

chisq.test(avg_transport_time,Simulation_T,rescale.p = T)


#------------------------------------------------------------------------------------------------------
# Problem 2
#------------------------------------------------------------------------------------------------------


r<-runif(5000,min=0,max=1)

norm_stimulation<-rnorm(r,mean=150,sd=50)

histogram(norm_stimulation,type = "percent", main = "Normal Distribution",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=25)


Average_victims<-function(value,hospital){
  hospital_name<-c()
  c<-0
  for (i in 1:length(norm_stimulation)) {
    c<-(norm_stimulation[i]*value)
    hospital_name<-c(hospital_name,c)
  }
  hospital<-hospital_name
}

Beth.Israel.Medical<-Average_victims(0.30,hospital)
Tufts.Medical<-Average_victims(0.15,hospital)
Massachusetts.General<-Average_victims(0.20,hospital)
Boston.Medical<-Average_victims(0.25,hospital)
Brigham.and.Womens<-Average_victims(0.10,hospital)

Avg_victims<-data.frame(Beth.Israel.Medical,Tufts.Medical,Massachusetts.General,
                        Boston.Medical,Brigham.and.Womens)



Average_time<-function(value,nam,std){
  hospital_name<-c()
  c<-0
  for (i in 1:length(norm_stimulation)) {
    c<-((value*nam[i])+std)/60
    hospital_name<-c(hospital_name,c)
  }
  tim<-hospital_name
}

Beth.Israel.Medical_time<-Average_time(7,Beth.Israel.Medical,2)
Tufts.Medical_time<-Average_time(10,Tufts.Medical,4)
Massachusetts.General_time<-Average_time(15,Massachusetts.General,3)
Boston.Medical_time<-Average_time(15,Boston.Medical,5)
Brigham.and.Womens_time<-Average_time(20,Brigham.and.Womens,3)

Avg_time<-data.frame(Beth.Israel.Medical_time,Tufts.Medical_time,Massachusetts.General_time,
                     Boston.Medical_time,Brigham.and.Womens_time)


Avg_time_per_victim<-c()
d<-0
for (i in 1:length(norm_stimulation)) {
  d<-sum(Beth.Israel.Medical_time[i]+Tufts.Medical_time[i]+Massachusetts.General_time[i]+Boston.Medical_time[i]+Brigham.and.Womens_time[i])*60/mean(Simulation_T)
  Avg_time_per_victim<-c(Avg_time_per_victim,d)
}

#Average of victims and time
beth_avg_vic<-mean(Avg_victims$Beth.Israel.Medical)
beth_avg_transport<-mean(Avg_time$Beth.Israel.Medical_time)


b<-describe(Avg_time$Beth.Israel.Medical_time)
b


#law of large numbers
roll <- function(n) {
  mean(sample(Avg_victims$Beth.Israel.Medical, size = n, replace = TRUE))
}

plot(sapply(1:5000, roll), type = "l", xlab = "Number of observations", ylab = "average", main="Beth Israel Medical-Law of large numbers")
abline(h = beth_avg_vic, col = "red")


#total transport time
total_transport_time<-c()
for (i in 1:length(norm_stimulation)) {
  v<-Beth.Israel.Medical[i]*Beth.Israel.Medical_time[i]
  total_transport_time<-c(total_transport_time,v)
}

confidence_interval(Avg_time$Beth.Israel.Medical_time,0.95)

histogram(Avg_time$Beth.Israel.Medical_time,type = "percent", main = "Probability Distribution plot",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=20)

library(psych)
nlm<-fitdistr(Avg_time$Beth.Israel.Medical_time,"normal")
para <- nlm$estimate

hist(Avg_time$Beth.Israel.Medical_time, prob = TRUE,breaks = 15)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

library(nortest)
pearson.test(Avg_time$Beth.Israel.Medical_time)

#average transport time per victim
avg_transport_time<-vector()
for (i in 1:length(norm_stimulation)) {
  avg_transport_time[i]<-(Beth.Israel.Medical_time[i]+Tufts.Medical_time[i]+Massachusetts.General_time[i]+Boston.Medical_time[i]+Brigham.and.Womens_time[i])/60
}

confidence_interval(avg_transport_time,0.95)

histogram(avg_transport_time,type = "percent", main = "Probability Distribution plot",  xlab = "Values", ylab = 'Frequency', col = "red",breaks=10)

nlm<-fitdistr(avg_transport_time,"normal")
para <- nlm$estimate

hist(avg_transport_time, prob = TRUE,breaks = 15)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

pearson.test(avg_transport_time)

