rm(list=ls())
library(lubridate)
#add time
time<-c(NA)
for(i in 1:31){
 time[i]<-20200501+i-1
}
for(i in 1:30){
  time[i+31]<-20200601+i-1
}
for(i in 1:31){
  time[i+61]<-20200701+i-1
}
for(i in 1:31){
  time[i+92]<-20200801+i-1
}
for(i in 1:30){
  time[i+123]<-20200901+i-1
}
for(i in 1:31){
  time[i+153]<-20201001+i-1
}
for(i in 1:30){
  time[i+184]<-20201101+i-1
}
for(i in 1:31){
  time[i+214]<-20201201+i-1
}
time<-ymd(time)

#read data
confirmed_data<-read.csv("confirmed.csv",row.names=1)
data<-as.data.frame(as.matrix(t(confirmed_data)))
data<-sqrt(data)
Weekday<-weekdays(time)
data<-data.frame(data,time,Weekday)
data<-data[which(data$Weekday!="ÐÇÆÚÁù"&data$Weekday!="ÐÇÆÚÈÕ"),]
#calculate the increased rate
increased_rate<-c(NA)
for(i in 1:(dim(data)[1]-1)){
  increased_rate[i]<-(data[(i+1),1]-data[i,1])/data[i,1]
}
#calculate the increased_increased_rate
increased_increased_rate<-c(NA)
for(i in 1:(length(increased_rate)-1)){
  increased_increased_rate[i]<-(increased_rate[i+1]-increased_rate[i])/increased_rate[i]
}
#input parameters 
t <- 1        
N <- length(increased_rate)     
mu <- (1+0.0003032881)^252
sigma <- (0.01180881*sqrt(252))
dt <- t/N
lambda <- 12.30172
a <- -0.006094214
b <- 0.03743268
M <-1
S0 <- 300
j<-c(NA)
X.JD <-matrix(NA,ncol=N+1,nrow=M)
X.JD[,1] <- log(S0)
dt <- t/N
sqdt <- sqrt(dt)
#generate historic jump process vector
for (i in 1:N){
  Z <- matrix(rnorm(M),ncol=1)
  NN <- matrix(rpois(M,lambda*dt),ncol=1) 
  Z2 <- matrix(rnorm(M),ncol=1)
  MM <- a*NN + b*sqrt(NN)*Z2
  j[i] <- MM
  X.JD[,i+1] <- X.JD[,i] + (mu - 0.5*sigma^2)*dt +sigma*sqdt*Z + MM
}
#estimate a and b
#j is the historic jump process vector
#news is the information vector
#J is the estimated reaction of market
J <- info <- rep(0,length(j))
jump <- j
jump[!jump==0]<-1
st <- 1
news<-increased_rate
for(i in 1:length(j)){
  if(!jump[i]==0){
    #define the end of accumulated sum of information
    ed <- i
    
    #build info variable to get accumulated information between jumps
    info[i]<-sum(news[st:ed])
    
    #define the start of next accumulated sum of information
    st <- ed
  }
}

#now we build a function to estimate a, our objective is to minimize the RSS
#to get estimated a, we need to do optimization for funciton below:
f <- function(a){
  res<- rep(0,length(j))
  
  act <- 0
  
  for(i in 1:length(j)){
    if(!jump[i]==0){
      #expectation of jump process
      J[i] <- (act+info[i])*a
      
      #the act vector is next justification of market reaction
      act <- -(j[i]-J[i])/a
      
      #record the residuals
      res[i] <- (j[i]-J[i])^2
    }
  }
  
  #RSS
  RSS <- sum(res)
  return(RSS)
}
##use gold search to get the minimal a
#main function
opt_golden<-function(f,a,b,tol=1e-1,N=200){
  tau<-(sqrt(5)-1)/2
  l<-a+(1-tau)*(b-a)
  x<-l
  r<-a+tau*(b-a)
  fl<-f(l)
  fr<-f(r)
  outstats<-as.data.frame(matrix(NA,nrow=N,ncol=5))
  names(outstats)<-c("iter","a","b","x","f(x)")
  outstats[1,1]<-0
  outstats[1,4]<-l
  outstats[1,5]<-f(l)
  for(i in 1:N){
    outstats[i+1,1]<-i
    outstats[i+1,2]<-a
    outstats[i+1,3]<-b
    outstats[i+1,4]<-x
    outstats[i+1,5]<-f(x)
    if(abs(a-b)<tol){
      result<-list("x*"=x,"iter"=i-1)
      message(sprintf("x*=%.4f",x))
      message(sprintf("%d iterations",i-1))
      return(result)
      break
    }
    if(fr<fl){
      a<-l
      l<-r
      r<-a+tau*(b-a)
      fl<-fr
      fr<-f(r)
      x<-r
      fx<-fr
    }
    else{
      b<-r
      r<-l
      l<-a+(1-tau)*(b-a)
      fr<-fl
      fl<-f(l)
      x<-l
      fx<-fl
    }
  }
  x<-"Don't get the precise answer"
  result<-list("x*"=x,"iter"=i-1)
  return(result)
}
#generate a and b
a<-opt_golden(f,-10,10,tol=1e-8,N=2000)$x
b<-sqrt(f(a))

#jump model:
M<-1000
Jump <- matrix(NA,M,N+1)
Jump[,1]<-log(S0)
dt <- t/N
st <- sqrt(dt)

start <- rep(1,M)
act <- info <- rep(0,M)


for (i in 1:N){
  #GBM part
  Z <- rnorm(M)
  
  #jump part
  #generate jump action
  j <- rpois(M,lambda*dt)
  
  #define a vector when jump apper marked as true
  do <- !j==0
  
  #ed is the vector of end period of accumulated information
  ed <- i*do+start*!do
  
  #define the infomation variable
  for(k in which(do==1)){
    info[k] <- sum(news[start[k]:ed[k]])
  }
  
  #define the magnitude, here we assumed market reaction is 
  #stocastic but around accumulated information-the pandemic
  Z2 <- rnorm(M)
  J <- (info+act)*do*a + b*sqrt(j)*Z2
  
  #overact or underact can influence next jump,
  act <- -b*sqrt(j)*Z2/a*do+act*!do
  
  #the end today is the start of accumulated information in next jump
  start <- (i+1)*do+start*!do
  
  Jump[,i+1] <- Jump[,i] + (mu - 0.5*sigma^2)*dt + sigma*st*Z + J
  
}
Jump <- exp(Jump)
plot(Jump[1,],type = "l")
apply(Jump)
