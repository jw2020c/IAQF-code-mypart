#get data and data processing
library("quantmod")
tics <- c("MCD","F","APTV","STZ","EL","SPG","KO","WMT","HD","NEE","NSC","EXC","SPWR","FSLR","CSX",
          "HON","CVX","COP","MRO","C","CRM","MRK","AXP","V","GOOGL","FB","QCOM","PYPL","GILD","AMZN",
          "SPY")
P.list1 <- lapply(tics, function(tic) 
  get(getSymbols(tic, from = "2016-02-02",to = "2017-02-03")))
P <- lapply(P.list1, function(p) p[,6])
P <- Reduce(merge,P)

names(P) <- tics

#get return matrix
m <- 15
n <- nrow(P)
re <- matrix(NA,n-1,3)

P1 <- (log(P[,1:m]/lag(P[,1:m]))) #portfolio1
P2 <- (log(P[,(m+1):(2*m)]/lag(P[,(m+1):(2*m)]))) #portfolio2
SP500 <- log(P[,2*m+1]/lag(P[,2*m+1])) #S&P500

#ignore NAs
P1 <- na.omit(P1)
P2 <- na.omit(P2)
SP500 <- SP500[-1]

#get portfolio return
P1 <- apply(P1,1,mean)
P2 <- apply(P2,1,mean)
date <- names(P1)

ithinkgood<-c("VLUE","RPG","DON","EQL")
gogogo <- lapply(ithinkgood, function(x) 
  get(getSymbols(x, from = "2016-02-02",to = "2017-02-03")))


#data process
go1 <- lapply(gogogo,function(p) p[,6]) 
go <- Reduce(merge,go1)
#go <- go[-1,]

#calculate return and other features
go <- log(go/lag(go))
go <- na.omit(go)

gog <- lm(P1~go)
ggo <- lm(P2~go)
summary(gog)
summary(ggo)

library(lmtest)
#res1 is the portfolio 1 residuals and res2 same
res1 <- resid(gog)
res2 <- resid(ggo)

gqtest(gog)
gqtest(ggo)

day <- which(names(res1)=="2016-11-09")


plot(res1,col="blue",xlab = "TIME",ylab = "residuals",main = "residuals for 2 portfolio")+
  points(res2,col="red")+abline(v=day,col="lightgray")+abline(h = 0)

