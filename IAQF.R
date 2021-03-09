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

#get portfolio volatility
v1 <- sd(P1)
V2 <- sd(P2)


#market impact
beta1  <- cov(P1,SP500)/var(SP500)
beta2  <- cov(P2,SP500)/var(SP500)

#P1 <- P1-SP500*as.vector(beta1)
#P2 <- P2-SP500*as.vector(beta2)

plot(P1)
lines(P2,col = "red")



#ML regression
library(glmnet)
#get data
tics <- c("SPY","MDY","DIA","QQQ","IJH","IJR","IVE","IVV","IVW","IWB",
          "IWD","IWF","IWM","IWV","IYY","IJJ","IJK","IJS","IJT","IUSG",
          "IWN","IWO","IUSV","OEF","SLYG","SLYV","SPTM","SPYG","SPYV",
          "VTI","IWP","IWR","IWS","VXF","PWC","RSP","ONEQ","ITOT","VB",
          "VBK","VBR","VO","VTV","VUG","VV","ETHO","SPXE","JPUS","JHMM",
          "JHML","GSLC","CSB","CSA","CDL","SCIU","XRLV","QUS","LRGF","SPYB",
          "ROUS","TUSA","VALX","EQAL","CFO","CFA","FLGE","FBGX","DGRO","VUSE",
          "NOBL","FNDX","FNDB","FNDA","SPSM","QUAL","DGRS","BFOR","SYLD",
          "DGRW","VLUE","JKD","JKE","JKF","JKG","JKH","JKI","JKJ","JKK","JKL",
          "SUSA","PWB","PWV","XSMO","XSVM","XLG","IWC","FDM","MDYG","SLY","SPLG",
          "PRF","PFG","RFV","RPG","RZG","QQEW","VIG","DES","DLN","DON","DTD","VOE",
          "VOT","PRFZ","DSI","VYM","EQWL","PKW","XMHQ","EES","EPS","EXT","EZM","QSY",
          "CZA","FAB","FAD","FEX","FNX","FTA","FTC","FYX","MGC","MGK","MGV","RWJ",
          "RWK","RWL","CSM","EQL","IWL","IWX","IWY","SCHA","SCHB","SCHX","SCHV",
          "SCHG","EUSA","IVOG","IVOO","IVOV","VIOG","VIOO","VIOV","VONE","VONG",
          "VONV","VOO","VOOG","VOOV","VTHR","VTWG","VTWO","VTWV","EWMC","EWSC","SCHM","FNK")

#("VFLQ","VFMV","VFMO","VFMFX","VFQY","VFVA","FOVL","MIDF",,"VFLQ")

try(Stock <- lapply(tics, function(x) 
  get(getSymbols(x, from = "2016-02-02",to = "2017-02-03"))))


#data process
data1 <- lapply(Stock,function(p) p[,6]) 
data <- Reduce(merge,data1)


#calculate return and other features
data <- log(data/lag(data))

#omit NAs
data <- na.omit(data)

#divide traning set and testing set
dim(data)
set.seed(200)
idx <- sample.int(2,size=nrow(data),replace=T,prob=c(0.8,0.2))
y <- P1[which(idx==1)]
x <- data[which(idx==1),]
ytest <- P1[which(idx==2)]
xtest <- as.matrix(data[which(idx==2),])

#MSE function
mse <- function(pred,act){
  mean((pred-act)^2)
}

lam <- 10^seq(5,-5,length=100)
lasso_mse <- glmnet(x,y,alpha = 1,lambda = lam,thresh = 1e-12,standardize = T)

MSEl <- NA
xt <- as.matrix(xtest)

for (i in 1:length(lam)){
  predlasso <- predict(lasso_mse,s = lam[i],newx = xt)
  MSEl[i]<- mse(predlasso,ytest)
}


plot(MSEl)

minl <- lam[which.min(MSEl)]
message(sprintf("when lamboda equal %f, we get minimal MSE for lasso regression ",minl))

predlasso <- predict(lasso_mse,s = minl,newx = xt)

lasso <- glmnet(x,y,alpha = 1,lambda = minl,thresh = 1e-12,standardize = T)

print(lasso_mse$beta[,which.min(MSEl)],digits=2)


lasso <- lasso_mse$beta[,which.min(MSEl)]

corelation <- cor(data)
good <- data[,which(lasso>0)]
good <- good[,-c(1,5,6)]


kkk <-lm(P1~good)
kk2 <-lm(P2~good)
newgood <- good[,-2]

newbad <- cbind(good,rep(0,253))
newa <- rbind(as.data.frame(P1),as.data.frame(P2))
summary(kkk)
summary(kk2)

res1 <- resid(kkk)
res2 <- resid(kk2)

#"VLUE" "RPG" "DON" "EQL"
names(res1)
which(names(res1)=="2016-11-08")
which(names(res1)=="2016-12-19")

plot(res1,col="red",xlab = "TIME",ylab = "residuals",main = "residuals for 2 portfolio")+
  points(res2,col="blue")+abline(v=195,col="lightgray")+abline(v=223,col="lightgray")+abline(h=0)


#test
notsogood <- which(lasso>0)
ithinkgood<-c("VLUE","RPG","DON","EQL")
try(gogogo <- lapply(ithinkgood, function(x) 
  get(getSymbols(x, from = "2016-02-02",to = "2017-02-03"))))


#data process
go1 <- lapply(gogogo,function(p) p[,6]) 
go <- Reduce(merge,go1)
#go <- go[-1,]

#calculate return and other features
go <- log(go/lag(go))
go <- na.omit(go)

fail1 <- lm(P1~data[,notsogood])
fail2 <- lm(P2~data[,notsogood])

summary(fail1)
summary(fail2)

gog <- lm(P1~go)
ggo <- lm(P2~go)
summary(gog)
summary(ggo)

res1 <- resid(gog)
res2 <- resid(ggo)
plot(res1)



tics <- c("XLI","XLU","XLK","XLE")
P.list1 <- lapply(tics, function(tic) 
  get(getSymbols(tic, from = "2016-05-03",to = "2017-02-03")))
P <- lapply(P.list1, function(p) p[,6])
P <- Reduce(merge,P)
plot(P[,1],col="blue")
plot(P[,2],col="blue")
plot(P[,3],col="red")
plot(P[,4],col="red")
par(mfrow=c(2,2))

par(mfrow=c(1,1))


