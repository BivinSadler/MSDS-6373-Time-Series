#UNIT 13 R CODE
SWATrain = ts(SWA$arr_delay[1:141],start= c(2004,1),frequency = 12)
SWATest = ts(SWA$arr_delay[142:177],start = c(2015,10),frequency = 12)
set.seed(2)
fit.mlp = mlp(SWATrain,reps = 50,comb = "mean")
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)
ASE = mean((SWATest - fore.mlp$mean)^2)
ASE



#MORE SWA NNs
fit.mlp = mlp(SWATrain, lags = c(1,2,3,4,5,6,7,8,9,10,11,12), allow.det.season = FALSE)
set.seed(2)
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)
ASE = mean((SWATest - fore.mlp$mean)^2)
ASE


#Even more SWA NNs
fit.mlp = mlp(SWATrain, difforder = c(12), allow.det.season = FALSE)
set.seed(2)
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)
ASE = mean((SWATest - fore.mlp$mean)^2)
ASE




#AIRLINE DATA
# First 108 months in the Training Set.
library(tswge)
library(nnfor)
set.seed(5)
data(airlog)
lairTrain = ts(airlog[1:108], frequency = 12, start = c(1949, 1))
# Last 36 months in the Test set. 
lairTest = ts(airlog[109:144], frequency = 12, start = c(1958, 1))
fit.mlp = mlp(lairTrain,difforder = c(12), reps = 100)
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)
ASE = mean((lairTest - fore.mlp$mean)^2)
ASE









#BS is the Business data
# Only Time as a regressor
tBS80 = ts(BS$sales[1:80])
set.seed(2)
fit3 = mlp(tBS80)
f = forecast(fit3, h = 20)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE


#With additional Regressors
set.seed(2)
tBS80 = ts(BS$sales[1:80])
tBSx = data.frame(ad_tv = ts(BS$ad_tv), ad_online = ts(BS$ad_online, frequency = 7),discount = ts(BS$discount)) 
fit3 = mlp(tBS80,xreg = tBSx)
fit3
f = forecast(fit3, h = 20, xreg = tBSx)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE


#ARIMA Model with Regressors
ad_tv1 = c(NA,BS$ad_tv[1:(length(BS$ad_tv)-1)])
ad_online1 = c(NA,BS$ad_online[1:(length(BS$ad_online)-1)])
BS$ad_tv1= ad_tv1
BS$ad_online1 = ad_online1
ksfit=lm(sales~ad_tv1+ad_online1+discount, data = BS)
aic.wge(ksfit$residuals,p=0:8,q=0:0)  # AIC picks p=7
fit=arima(BS$sales,order=c(7,0,0),xreg=cbind(BS$ad_tv1, BS$ad_online1, BS$discount))
preds = forecast(fit,h = 20, xreg = cbind(BS$ad_tv1[81:100],BS$ad_online1[81:100],BS$discount[81:100]))
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),preds$mean, col = "blue")
ASE = mean((BS$sales[81:100]-preds$mean)^2)
ASE


#NN With additional Regressors
set.seed(2)
tBS80 = ts(BS$sales[1:80])
tBSx = data.frame(ad_tv = ts(BS$ad_tv), ad_online = ts(BS$ad_online, frequency = 7),discount = ts(BS$discount)) 
fit3 = mlp(tBS80,xreg = tBSx)
f = forecast(fit3, h = 20, xreg = tBSx)
plot(BS$sales[81:100],type = "l")
lines(seq(1,20),f$mean, col = "blue")
ASE = mean((BS$sales[81:100]-f$mean)^2)
ASE
