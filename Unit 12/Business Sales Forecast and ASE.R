# UNIT 12 R Code

### MLR Modeling


# Model 1

#Assuming a data.frame exists with corresponding names below.
BSales = read.csv(file.choose(), header = TRUE)

# All data with no lag and no trend
ksfit=lm(sales~ad_tv+ad_online+discount, data = BSales)
aic.wge(ksfit$residuals,p=0:8, q=0)  # AIC picks p=7
fit=arima(BSales$sales,order=c(7,0,0),xreg=BSales[,3:5])
fit

acf(fit$residuals)
ltest = ljung.wge(fit$resid)
ltest$pval

# ASE for model with no lag and no trend (last 5)
BSales2 = BSales[1:95,]
ksfit=lm(sales~ad_tv+ad_online+discount, data = BSales2)
aic.wge(ksfit$residuals,p=0:8, q=0)  # AIC picks p=7
fit=arima(BSales2$sales,order=c(7,0,0),xreg=cbind(BSales2$ad_tv,BSales2$ad_online,BSales2$discount))
fit

preds = predict(fit, newxreg = cbind(BSales$ad_tv[96:100],BSales$ad_online[96:100],BSales$discount[96:100]))
ASE1 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE1

dev.off()
plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$pred, type = "l", col = "red")





# Model 2

#with trend

# ASE for model with no lag and trend (last 5)
t=1:100
BSales$t = t
BSales2 = BSales[1:95,]
ksfit=lm(sales~t+ad_tv+ad_online+discount, data = BSales2)
aic.wge(ksfit$residuals,p=0:8, q=0)  # AIC picks p=7
fit=arima(BSales2$sales,order=c(6,0,0),xreg=cbind(BSales2$ad_tv,BSales2$ad_online,BSales2$t,BSales2$discount))
fit

preds = predict(fit, newxreg = cbind(BSales$ad_tv[96:100],BSales$ad_online[96:100],BSales$t[96:100],BSales$discount[96:100]))
ASE2 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE2

dev.off()
plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$pred, type = "l", col = "red")





#Lagging Variables
#Example:
#With dplyr lag function
library(dplyr)
df = data.frame(Y = c(1,1,2,3,4,4,5,8),X1 = c(5,6,6,7,7,8,8,9))
df$X1_L1 = dplyr::lag(df$X1,1)
df$X1_L2 = dplyr::lag(df$X1,2)
df




# Model 3

#Lagging BSales Ad Variables
ad_tv1 = dplyr::lag(BSales$ad_tv,1)
ad_online1 = dplyr::lag(BSales$ad_online,1)
BSales$ad_tv1= ad_tv1
BSales$ad_online1 = ad_online1


#with trend and lagging

# ASE for model with no lag and trend (last 5)
t=1:100
BSales$t = t
BSales2 = BSales[2:95,]
ksfit=lm(sales~t+ad_tv1+ad_online1+discount, data = BSales2)
aic.wge(ksfit$residuals,p=0:8,q=0:0)  # AIC picks p=7
fit = arima(BSales2$sales,order = c(7,0,0), xreg = cbind(BSales2$ad_tv1,BSales2$ad_online1,BSales2$t,BSales2$discount))
fit


preds = predict(fit, newxreg = cbind(BSales$ad_tv1[96:100],BSales$ad_online1[96:100],BSales$t[96:100],BSales$discount[96:100]))
ASE3 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE3


dev.off()
plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$pred, type = "l", col = "red")





####### Forecast Features 

plotts.sample.wge(BSales$ad_tv)
aic5.wge(BSales$ad_tv)
est_ad_tv = est.arma.wge(BSales$ad_tv,p = 2, q = 2)
ad_tvFORECAST = fore.arma.wge(BSales$ad_tv,phi = est_ad_tv$phi, theta = est_ad_tv$theta, n.ahead = 6)

plotts.sample.wge(BSales$ad_online)
aic5.wge(BSales$ad_online, p = 0:10)
est_online = est.arma.wge(BSales$ad_online,p = 6)
dev.off()
plot.ts(BSales$ad_online[1:100])
ad_onlineFORECAST = fore.arma.wge(BSales$ad_online,phi = est_online$phi, n.ahead = 6)



#with trend and lagging

ad_tvFORECAST1 = lag(ad_tvFORECAST,1)

ad_onlineFORECAST1 = lag(ad_onlineFORECAST,1)

# ASE for model with no lag and trend (last 5)
t=1:100
BSales$t = t
BSales2 = BSales[2:95,]
ksfit=lm(sales~t+ad_tv1+ad_online1, data = BSales2)
aic.wge(ksfit$residuals,p=0:8,q=0:0)  # AIC picks p=7
fit = arima(BSales2$sales,order = c(7,0,0), xreg = cbind(BSales2$ad_tv1,BSales2$ad_online1,BSales2$t))
fit


preds = predict(fit, newxreg = cbind(ad_tvFORECAST$f[2:6],ad_onlineFORECAST$f[2:6],BSales$t[96:100]))
ASE3.5 = mean((BSales$sales[96:100] - preds$pred[1:5])^2)
ASE3.5


plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$pred[2:6], type = "l", col = "red")








############ VAR MODELS ##########################

#Model 4

BSVar = VAR(cbind(BSales2$sales,BSales2$ad_tv1,BSales2$ad_online1), type = "both", lag.max = 10)

preds = predict(BSVar,n.ahead = 5)
                  
ASE4 = mean((BSales$sales[96:100] - preds$fcst$y1[,1])^2)
ASE4


dev.off()
plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$fcst$y1[,1], type = "l", col = "red")


#Model 5
BSVar = VAR(cbind(BSales2$sales,BSales2$ad_tv,BSales2$ad_online), type = "both", lag.max = 10)

preds = predict(BSVar,n.ahead = 5)

ASE5 = mean((BSales$sales[96:100] - preds$fcst$y1[,1])^2)
ASE5

dev.off()
plot(seq(1,100,1), BSales$sales[1:100], type = "l",xlim = c(0,100), ylab = "Business Sales", main = "5 Week Sales Forecast")
lines(seq(96,100,1), preds$fcst$y1[,1], type = "l", col = "red")
