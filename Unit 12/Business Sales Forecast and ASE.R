# UNIT 12 R Code

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
fit=arima(BSales2$sales,order=c(7,0,0),xreg=BSales2[,3:5])
fit

preds = predict(fit, newxreg = BSales[96:100,3:5])
ASE1 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE1




#with trend

# ASE for model with no lag and trend (last 5)
t=1:100
BSales$t = t
BSales2 = BSales[1:95,]
ksfit=lm(sales~t+ad_tv+ad_online+discount, data = BSales2)
aic.wge(ksfit$residuals,p=0:8, q=0)  # AIC picks p=7
fit=arima(BSales2$sales,order=c(6,0,0),xreg=BSales2[,3:6])
fit

preds = predict(fit, newxreg = BSales[96:100,3:6])
ASE2 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE2





#Lagging Variables
#Example:
#With dplyr lag function
library(dplyr)
df = data.frame(Y = c(1,1,2,3,4,4,5,8),X1 = c(5,6,6,7,7,8,8,9))
df$X1_L1 = dplyr::lag(df$X1,1)
df$X1_L2 = dplyr::lag(df$X1,2)
df



ad_tv1 = dplyr::lag(BSales$ad_tv,1)
ad_online1 = dplyr::lag(BSales$ad_online,1)
discount = BSales$discount
BSales$ad_tv1= ad_tv1
BSales$ad_online1 = ad_online1


#with trend and lagging

# ASE for model with no lag and trend (last 5)
t=1:100
BSales$t = t
BSales2 = BSales[1:95,]
ksfit=lm(sales~t+ad_tv1+ad_online1+discount, data = BSales2)
aic.wge(ksfit$residuals,p=0:8,q=0:0)  # AIC picks p=7
fit=arima(BSales2$sales,order=c(7,0,0),xreg=BSales2[,5:8])
fit

preds = predict(fit, newxreg = BSales[96:100,5:8])
ASE3 = mean((BSales$sales[96:100] - preds$pred)^2)
ASE3



BSVar = VARselect(cbind(BSales2$sales,ad_tv1+ad_online1+discount, type = "both", )
                  
                  