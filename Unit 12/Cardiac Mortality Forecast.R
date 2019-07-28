# CMort LA Pollution and Temperature Study

# MLR with Cor Errors 

#EDA
library(tidyverse)
head(CM)
ggpairs(CM[2:4]) #matrix of scatter plots

#forecast Particles
plotts.sample.wge(CM$part) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$part, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(2,1) assume stationary
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval #FTR Ho
ljung.wge(CM_52, K = 48)$pval #FTR Ho
#Going with white noise despite peak at 0 in Spec D. 
#est = est.arma.wge(CM_52, p = 3, q = 2)
#CM_52_AR2_MA1 = artrans.wge(CM_52,est$phi)
predsPart = fore.aruma.wge(CM$part,s = 52, n.ahead = 20)


#forecast Temp
plotts.sample.wge(CM$temp) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$temp, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(0,0)
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval
ljung.wge(CM_52, K = 48)$pval #barely rejects
acf(CM_52,lag.max = 48) # acf looks consistent with white noise
predsTemp = fore.aruma.wge(CM$temp,s = 52, n.ahead = 20)


# Model cmort based on predicted part and temp using MLR with Cor Erros
#assuming data is loaded in dataframe CM
ksfit = lm(cmort~temp+part+Week, data = CM)
phi = aic.wge(ksfit$residuals)
attach(CM)
fit = arima(cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(temp, part, Week))

# Check for whiteness of residuals
acf(fit$residuals)
ljung.wge(fit$residuals) # pval = .059
ljung.wge(fit$residuals, K = 48) # pval = .004

#load the forecasted Part and Temp in a data frame
next20 = data.frame(temp = predsTemp$f, part = predsPart$f, Week = seq(509,528,1))
#get predictions
predsCMort = predict(fit,newxreg = next20)
#plot next 20 cmort wrt time
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), predsCMort$pred, type = "l", col = "red")






#VAR Forecasts Seasonally Differenced Data 

#Difference all series to make them stationary (assumptoin of VAR)
# Doesn't have to be white... just stationary

CM_52 = artrans.wge(cmort,c(rep(0,51),1))
Part_52 = artrans.wge(part,c(rep(0,51),1))
Temp_52 = artrans.wge(temp,c(rep(0,51),1))

#VARSelect on Differenced Data chooses 2
VARselect(cbind(CM_52, Part_52, Temp_52),lag.max = 10, type = "both")

#VAR with p = 2
CMortDiffVAR = VAR(cbind(CM_52, Part_52, Temp_52),type = "both",p = 2)
preds=predict(CMortDiffVAR,n.ahead=20)

#We have predicted differences .... calculate actual cardiac mortalities 
startingPoint = CM$cmort[508]
CMortForcasts = preds$fcst$CM_52[,1:3] + startingPoint

#Plot
dev.off()
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), as.data.frame(CMortForcasts)$fcst, type = "l", col = "red")




#VAR Forecasts Seasonal Dummy

#VARSelect on Seasonal Data chooses 2
VARselect(cbind(CM$cmort, CM$part, CM$temp),lag.max = 10, season = 52, type = "both")

#VAR with p = 2
CMortVAR = VAR(cbind(CM$cmort, CM$part, CM$temp),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=20)

#Plot
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), preds$fcst$y1[,1], type = "l", col = "red")





#ARIMA 2: attempt at categorical variable for week but arima takes only continuous variables

#forecast Particles
plotts.sample.wge(CM$part) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$part, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(2,1) assume stationary
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval #FTR Ho
ljung.wge(CM_52, K = 48)$pval #FTR Ho
#Going with white noise despite peak at 0 in Spec D. 
#est = est.arma.wge(CM_52, p = 3, q = 2)
#CM_52_AR2_MA1 = artrans.wge(CM_52,est$phi)
predsPart = fore.aruma.wge(CM$part,s = 52, n.ahead = 20)


#forecast Temp
plotts.sample.wge(CM$temp) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$temp, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(0,0)
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval
ljung.wge(CM_52, K = 48)$pval #barely rejects
acf(CM_52,lag.max = 48) # acf looks consistent with white noise
predsTemp = fore.aruma.wge(CM$temp,s = 52, n.ahead = 20)


# Model cmort based on predicted part and temp using MLR with Cor Erros
#assuming data is loaded in dataframe CM
CM$FWeek = as.factor(CM$Week%%52)
ksfit = lm(cmort~temp+part+Week+FWeek, data = CM)
phi = aic.wge(ksfit$residuals)
attach(CM)
fit = arima(cmort,order = c(phi$p,0,0), xreg = cbind(temp, part, Week, FWeek))

# Check for whiteness of residuals
acf(fit$residuals)
ljung.wge(fit$residuals) # pval = .066
ljung.wge(fit$residuals, K = 48) # pval = .0058

#load the forecasted Part and Temp in a data frame
next20 = data.frame(temp = predsTemp$f, part = predsPart$f, Week = seq(509,528,1), FWeek = as.factor(seq(509,528,1)%%52))
#get predictions
predsCMort = predict(fit,newxreg = next20) #creates error because of factor

#predict residuals manually
plotts.sample.wge(ksfit$residuals)
phi = aic.wge(ksfit$residuals)
resids = fore.arma.wge(ksfit$residuals,phi = phi$phi,n.ahead = 20)
#predict trend manually
preds = predict(ksfit, newdata = next20)

predsFinal = preds + resids$f

#plot next 20 cmort wrt time
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), predsFinal, type = "l", col = "red")







#ARIMA 3: categorical variable
#With Lagged Time
library(dplyr)

#Lag Temperature 1 
CM$temp1 = dplyr::lag(CM$temp,1)


#forecast Particles
plotts.sample.wge(CM$part) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$part, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(2,1) assume stationary
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval #FTR Ho
ljung.wge(CM_52, K = 48)$pval #FTR Ho
#Going with white noise despite peak at 0 in Spec D. 
#est = est.arma.wge(CM_52, p = 3, q = 2)
#CM_52_AR2_MA1 = artrans.wge(CM_52,est$phi)
predsPart = fore.aruma.wge(CM$part,s = 52, n.ahead = 20)


#forecast Temp
plotts.sample.wge(CM$temp) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$temp, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(0,0)
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval
ljung.wge(CM_52, K = 48)$pval #barely rejects
acf(CM_52,lag.max = 48) # acf looks consistent with white noise
predsTemp = fore.aruma.wge(CM$temp,s = 52, n.ahead = 20)


# Model cmort based on predicted part and temp using MLR with Cor Erros
#assuming data is loaded in dataframe CM
CM$FWeek = as.factor(CM$Week%%52)
ksfit = lm(cmort~temp1+part+Week+FWeek, data = CM)
phi = aic.wge(ksfit$residuals)
attach(CM)
fit = arima(cmort,order = c(phi$p,0,0), xreg = cbind(temp1, part, Week, FWeek))

# Check for whiteness of residuals
acf(fit$residuals)
ljung.wge(fit$residuals) # pval = .066
ljung.wge(fit$residuals, K = 48) # pval = .0058

predsTemp$f1 = dplyr::lag(predsTemp$f,1)

#load the forecasted Part and Temp in a data frame
next20 = data.frame(temp1 = predsTemp$f1, part = predsPart$f, Week = seq(509,528,1), FWeek = as.factor(seq(509,528,1)%%52))
#get predictions
predsCMort = predict(fit,newxreg = next20) #creates error because of factor

#predict residuals manually
plotts.sample.wge(ksfit$residuals)
phi = aic.wge(ksfit$residuals)
resids = fore.arma.wge(ksfit$residuals,phi = phi$phi,n.ahead = 20)
#predict trend manually
preds = predict(ksfit, newdata = next20)

predsFinal = preds + resids$f

#plot next 20 cmort wrt time
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), predsFinal, type = "l", col = "red")









#Sandbox 1 s = 52 

#forecast Particles
plotts.sample.wge(CM$part) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$part, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(2,1) assume stationary
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval #FTR Ho
ljung.wge(CM_52, K = 48)$pval #FTR Ho
#Going with white noise despite peak at 0 in Spec D. 
#est = est.arma.wge(CM_52, p = 3, q = 2)
#CM_52_AR2_MA1 = artrans.wge(CM_52,est$phi)
predsPart = fore.aruma.wge(CM$part,s = 52, n.ahead = 20)


#forecast Temp
plotts.sample.wge(CM$temp) #freq near .0192 (annual)
CM_52 = artrans.wge(CM$temp, c(rep(0,51),1))
plotts.sample.wge(CM_52) #looks like some low freq?
aic5.wge(CM_52) #picks ARMA(0,0)
aic5.wge(CM_52,type = "bic") #picks ARMA(0,0) 
ljung.wge(CM_52)$pval
ljung.wge(CM_52, K = 48)$pval #barely rejects
acf(CM_52,lag.max = 48) # acf looks consistent with white noise
predsTemp = fore.aruma.wge(CM$temp,s = 52, n.ahead = 20)


# Model cmort based on predicted part and temp using MLR with Cor Erros
#assuming data is loaded in dataframe CM

ksfit = lm(cmort~temp+part+Week, data = CM)
phi = aic.wge(ksfit$residuals)
attach(CM)
fit = arima(cmort,order = c(phi$p,0,0), seasonal = list(order = c(0,1,0), period = 52),xreg = cbind(temp, part, Week))
AIC(fit)

# Check for whiteness of residuals
acf(fit$residuals)
ljung.wge(fit$residuals) # pval = .059
ljung.wge(fit$residuals, K = 48) # pval = .004

#load the forecasted Part and Temp in a data frame
next20 = data.frame(temp = predsTemp$f, part = predsPart$f, Week = seq(509,528,1))
#get predictions
predsCMort = predict(fit,newxreg = next20)
#plot next 20 cmort wrt time
plot(seq(1,508,1), cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), predsCMort$pred, type = "l", col = "red")



