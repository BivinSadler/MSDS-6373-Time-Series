# CMort LA Pollution and Temperature Study

# ARIMA 1 MLR with Cor Errors (no lag, no seasonl categorical variable)

#EDA
library(tidyverse)
library(GGally)
library(astsa)
library(tswge)
CM = read.csv(file.choose(),header = TRUE)

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

fit = arima(CM$cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(CM$temp, CM$part, CM$Week))

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

#Find ASE  Need to forecast last 30 of known series.  
CMsmall = CM[1:478,]
ksfit = lm(cmort~temp+part+Week, data = CMsmall)
phi = aic.wge(ksfit$residuals)
attach(CMsmall)
fit = arima(cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(temp, part, Week))

last30 = data.frame(temp = CM$temp[479:508], part = CM$part[479:508], Week = seq(479,508,1))
#get predictions
predsCMort = predict(fit,newxreg = last30)


ASE = mean((CM$cmort[479:508] - predsCMort$pred)^2)
ASE

#Find ASE  Need to forecast last 30 of known series.  
CMsmall = CM[1:478,]
CMsmall$temp_1 = dplyr::lag(CMsmall$temp,1)
CM$temp_1 = dplyr::lag(CM$temp,1)
ksfit = lm(cmort~temp_1+part+Week, data = CMsmall)
phi = aic.wge(ksfit$residuals)

fit = arima(CMsmall$cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(CMsmall$temp, CMsmall$part, CMsmall$Week))

last30 = data.frame(temp = CM$temp_1[479:508], part = CM$part[479:508], Week = seq(479,508,1))
#get predictions
predsCMort = predict(fit,newxreg = last30)

plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), predsCMort$pred, type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - predsCMort$pred)^2)
ASE








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
fit = arima(cmort,order = c(phi$p,0,0), xreg = cbind(CM$temp, CM$part, CM$Week, CM$FWeek))

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


#Find ASE  Need to forecast last 30 of known series.  
CMsmall = CM[2:478,]
ksfit = lm(cmort~temp+part+Week+FWeek, data = CMsmall)
phi = aic.wge(ksfit$residuals)
fit = arima(CMsmall$cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(CMsmall$temp1, CMsmall$part, CMsmall$Week, CMsmall$FWeek))

last30 = data.frame(temp = CM$temp[479:508], part = CM$part[479:508], Week = seq(479,508,1), FWeek = as.factor(seq(479,508,1)%%52))
#get predictions
predsCMort = predict(fit,newxreg = last30)

#predict residuals manually
plotts.sample.wge(ksfit$residuals)
phi = aic.wge(ksfit$residuals)
resids = fore.arma.wge(ksfit$residuals,phi = phi$phi,n.ahead = 30)
#predict trend manually
preds = predict(ksfit, newdata = last30)

predsFinal = preds + resids$f


plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), predsFinal, type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - predsFinal)^2,na.rm = TRUE)
ASE










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
fit = arima(cmort,order = c(phi$p,0,0), xreg = cbind(CM$temp1, CM$part, CM$Week, CM$FWeek))

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


#Find ASE  Need to forecast last 30 of known series.  
CMsmall = CM[2:478,]
ksfit = lm(cmort~temp1+part+Week+FWeek, data = CMsmall)
phi = aic.wge(ksfit$residuals)
fit = arima(CMsmall$cmort,order = c(phi$p,0,0), seasonal = list(order = c(1,0,0), period = 52), xreg = cbind(CMsmall$temp1, CMsmall$part, CMsmall$Week, CMsmall$FWeek))

last30 = data.frame(temp1 = CM$temp1[479:508], part = CM$part[479:508], Week = seq(479,508,1), FWeek = as.factor(seq(479,508,1)%%52))
#get predictions
predsCMort = predict(fit,newxreg = last30)  #Showing Error ... why we have to predict resids manually (next step)

#predict residuals manually
plotts.sample.wge(ksfit$residuals)
phi = aic.wge(ksfit$residuals)
resids = fore.arma.wge(ksfit$residuals,phi = phi$phi,n.ahead = 30)
#predict trend manually
preds = predict(ksfit, newdata = last30)

predsFinal = preds + resids$f


plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), predsFinal, type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - predsFinal)^2,na.rm = TRUE)
ASE







############ VAR MODELS ##########################

#VAR Model 1 Forecasts Seasonally Differenced Data 

#Difference all series to make them stationary (assumptoin of VAR)
# Doesn't have to be white... just stationary
library(vars)

CM = read.csv(file.choose(),header = TRUE)

CM_52 = artrans.wge(CM$cmort,c(rep(0,51),1))
Part_52 = artrans.wge(CM$part,c(rep(0,51),1))
Temp_52 = artrans.wge(CM$temp,c(rep(0,51),1))

#VARSelect on Differenced Data chooses 2
VARselect(cbind(CM_52, Part_52, Temp_52),lag.max = 10, type = "both")

#VAR with p = 2
CMortDiffVAR = VAR(cbind(CM_52, Part_52, Temp_52),type = "both",p = 2)
preds=predict(CMortDiffVAR,n.ahead=20)

#We have predicted differences .... calculate actual cardiac mortalities 
startingPoints = CM$cmort[456:475]
CMortForcasts = preds$fcst$CM_52[,1:3] + startingPoints

#Plot
dev.off()
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), as.data.frame(CMortForcasts)$fcst, type = "l", col = "red")


#Find ASE using last 30

CM_52 = artrans.wge(CMsmall$cmort,c(rep(0,51),1))
Part_52 = artrans.wge(CMsmall$part,c(rep(0,51),1))
Temp_52 = artrans.wge(CMsmall$temp,c(rep(0,51),1))

VARselect(cbind(CM_52, Part_52, Temp_52),lag.max = 10, type = "both")

CMortDiffVAR = VAR(cbind(CM_52, Part_52, Temp_52),type = "both",p = 2)
preds=predict(CMortDiffVAR,n.ahead=30)

startingPoints = CM$cmort[428:457]
CMortForcasts = preds$fcst$CM_52[,1:3] + startingPoints

dev.off()
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), CMortForcasts[,1], type = "l", col = "red")

ASE = mean((CM$cmort[479:508] - CMortForcasts[,1])^2)
ASE




#Find ASE using last 52

CMsmall = CM[1:456,]  # 456 = 508-52

CM_52 = artrans.wge(CMsmall$cmort,c(rep(0,51),1))
Part_52 = artrans.wge(CMsmall$part,c(rep(0,51),1))
Temp_52 = artrans.wge(CMsmall$temp,c(rep(0,51),1))

VARselect(cbind(CM_52, Part_52, Temp_52),lag.max = 10, type = "both")

CMortDiffVAR = VAR(cbind(CM_52, Part_52, Temp_52),type = "both",p = 2)
preds=predict(CMortDiffVAR,n.ahead=52)

startingPoints = CM$cmort[405:456]
CMortForcasts = preds$fcst$CM_52[,1:3] + startingPoints

dev.off()
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), CMortForcasts[,1], type = "l", col = "red")

ASE = mean((CM$cmort[457:508] - CMortForcasts[,1])^2)
ASE















#VAR Model 2 Forecasts Seasonal Dummy

CM = read.csv(file.choose(),header = TRUE)

#VARSelect on Seasonal Data chooses 2
VARselect(cbind(CM$cmort, CM$part, CM$temp),lag.max = 10, season = 52, type = "both")

#VAR with p = 2
CMortVAR = VAR(cbind(CM$cmort, CM$part, CM$temp),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=20)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), preds$fcst$y1[,1], type = "l", col = "red")


#Find ASE using last 30

CMsmall = CM[1:478,]

VARselect(cbind(CMsmall$cmort, CMsmall$part, CMsmall$temp),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort, CMsmall$part, CMsmall$temp),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=30)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - preds$fcst$y1[,1])^2)
ASE



#Find ASE using last 52

CMsmall = CM[1:456,]  # 456 = 508-52
VARselect(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp[2:456]),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp[2:456]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=52)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[457:508] - preds$fcst$y1[,1])^2)
ASE











#VAR Model 3 seasonal with Lag 1 Temp

CM = read.csv(file.choose(),header = TRUE)

CM$temp_1 = dplyr::lag(CM$temp,1)
ggpairs(CM[,-7])

VARselect(cbind(CM$cmort[2:479], CM$part[2:479], CM$temp_1[2:479]),lag.max = 10, season = 52, type = "both")

#VAR with p = 2
CMortVAR = VAR(cbind(CM$cmort[2:479], CM$part[2:479], CM$temp_1[2:479]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=20)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,528,1), preds$fcst$y1[,1], type = "l", col = "red")


#Find ASE using last 30

CMsmall = CM[1:479,]
VARselect(cbind(CMsmall$cmort[2:478], CMsmall$part[2:478], CMsmall$temp_1[2:478]),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort[2:478], CMsmall$part[2:478], CMsmall$temp_1[2:478]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=30)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - preds$fcst$y1[,1])^2)
ASE



#Find ASE using last 52

CMsmall = CM[1:456,]  # 456 = 508-52
VARselect(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp_1[2:456]),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp_1[2:456]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=52)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[457:508] - preds$fcst$y1[,1])^2)
ASE






















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



