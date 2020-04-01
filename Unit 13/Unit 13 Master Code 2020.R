#More on mlp() from the author: https://kourentzes.com/forecasting/2017/02/10/forecasting-time-series-with-neural-networks-in-r/

#Sunspot Melenoma 

library(nnfor)
library(forecast)
library(vars)
library(tswge)

### SUNSPOT DATA ... This is purely an academic exercise. This assumes you know the sunspot data for your forecasts which is a poor assumpiton.  
# One way to do this in practice would be to first forecast the sunspot data... this will of course increase your uncertainty
# in your forecasts and likely degrade the forecasts themselves.  

#VAR
SM = read.csv(file.choose(),header = TRUE)

SMsmall = SM[1:29,]

VAR_SM = VAR(cbind(SMsmall$Melanoma,SMsmall$Sunspot),lag.max = 5, type = "both")

pred = predict(VAR_SM,n.ahead = 8)

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),pred$fcst$y1[,1],col = "red")

ASE = mean((SM$Melanoma[30:37] - pred$fcst$y1[1:8])^2)

ASE


#MLP


SMsmallDF = data.frame(Sunspot = ts(SMsmall$Sunspot))

#Using Known Sunspots
fit.mlp = mlp(ts(SMsmall$Melanoma),reps = 50,comb = "mean",xreg = SMsmallDF)
fit.mlp
plot(fit.mlp)
SMDF = data.frame(Sunspot = ts(SM$Sunspot))
fore.mlp = forecast(fit.mlp, h = 8, xreg = SMDF)
plot(fore.mlp)

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),fore.mlp$mean,col = "blue")

ASE = mean((SM$Melanoma[30:37] - fore.mlp$mean)^2)
ASE


#Using forecast Sunspots
fit.mlp.SP = mlp(ts(SMsmallDF$Sunspot),reps = 50, comb = "mean")
fore.mlp.SP = forecast(fit.mlp.SP, h = 8)

SMsmallDF_fore = data.frame(Sunspot = ts(fore.mlp.SP$mean))
SMsmallDF_fore

fit.mlp = mlp(ts(SMsmall$Melanoma),reps = 50,comb = "mean",xreg = SMsmallDF)
fit.mlp
plot(fit.mlp)
SMDF = data.frame(Sunspot = ts(c(SMsmallDF$Sunspot,SMsmallDF_fore$Sunspot)))
fore.mlp = forecast(fit.mlp, h = 8, xreg = SMDF)
plot(fore.mlp)

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),fore.mlp$mean,col = "blue")

ASE = mean((SM$Melanoma[30:37] - fore.mlp$mean)^2)
ASE


#ensemble

ensemble = (fore.mlp$mean + pred$fcst$y1[,1])/2

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),ensemble,col = "green")

ASE = mean((SM$Melanoma[30:37] - ensemble)^2)
ASE


#### ABOVE IS MODEL SELECTION / FITTING

### NOW LETS FORECAST!!!

#UNDER CONSTRUCTION
fit.mlp = mlp(ts(SM$Melanoma),reps = 50,comb = "mean",xreg = SM)
fit.mlp
plot(fit.mlp)
SMDF = data.frame(Sunspot = ts(c(SM$Sunspot,SMsmallDF_fore$Sunspot)))
fore.mlp = forecast(fit.mlp, h = 8, xreg = SMDF)
plot(fore.mlp)








##### CARDIAC MORTALITY DATA    52 week forecast
library(tidyverse)
library(GGally)
library(astsa)
CM = read.csv(file.choose(),header = TRUE)

head(CM)
ggpairs(CM[2:4]) #matrix of scatter plots



#VAR Model 3 seasonal with Lag 1 Temp
CM$temp_1 = dplyr::lag(CM$temp,1)
ggpairs(CM)

VARselect(cbind(CM$cmort[2:508], CM$part[2:508], CM$temp_1[2:508]),lag.max = 10, season = 52, type = "both")

#VAR with p = 2
CMortVAR = VAR(cbind(CM$cmort[2:508], CM$part[2:508], CM$temp_1[2:508]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=52)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,565), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,560,1), preds$fcst$y1[,1], type = "l", col = "red")



#Find ASE using last 52
CMsmall = CM[1:456,]

#Start and 2 since the lagged variable is NA at first index
VARselect(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp_1[2:456]),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort[2:456], CMsmall$part[2:456], CMsmall$temp_1[2:456]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=52)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,510), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[457:508] - preds$fcst$y1[,1])^2)
ASE








##### MLP MODEL FOR CARDIAC MORTALITY DATA ... if you knew the temp and particles ... Unrealistic but added for comparison 
CMsmall = CM[1:456,]
CMsmallDF = data.frame(Week = ts(CMsmall$Week),temp = ts(CMsmall$temp), part = ts(CMsmall$part))
fit.mlp = mlp(ts(CMsmall$cmort),reps = 20,comb = "mean",xreg = CMsmallDF)
fit.mlp
plot(fit.mlp)
CMDF = data.frame(Week = ts(CM$Week),temp = ts(CM$temp), part = ts(CM$part))
fore.mlp = forecast(fit.mlp, h = 52, xreg = CMDF)
plot(fore.mlp)
ASE = mean((CM$cmort[457:508] - fore.mlp$mean)^2)
ASE

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,510), ylab = "Cardiac Mortality", main = "52 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), fore.mlp$mean, type = "l", col = "red")



#Ensemble 

ensemble  = (preds$fcst$y1[,1] + fore.mlp$mean)/2

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "52 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), ensemble, type = "l", col = "green")

ASE = mean((CM$cmort[457:508] - ensemble)^2)
ASE








##### MLP MODEL FOR CARDIAC MORTALITY DATA ... if you do not know the temp and particles

CMsmall = CM[1:456,]

#forecast temp and particles

#temp
fit.mlp.temp = mlp(ts(CMsmall$temp,frequency = 52),reps = 50, comb = "median")
plot(fit.mlp.temp)
fore.mlp.temp = forecast(fit.mlp.temp, h = 52)
plot(fore.mlp.temp)

#particles
fit.mlp.part = mlp(ts(CMsmall$part,frequency = 52),reps = 50, comb = "median")
fore.mlp.part = forecast(fit.mlp.part, h = 52)
plot(fore.mlp.part)


#package them up in data frame.
fore.mlp.temp1 = dplyr::lag(as.vector(fore.mlp.temp$mean),1)
CMDF_fore = data.frame(Week = ts(seq(2,508,1)),temp = ts(c(CMsmall$temp,fore.mlp.temp$mean)), part = ts(c(CMsmall$part,fore.mlp.part$mean)))
CMDF_fore


#forecast cmort using mlp with forecasted xreg (don't need to forecast week.)
CMsmallDF = data.frame(Week = ts(CMsmall$Week),temp = ts(CMsmall$temp), part = ts(CMsmall$part))
fit.mlp = mlp(ts(CMsmall$cmort, frequency = 52),reps = 50,comb = "mean",xreg = CMsmallDF)
fit.mlp
plot(fit.mlp)
#CMDF = data.frame(Week = ts(CM$Week),temp = ts(CM$temp), part = ts(CM$part), temp_1 = ts(CM$temp_1))
fore.mlp = forecast(fit.mlp, h = 52, xreg = CMDF_fore)
plot(fore.mlp)
ASE = mean((CM$cmort[457:508] - fore.mlp$mean)^2)
ASE

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,510), ylab = "Cardiac Mortality", main = "52 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), fore.mlp$mean, type = "l", col = "red")



#Ensemble 

ensemble  = (preds$fcst$y1[,1] + fore.mlp$mean)/2

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), ensemble, type = "l", col = "green")

ASE = mean((CM$cmort[457:508] - ensemble)^2)
ASE



















###  Link to Ensemble Study on tourist data

#ForecastXGB R package: 
#http://freerangestats.info/blog/2016/11/06/forecastxgb
#http://freerangestats.info/blog/2016/10/19/Tcomp
# The Theta Method: https://robjhyndman.com/papers/Theta.pdf






#### Sunspot.month traditional(Univariate)
  
### ARIMA s = 131

plotts.wge(sunspot.month)
plotts.sample.wge(sunspot.month)

ss_131 = artrans.wge(sunspot.month,c(rep(0,131),1))
ss_131_1 = artrans.wge(ss_131,1)
aic5.wge(ss_131_1)
aic5.wge(ss_131_1, type = "bic")
parzen.wge(ss_131_1)
ljung.wge(ss_131_1)
est_4_2 = est.arma.wge(ss_131_1, p = 4, q = 4)
ss_131_1_4_2 = artrans.wge(ss_131_1,phi.tr = est_4_2$phi)

#forecast with ss_131 approx 10.9 year period
fore_131_1 = fore.aruma.wge(sunspot.month,phi = est_4_2$phi, theta = est_4_2$theta,d = 1, s = 131,n.ahead = 600, limits = F, lastn = TRUE)

ASE = mean((sunspot.month[(3177-600+1):3177] - fore_131_1$f)^2)
ASE



### ARIMA s = 984 and s = 131

plotts.wge(sunspot.month)
plotts.sample.wge(sunspot.month)

ss_984 = artrans.wge(sunspot.month,c(rep(0,983),1))
parzen.wge(ss_984, trunc = 300)

ss_984_131 = artrans.wge(ss_984,c(rep(0,130),1))
parzen.wge(ss_984_131, trunc = 300)

ss_984_131_1 = artrans.wge(ss_984_131,1)

aic5.wge(ss_984_131_1)
aic5.wge(ss_984_131_1, type = "bic")
parzen.wge(ss_984_131_1)
ljung.wge(ss_984_131_1)

est_2_1 = est.arma.wge(ss_984_131_1, p = 2, q = 1)
ss_984_131_1_AR_2_1 = artrans.wge(ss_984_131_1,phi.tr = est_2_1$phi)
ljung.wge(ss_984_131_1_AR_2_1)


#forecast with ss_984 approx 82 year period and s = 131 (10.9 year period)
sss = mult.wge(fac1 = c(rep(0,130),1), fac2 = c(rep(0,983),1))
fore_984_131_1_AR_2_1 = fore.aruma.wge(sunspot.month,lambda = sss$model.coef, phi = est_2_1$phi, theta = est_2_1$theta, n.ahead = 600, limits = F, lastn = TRUE)

ASE = mean((sunspot.month[(3177-600+1):3177] - fore_984_131_1_AR_2_1$f)^2)
ASE






### ARIMA s = 984

plotts.wge(sunspot.month)
plotts.sample.wge(sunspot.month)

ss_984 = artrans.wge(sunspot.month,c(rep(0,983),1))
parzen.wge(ss_984, trunc = 300)

aic5.wge(ss_984)
aic5.wge(ss_984, type = "bic")
parzen.wge(ss_984_131_1)
ljung.wge(ss_984_131_1)

est_4_2 = est.arma.wge(ss_984, p = 4, q = 2)
ss_984_AR_4_2 = artrans.wge(ss_984_131_1,phi.tr = est_2_1$phi)
ljung.wge(ss_984_AR_4_2)


#forecast with ss_984 approx 82 year period
fore_984_AR_4_2 = fore.aruma.wge(sunspot.month,s = 984, phi = est_4_2$phi, theta = est_4_2$theta, n.ahead = 600, limits = F, lastn = TRUE)

ASE = mean((sunspot.month[(3177-600+1):3177] - fore_984_AR_4_2$f)^2)
ASE




### MLP

fit.mlp = mlp(ts(sunspot.month),reps = 20,comb = "mean")
fit.mlp
plot(fit.mlp)
fore.mlp = forecast(fit.mlp, h = 600)
plot(fore.mlp)





