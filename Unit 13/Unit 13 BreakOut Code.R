#Sunspot Melenoma 

library(nnfor)
library(forecast)

### SUNSPOT DATA

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


#ensemble

ensemble = (fore.mlp$mean + pred$fcst$y1[,1])/2

plot(SM$Melanoma, type = "l")
lines(seq(30,37,1),ensemble,col = "green")

ASE = mean((SM$Melanoma[30:37] - ensemble)^2)
ASE







##### CARDIAC MORTALITY DATA    30 week forecast
library(tidyverse)
library(GGally)
library(astsa)
CM = read.csv(file.choose(),header = TRUE)

head(CM)
ggpairs(CM[2:4]) #matrix of scatter plots



#VAR Model 3 seasonal with Lag 1 Temp
CM$temp_1 = dplyr::lag(CM$temp,1)
ggpairs(CM)

VARselect(cbind(CM$cmort[2:508], CM$part[2:508], CM$temp[2:508]),lag.max = 10, season = 52, type = "both")

#VAR with p = 2
CMortVAR = VAR(cbind(CM$cmort[2:508], CM$part[2:508], CM$temp[2:508]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=30)

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,528), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(509,538,1), preds$fcst$y1[,1], type = "l", col = "red")


#Find ASE using last 30
CMsmall = CM[1:478,]

#Start and 2 since the lagged variable is NA at first index
VARselect(cbind(CMsmall$cmort[1:478], CMsmall$part[1:478], CMsmall$temp[1:478]),lag.max = 10, season = 52, type = "both")

CMortVAR = VAR(cbind(CMsmall$cmort[1:478], CMsmall$part[1:478], CMsmall$temp[1:478]),season = 52, type = "both",p = 2)
preds=predict(CMortVAR,n.ahead=30)


dev.off()
par(mfrow = c(2,1))
#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), preds$fcst$y1[,1], type = "l", col = "red")

#Plot
plot(seq(479,508,1), CM$cmort[479:508], type = "l",xlim = c(479,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), preds$fcst$y1[,1], type = "l", col = "red")


ASE = mean((CM$cmort[479:508] - preds$fcst$y1[,1])^2)
ASE




##### MLP MODEL FOR CARDIAC MORTALITY DATA
CMsmall = CMsmall[1:478,]
CMsmallDF = data.frame(Week = ts(CMsmall$Week),temp = ts(CMsmall$temp), part = ts(CMsmall$part), temp = ts(CMsmall$temp))
fit.mlp1 = mlp(ts(CMsmall$cmort),reps = 50,comb = "mean",xreg = CMsmallDF)
fit.mlp1
plot(fit.mlp1)
CMDF = data.frame(Week = ts(CM$Week),temp = ts(CM$temp), part = ts(CM$part), temp = ts(CM$temp))
fore.mlp1 = forecast(fit.mlp1, h = 30, xreg = CMDF)
plot(fore.mlp1)
ASE = mean((CM$cmort[479:508] - fore.mlp1$mean)^2)
ASE

dev.off()
par(mfrow = c(2,1))

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), fore.mlp1$mean, type = "l", col = "blue")

#Plot
plot(seq(479,508,1), CM$cmort[479:508], type = "l",xlim = c(479,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), fore.mlp1$mean, type = "l", col = "blue")



#Ensemble 

ensemble  = (preds$fcst$y1[,1] + fore.mlp1$mean)/2

dev.off()
par(mfrow = c(2,1))

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), ensemble, type = "l", col = "green")

#Plot
plot(seq(479,508,1), CM$cmort[479:508], type = "l",xlim = c(479,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(479,508,1), ensemble, type = "l", col = "green")


ASE = mean((CM$cmort[479:508] - ensemble)^2)
ASE








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




##### MLP MODEL FOR CARDIAC MORTALITY DATA
CMsmall = CMsmall[2:456,]
CMsmallDF = data.frame(Week = ts(CMsmall$Week,frequency = 52),temp = ts(CMsmall$temp,frequency = 52), part = ts(CMsmall$part,frequency = 52), temp_1 = ts(CMsmall$temp_1,frequency = 52))
fit.mlp2 = mlp(ts(CMsmall$cmort),reps = 50, comb = "median",xreg = CMsmallDF)
fit.mlp2
plot(fit.mlp2)
CMDF = data.frame(Week = ts(CM$Week),temp = ts(CM$temp), part = ts(CM$part), temp_1 = ts(CM$temp_1))
fore.mlp2 = forecast(fit.mlp2, h = 52, xreg = CMDF)
plot(fore.mlp2)
ASE = mean((CM$cmort[457:508] - fore.mlp2$mean)^2)
ASE

dev.off()

par(mfrow = c(2,1))
#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,510), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast", xlab = "Time")
lines(seq(457,508,1), fore.mlp2$mean, type = "l", col = "blue")

#Plot
plot(seq(457,508,1), CM$cmort[457:508], type = "l",xlim = c(457,510), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast", xlab = "Time")
lines(seq(457,508,1), fore.mlp2$mean, type = "l", col = "blue")



#Ensemble 

ensemble  = (preds$fcst$y1[,1] + fore.mlp2$mean)/2

#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,508), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast")
lines(seq(457,508,1), ensemble, type = "l", col = "green")


dev.off()

par(mfrow = c(2,1))
#Plot
plot(seq(1,508,1), CM$cmort, type = "l",xlim = c(0,510), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast", xlab = "Time")
lines(seq(457,508,1), ensemble, type = "l", col = "green")

#Plot
plot(seq(457,508,1), CM$cmort[457:508], type = "l",xlim = c(457,510), ylab = "Cardiac Mortality", main = "20 Week Cardiac Mortality Forecast", xlab = "Time")
lines(seq(457,508,1), ensemble, type = "l", col = "green")




ASE = mean((CM$cmort[457:508] - ensemble)^2)
ASE



#ts() experiment

a = gen.aruma.wge(1000,s = 12, phi = .9, sn = 1)

fit1 = mlp(ts(a),reps = 50,comb = "mean")
fit1
fit2 = mlp(ts(a,frequency = 12),reps = 50,comb = "mean")
fit2
fit3 = mlp(ts(a,frequency),m = 12,reps = 50,comb = "mean")
fit3


###  Link to Ensemble Study on tourist data

#ForecastXGB R package: 
  #http://freerangestats.info/blog/2016/11/06/forecastxgb
  #http://freerangestats.info/blog/2016/10/19/Tcomp
  
  
  