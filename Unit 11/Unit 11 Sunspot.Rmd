---
title: "Unit 11 Sunspot"
author: "Bivin"
date: "7/19/2019"
output: html_document
---

# Non Stationary 
## Seasonal ARMA(3,0) s = 10
```{r}
#SS = read.csv(file.choose(),header = TRUE)
library(tswge)
getwd()
SS = read.csv("/Users/bivinsadler/Box/Time Series/Summer 2020/Units/Unit 11/SunspotSummer2020.csv", header = TRUE)
head(SS)
plotts.sample.wge(SS$SunSpots)
est.ar.wge(SS$SunSpots,p = 12) #overfit for seasonality
SS_10 = artrans.wge(SS$SunSpots,phi.tr = c(rep(0,9),1))
aic5.wge(SS_10)
aic5.wge(SS_10,type = "bic")
AR3 = est.ar.wge(SS_10,p = 3)
SS_10_AR3 = artrans.wge(SS_10,AR3$phi)
ljung.wge(SS_10_AR3)
ljung.wge(SS_10_AR3, K = 48)
f = fore.aruma.wge(SS$SunSpots,s = 10,phi = AR3$phi,n.ahead = 15,limits = F, lastn = T)

ASE = mean((SS$SunSpots[(length(SS$SunSpots)-14):length(SS$SunSpots)] - f$f)^2)

ASE

#Compare Spectral Densities
sims = 5
SpecDen = parzen.wge(SS$SunSpots, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)

for( i in 1: sims)
{
   SpecDen2 = parzen.wge(gen.aruma.wge(319,s = 10, phi = AR3$phi, plot ="FALSE"), plot = "FALSE")
   lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}


#Compare ACFs
sims = 5
ACF = acf(SS$SunSpots, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)

for( i in 1: sims)
{
   ACF2 = acf(gen.aruma.wge(319, s = 10, phi = AR3$phi, plot =    "FALSE"), plot = "FALSE")
   lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

#Compare Generated Realizations 


S10AR3gen = gen.aruma.wge(319,s = 10,phi = AR3$phi, vara = AR3$avar)

plotts.sample.wge(S10AR3gen)

```


# Non Stationary
## Seasonal s = 11  ARMA(2,0) s = 11
```{r}
plotts.sample.wge(SS$SunSpots)
est.ar.wge(SS$SunSpots,p = 12) #overfit for seasonality
SS_11 = artrans.wge(SS$SunSpots,phi.tr = c(rep(0,10),1))
aic5.wge(SS_11)
aic5.wge(SS_11, type = "bic")
AR2 = est.arma.wge(SS_11,p = 2,q = 0)
SS_11_AR2 = artrans.wge(SS_11,AR2$phi)
ljung.wge(SS_11_AR2)
ljung.wge(SS_11_AR2, K = 48)
f = fore.aruma.wge(SS$SunSpots,s = 11,phi = AR2$phi,n.ahead = 15,limits = F,lastn = TRUE)

ASE = mean((SS$SunSpots[(length(SS$SunSpots)-14):length(SS$SunSpots)] - f$f)^2)

ASE

#Compare Spectral Densities
sims = 5
SpecDen = parzen.wge(SS$SunSpots, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)

for( i in 1: sims)
{
   SpecDen2 = parzen.wge(gen.aruma.wge(289,s = 11, phi = AR2$phi, theta = AR2$theta, plot = "FALSE"), plot = "FALSE")
   lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}


#Compare ACFs
sims = 5
ACF = acf(SS$SunSpots, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)

for( i in 1: sims)
{
   ACF2 = acf(gen.aruma.wge(289, s = 11, phi = AR2$phi, theta = AR2$theta, plot =    "FALSE"), plot = "FALSE")
   lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

#Compare Generated Realizations 

S11ARMA31gen = gen.aruma.wge(160,s = 11,phi = AR2$phi, theta = AR2$theta, vara = AR2$avar)

plotts.sample.wge(S11ARMA31gen)

```


#Stationary 
##ARMA(3,0)
```{r}
plotts.sample.wge(SS$SunSpots)
aic5.wge(SS$SunSpots)
aic5.wge(SS$SunSpots, type = "bic")
AR3 = est.arma.wge(SS$SunSpots,p = 3) #avar 645.5
SS_AR3 = artrans.wge(SS$SunSpots,AR3$phi)
ljung.wge(SS_AR3)
ljung.wge(SS_AR3, K = 48)
f = fore.arma.wge(SS$SunSpots,phi = AR3$phi, n.ahead = 50,limits = F, lastn = F)

ASE = mean((SS$SunSpots[(length(SS$SunSpots)-14):length(SS$SunSpots)] - f$f)^2)

ASE

#Compare Spectral Densities
sims = 5
SpecDen = parzen.wge(SS$SunSpots, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)

for( i in 1: sims)
{
   SpecDen2 = parzen.wge(gen.arma.wge(289,phi = AR3$phi, plot =    "FALSE"), plot = "FALSE")
   lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}


#Compare ACFs
sims = 20
ACF = acf(SS$SunSpots, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)

for( i in 1: sims)
{
   ACF2 = acf(gen.arma.wge(319,phi = AR3$phi, plot =    "FALSE"), plot = "FALSE")
   lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

#Compare Generated Realizations 
         
AR3gen = gen.arma.wge(160,phi = AR3$phi, vara = AR3$avar)

plotts.sample.wge(AR3gen)


#Make Final Forecasts
f = fore.arma.wge(SS$SunSpots,phi = AR3$phi, n.ahead = 15,limits = F, lastn = F)


```

# Non Stationary
## Seasonal s = 83  ARMA(2,0) s = 83
```{r}
plotts.sample.wge(SS$SunSpots)
#est.ar.wge(SS$SunSpots,p = 12) #overfit for seasonality
SS_83 = artrans.wge(SS$SunSpots,phi.tr = c(rep(0,82),1))
aic5.wge(SS_83)
aic5.wge(SS_83, type = "bic") #p = 3 q = 0
AR3 = est.arma.wge(SS_83,p = 3,q = 0)
SS_83_AR3 = artrans.wge(SS_83,AR3$phi)
ljung.wge(SS_83_AR3)
ljung.wge(SS_83_AR3, K = 48)
f = fore.aruma.wge(SS$SunSpots,s = 83,phi = AR3$phi,n.ahead = 15,limits = F,lastn = TRUE)

ASE = mean((SS$SunSpots[(length(SS$SunSpots)-14):length(SS$SunSpots)] - f$f)^2)

ASE

#Compare Spectral Densities
sims = 5
SpecDen = parzen.wge(SS$SunSpots, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)

for( i in 1: sims)
{
   SpecDen2 = parzen.wge(gen.aruma.wge(289,s = 83, phi = AR3$phi, theta = AR3$theta, plot = "FALSE"), plot = "FALSE")
   lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}


#Compare ACFs
sims = 5
ACF = acf(SS$SunSpots, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)

for( i in 1: sims)
{
   ACF2 = acf(gen.aruma.wge(289, s = 13, phi = AR3$phi, theta = AR3$theta, plot =    "FALSE"), plot = "FALSE")
   lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

#Compare Generated Realizations 

S83AR3gen = gen.aruma.wge(160,s = 83,phi = AR3$phi, theta = AR3$theta, vara = AR3$avar)

plotts.sample.wge(S83AR3gen)

```
