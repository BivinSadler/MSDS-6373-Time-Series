---
title: "Unit 11 AcuSpike"
author: "Bivin"
date: "7/20/2019"
output: html_document
---

#Analysis of AcuSpike Web Hits

## Plot the Data
```{r}

AS = read.csv(file.choose(),header = TRUE)
plotts.sample.wge(AS$Active.Users)

```

## Ignore the Anomoly

```{r}
AS2 = AS[1:180,]
plotts.sample.wge(AS2$Active.Users)

```

#Evidence of Weekly Periodic Behavior
```{r}
AS2_S7 = artrans.wge(AS2$Active.Users,c(rep(0,6),1)) 
plotts.sample.wge(AS2_S7)
aic5.wge(AS2_S7, p = 1:10, q = 1:3)
aic5.wge(AS2_S7, p = 1:10, q = 1:3, type = "bic")
ARMA81 = est.arma.wge(AS2_S7,p = 8, q = 1)
AS2_S7_ARMA81 = artrans.wge(AS2_S7,phi.tr = ARMA81$phi)
ljung.wge(AS2_S7_ARMA81)
ljung.wge(AS2_S7_ARMA81, K = 48)

ASE = mean((AS2$Active.Users[(length(AS2$Active.Users)-14):length(AS2$Active.Users)] - f$f)^2)

ASE

#Compare Spectral Densities
sims = 5
SpecDen = parzen.wge(AS2$Active.Users, plot = "FALSE")
plot(SpecDen$freq,SpecDen$pzgram, type = "l", lwd = 6)

for( i in 1: sims)
{
   SpecDen2 = parzen.wge(gen.aruma.wge(180,s = 7, phi = ARMA81$phi, theta = ARMA81$theta, plot = "FALSE"), plot = "FALSE")
   lines(SpecDen2$freq,SpecDen2$pzgram, lwd = 2, col = "red")
}


#Compare ACFs
sims = 5
ACF = acf(AS2$Active.Users, plot = "FALSE")
plot(ACF$lag ,ACF$acf , type = "l", lwd = 6)

for( i in 1: sims)
{
   ACF2 = acf(gen.aruma.wge(180,s = 7,phi = ARMA81$phi, theta = ARMA81$theta, plot =    "FALSE"), plot = "FALSE")
   lines(ACF2$lag ,ACF2$acf, lwd = 2, col = "red")
}

#Make Final Forecasts
f = fore.aruma.wge(AS2$Active.Users,phi = ARMA81$phi, theta = ARMA81$theta, n.ahead = 10,limits = T, lastn = F)

```


data = gen.aruma.wge(1000, d = 1)