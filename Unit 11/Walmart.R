# Unit 11 Timer Series R Code

#Sunspot Analysis








## Walmart Analysis

# Two Frequencies at once using fore.aruma.wge()

# ONLY (1-B^365) Baseline
Walmart = read.csv(file.choose(), header = TRUE)
Store8Item50 = Walmart %>% filter(store == 8, item == 50)
plotts.sample.wge(Store8Item50$sales)
parzen.wge(Store8Item50$sales, trunc = 500)
f365 = fore.aruma.wge(Store8Item50$sales,s = 365,n.ahead = 365, lastn = TRUE)
ASE365 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f365$f)^2)
ASE365
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f365$f, col = "red")
f365 = fore.aruma.wge(Store8Item50$sales,s = 365, n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f365$f, trunc = 300)
#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f365$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f365$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f365$f[2:1999])




# ONLY (1-B^364) Baseline (WHy ill this work better)
Walmart = read.csv(file.choose(), header = TRUE)
Store8Item50 = Walmart %>% filter(store == 8, item == 50)
plotts.sample.wge(Store8Item50$sales)
f364 = fore.aruma.wge(Store8Item50$sales,s = 364,n.ahead = 365, lastn = TRUE)
ASE364 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364$f)^2)
ASE364
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f364$f, col = "red")
f364 = fore.aruma.wge(Store8Item50$sales,s = 364, n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f364$f, trunc = 300)
#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364$f[2:1999])





#Only (1-B^364) (1-B^7)  
f364_7 = fore.aruma.wge(Store8Item50$sales,s = 364, lambda = c(rep(0,6),1), n.ahead = 365, lastn = TRUE, limits = FALSE)
ASE364_7 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7$f)^2)
ASE364_7
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f364_7$f, col = "red")
parzen.wge(f364_7$f)
parzen.wge(f364_7$f, trunc = 300)
parzen.wge(f364_7$f, trunc = 100)
f364_7 = fore.aruma.wge(Store8Item50$sales,s = 364, lambda = c(rep(0,6),1), n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f364_7$f, trunc = 300)
#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364_7$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364_7$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364_7$f[2:1999])

#Whtie Noise Test
f364_7 = fore.aruma.wge(Store8Item50$sales,s = 364, lambda = c(rep(0,6),1), n.ahead = 365, lastn = TRUE, limits = FALSE)
plot((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7$f), type = "l")
ljung.wge((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7$f))




#Only (1-B^364)(1-B^7)(1-B)
f364_7_1 = fore.aruma.wge(Store8Item50$sales,d= 1, s = 364, lambda = c(rep(0,6),1), n.ahead = 365, lastn = TRUE, limits = FALSE)
ASE364_7_1 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7_1$f)^2)
ASE364_7_1
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f364_7_1$f, col = "red")
parzen.wge(f364_7_1$f)
parzen.wge(f364_7_1$f, trunc = 300)
parzen.wge(f364_7_1$f, trunc = 100)
f364_7_1 = fore.aruma.wge(Store8Item50$sales,d = 1, s = 364, lambda = c(rep(0,6),1), n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f364_7_1$f, trunc = 300)
#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364_7_1$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364_7_1$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364_7_1$f[2:1999])







#Only (1-B^364)(1-B)
f364_1 = fore.aruma.wge(Store8Item50$sales,d= 1, s = 364, lambda = c(), n.ahead = 365, lastn = TRUE, limits = FALSE)
ASE364_1 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_1$f)^2)
ASE364_1
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f364_1$f, col = "red")
parzen.wge(f364_1$f)
parzen.wge(f364_1$f, trunc = 300)
parzen.wge(f364_1$f, trunc = 100)
f364_1 = fore.aruma.wge(Store8Item50$sales,d = 1, s = 364, lambda = c(), n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f364_1$f, trunc = 300)

#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364_1$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364_1$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364_1$f[2:1999])

# With just Lambda Notation: Same result
lambda_coef = mult.wge(fac1 = 1, c(rep(0,363),1))
f364_1_Lambda = fore.aruma.wge(Store8Item50$sales, lambda = lambda_coef$model.coef, n.ahead = 365, lastn = TRUE, limits = FALSE)
ASE364_1_Lambda = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7_1_Lambda$f)^2)
ASE364_1_Lambda

#Whtie Noise Test
plot((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7_1_Lambda$f), type = "l")
ljung.wge((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7_1_Lambda$f))



#forecasts increase too fast.  Estimate desired slope and add to each forecast from (1-B^365)
# Back to (1-B^364) and we will adjust these forecasts to have an ajusted slope.  
Store8Item50$time = seq(1,length(Store8Item50$sales),1)
fit = lm(sales~time, data = Store8Item50)

newdata = data.frame(time = seq((length(Store8Item50$sales)+1),(length(Store8Item50$sales)+365)))
preds = predict(fit,newdata)
baseline = mean(Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)])
difference = preds - baseline
f364 = fore.aruma.wge(Store8Item50$sales,s = 364,n.ahead = 365, lastn = TRUE)
f364_adj = f364$f + difference 
ASE364_adj= mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_adj)^2)
ASE364_adj

#calculate adjusted forecasts
f364 = fore.aruma.wge(Store8Item50$sales,s = 364, n.ahead = 2000, lastn = FALSE, limits = FALSE)
newdata = data.frame(time = seq((length(Store8Item50$sales)+1),(length(Store8Item50$sales)+2000)))
preds = predict(fit,newdata)
baseline = mean(Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)])
difference = preds - baseline
f364_adj = f364$f + difference 
dataAndForecasts_adj = c(Store8Item50$sales,f364_adj)

# collect forecasts from (1-B^365) and (1-B^365)(1-B) models
f364 = fore.aruma.wge(Store8Item50$sales,s = 364, n.ahead = 2000, lastn = FALSE, limits = FALSE)
f364_1 = fore.aruma.wge(Store8Item50$sales,d = 1, s = 364, n.ahead = 2000, lastn = FALSE, limits = FALSE)
dataAndForecasts_364 = c(Store8Item50$sales,f364$f)
dataAndForecasts_364_1 = c(Store8Item50$sales,f364_1$f)

#plot all forecasts against each other
par(mfrow = c(3,1))
plot(seq(1,length(dataAndForecasts),1),dataAndForecasts_364, type = "l")
plot(seq(1,length(dataAndForecasts),1),dataAndForecasts_364_1, type = "l")
plot(seq(1,length(dataAndForecasts),1),dataAndForecasts_adj, type = "l")








#Full Model ARIMA(0,1,1) s = 364 .....  (1-B^364)(1-B)Xt = (1-theta1)at   ***
S8I50_1 = artrans.wge(Store8Item50$sales,1)
S8I50_1_364 = artrans.wge(S8I50_1,c(rep(0,363),1))
aic5.wge(S8I50_1_364)
estMA1 = est.arma.wge(S8I50_1_364,q = 1)
f364_7_1_MA1 = fore.aruma.wge(Store8Item50$sales,d= 1, s = 364, lambda = c(), theta = estMA1$theta, n.ahead = 365, lastn = TRUE, limits = FALSE)
ASE364_7_1_MA1 = mean((Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)] - f364_7_1_MA1$f)^2)
ASE364_7_1_MA1
plot(seq(1,365,1), Store8Item50$sales[(length(Store8Item50$sales)-364):length(Store8Item50$sales)], type = "l")
lines(f364_7_1_MA1$f, col = "red")
parzen.wge(f364_7_1_MA1$f)
parzen.wge(f364_7_1_MA1$f, trunc = 300)
parzen.wge(f364_7_1_MA1$f, trunc = 100)
f364_7_1_MA1 = fore.aruma.wge(Store8Item50$sales,s = 364, lambda = c(rep(0,6),1), n.ahead = 2000, lastn = FALSE, limits = FALSE)
parzen.wge(f364_7_1_MA1$f, trunc = 300)
#compare spectral densities and acfs
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364_7_1_MA1$f[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364_7_1_MA1$f[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364_7_1_MA1$f[2:1999])


# Smoothed Forecasts
n = 3
f364_7_1_MA1_Smoothed = stats::filter(f364_7_1_MA1$f,rep(1/n,n))
plot(seq(1,363,1), Store8Item50$sales[(length(Store8Item50$sales)-363):(length(Store8Item50$sales)-1)], type = "l")
lines(f364_7_1_MA1_Smoothed, col = "red")
ASE364_7_1_MA1_Smoothed = mean((Store8Item50$sales[(length(Store8Item50$sales)-363):(length(Store8Item50$sales)-1)] - f364_7_1_MA1_Smoothed[2:364])^2)
ASE364_7_1_MA1_Smoothed

#compare spectral densities and acfs
f364_7_1_MA1 = fore.aruma.wge(Store8Item50$sales,s = 364, lambda = c(rep(0,6),1), n.ahead = 2000, lastn = FALSE, limits = FALSE)
f364_7_1_MA1_Smoothed = stats::filter(f364_7_1_MA1$f,rep(1/n,n))
par(mfrow = c(3,2))
plot(seq(1,length(Store8Item50$sales),1),Store8Item50$sales, type = "l", main = "Sales Data")
plot(seq(1,1998,1),f364_7_1_MA1_Smoothed[2:1999], type = "l", main = "Forecasts")
parzen.wge(Store8Item50$sales, trunc = 500)
parzen.wge(f364_7_1_MA1_Smoothed[2:1999], trunc = 500)
acf(Store8Item50$sales)
acf(f364_7_1_MA1_Smoothed[2:1999])






