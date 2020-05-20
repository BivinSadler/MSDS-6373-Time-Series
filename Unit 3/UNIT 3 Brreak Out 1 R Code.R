#UNIT 3 Time Series


# Breakout 1

# Part 1
set.seed(2)
x = gen.sigplusnoise.wge(n = 200,coef = c(1,1),freq = c(.1,.4),vara = 5)
plotts.wge(x)
parzen.wge(x)

a = stats::filter(ts(x),rep(1,5))/5
plot(a,type = "l")
parzen.wge(na.omit(a))

dif = diff(x,lag = 1)
plot(dif,type = "l")
parzen.wge(dif)

xx = butterworth.wge(x,order = 1, type = "low", cutoff = .2)
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 100)

xx = butterworth.wge(x,type = "pass", cutoff = c(.3,.45))
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 100)



#Part 2
set.seed(3)
x = gen.sigplusnoise.wge(n = 500,coef = c(1,1),freq = c(.15,.2),vara = 10)
y = gen.sigplusnoise.wge(n = 500,coef = c(1,0),freq = c(.32,0),vara = 10)
z = x+y
plotts.wge(z)
parzen.wge(z)


a = stats::filter(ts(z),rep(1,5))/5
plot(a,type = "l")
parzen.wge(na.omit(a), trunc = 100)

dif = diff(z,lag = 1)
plot(dif,type = "l")
parzen.wge(dif)

xx = butterworth.wge(z,order = 10,type = "low",cutoff = c(.3))
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 70)

xx = butterworth.wge(z,order = 10,type = "pass",cutoff = c(.1,.3))
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 70)




#Walmart Analysis 
Walmart = read.csv(file.choose(),header = TRUE)
Store8Item50 = Walmart %>% filter(store == "8", item == "50")
plotts.wge(Store8Item50$sales)
parzen.wge(na.omit(Store8Item50$sales))
parzen.wge(na.omit(Store8Item50$sales), trunc = 500)


Store8Item50_MA_5 = stats::filter(Store8Item50$sales,rep(1,5)/5)
Store8Item50_MA_51 = stats::filter(Store8Item50$sales,rep(1,51)/51)
plotts.sample.wge(na.omit(Store8Item50_MA_5))
plotts.sample.wge(na.omit(Store8Item50_MA_51))
parzen.wge(na.omit(Store8Item50$sales), trunc = 500)

Store8Item50_Diff_1 = diff(Store8Item50$sales,lag = 1)
plotts.sample.wge(na.omit(Store8Item50_Diff_1))
parzen.wge(na.omit(Store8Item50_Diff_1), trunc = 500)

  #Walmart Butterworth

xx = butterworth.wge(Store8Item50$sales,order = 10,type = "pass",cutoff = c(.1,.2))
plotts.wge(xx$x.filt)
parzen.wge(xx$x.filt,trunc = 70)

xx = butterworth.wge(Store8Item50$sales,order = 10,type = "low",cutoff = c(.01))
plotts.wge(xx$x.filt)
parzen.wge(xx$x.filt,trunc = 70)
parzen.wge(xx$x.filt,trunc = 300)



# Analyze gammo_0 = sigma^2_Xt = variance of Xt from AR(1) with phi = .7

x = gen.arma.wge(10000,phi = .7, vara = 1, sn = 5)
SigSq_Hat_Xt = var(x)
SigSq_Xt  = 1/(1-.7^2)

SigSq_Hat_Xt
SigSq_Xt


