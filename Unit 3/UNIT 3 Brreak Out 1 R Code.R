#UNIT 3 Time Series


# Breakout 1

# Part 1
set.seed(2)
x = gen.sigplusnoise.wge(n = 200,coef = c(1,1),freq = c(.1,.4),vara = 5)
plotts.wge(x)
parzen.wge(x)

a = stats::filter(ts(x),rep(1,5))/5
plot(a,type = "l")
parzen.wge(as.numeric(a[!is.na(a)]))

dif = diff(x,lag = 1)
plot(dif,type = "l")
parzen.wge(dif)

xx = butterworth.wge(x,type = "low", cutoff = .45)
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 100)

xx = butterworth.wge(x,type = "pass", cutoff = c(.2,.45))
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 100)



#Part 2
set.seed(3)
x = gen.sigplusnoise.wge(n = 500,coef = c(1,1),freq = c(.15,.2),vara = 10)
y = gen.sigplusnoise.wge(n = 500,coef = c(1,0),freq = c(.32,0),vara = 10)
z = x+y
parzen.wge(z)

plotts.wge(z)

a = stats::filter(ts(z),rep(1,5))/5
plot(a,type = "l")
parzen.wge(as.numeric(a[!is.na(a)]), trunc = 100)

dif = diff(z,lag = 1)
plot(dif,type = "l")
parzen.wge(dif)

xx = butterworth.wge(z,order = 10,type = "pass",cutoff = c(.25,.38))
par(mfrow = c(1,1))
parzen.wge(xx$x.filt,trunc = 70)

