#(1-.9B)(1-.8B)Xt = at
#(1-1.7B+.72B^2)Xt = at
#Phi_1 = 1.7  Phi_2 = -.72

s = c(5,8,9,8,7,6,4,3)

#double check psi weights
psi.weights.wge(phi = c(1.7, -.72), lag = 5)

#AR(2)

fit = fore.arma.wge(s,phi = c(1.7, -.72), n.ahead = 3)

#forecasts for l = 1,2 and 3
fit$f

#Conf limits for l = 3
# ll:   1.75415 - 1.96*sqrt(fit$wnv)*sqrt(1+1.7^2+ 2.17^2)
# ul:   1.75415 + 1.96*sqrt(fit$wnv)*sqrt(1+1.7^2+ 2.17^2)
fit$ll[3]
fit$ul[3]

#sigma_at_hat
sqrt(fit$wnv)

#Calc sigma_at_hat
wnv = 1/(8-2) * sum(fit$resid[3:8]^2)