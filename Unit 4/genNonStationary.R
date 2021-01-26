# Generating a Non-Stationary Time Series

vara = 1

n = 100

#phis = c(1.445,-.411,-.038,.170,.362,-.245,-.177,.213)
phis = .9

p = length(phis)

initial_values = runif(p,-10,10)

burn_in = 1000

Xt = numeric(burn_in+n)

Xt[1:p] = initial_values*phis

for (i in (p+1):(burn_in+n+p))
{
  Xt[i] = Xt[(i-1):(i-p)] * phis + rnorm(1,0,vara)
}

