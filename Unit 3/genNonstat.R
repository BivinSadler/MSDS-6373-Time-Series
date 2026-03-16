library(tswge)
gen.nonstat  <- function(n,phi){
	x <- rep(0,n)
	a <- rnorm(n)
	x[1:n]  <- 0
	for(k in 2:n){
		x[k] = phi*x[k-1] +a[k]
	}
	plotts.wge(x)
}

gen.nonstat(n = 50, phi = 1.5)
