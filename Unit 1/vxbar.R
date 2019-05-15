# vxbar: variance of a stationary time series where autocorrelation approaches zero (from section 1.9) 
# requires:
# NUMERIC variables
library(tidyverse)
# for purrr and %>%

# Code
vxbar  <- function(vec){
	vec %>%
		discard(is.na) -> vec
	n  <-  length(vec)
	nlag  <-  n-1
	m  <- mean(vec)
	v  <-  var(vec)
	g0  <- v*nlag/n
	aut  <-  acf(vec,lag.max = nlag)
	sum  <- 0
	for (k in 1:nlag){
		sum  <- sum + (1-k/n)*aut$acf[k+1]*g0
	}
	 return(2*sum/n + g0/n)
}

# example
# brate = read.csv("../../Data/10_year_bond_rate_2010-2015.csv")
# x <- brate$Adj.Close
# vxbar(x)

# vxbarci 
# returns a lovely confidence interval on vxbar
# requires vxbar and all its dependencies

vxbarci  <- function(vec){
	rhs  <- 1.96*sqrt(vxbar(vec))
	lhs  <- mean(x)
	return(c(lhs-rhs, lhs+rhs))
}

# Example
# vxbarci(x)
