postmed.laplace <- function(x, s = 1, w = 0.5, a = 0.5) {
#
#  Find the posterior median for the Laplace prior for 
#   given x (observations), s (sd), w and a.
#
    
  # Only allow a < 20 for input value
  a <- min(a, 20)
  
  # Work with the absolute value of x, and for x > 25 use the approximation
  #  to dnorm(x-a)*beta.laplace(x, a)
	sx <- sign(x)
	x <- abs(x)
	xma <- x/s - s*a
	#zz <- 1/a * (1/s)*dnorm(xma) * (1/w + beta.laplace(x, s, a))
	#zz[xma > 25] <- 1/2
	logzz = log(1/a) + log(1/s) + dnorm(xma,log=TRUE) + log(1/w + beta.laplace(x,s,a))
  logzz[xma>25] <- log(0.5)
#	mucor <- qnorm(pmin(zz, 1))
	mucor <- qnorm(pmin(logzz,0),log.p=TRUE)
	muhat <- sx * pmax(0, xma - mucor) * s
	return(muhat)
}
