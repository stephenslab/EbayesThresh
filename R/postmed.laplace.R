"postmed.laplace" <-
function(x, s, w, a = 0.5)
{
#
# find the posterior median for the Laplace prior for 
#   given x (possibly vector), s (sd vector), w and a.
#
#  only allow a < 20 for input value.
  a <- min(a, 20)
#  Work with the absolute value of x, and for x > 25 use the approximation
#    to dnorm(x-a)*beta.laplace(x, a)
#
	sx <- sign(x)
	x <- abs(x)
	xma <- x/s - s*a
	zz <- (dnorm(xma) * (1/w + beta.laplace(x, s, a)))/a /s
	zz[xma > 25] <- 1/2
	mucor <- qnorm(pmin(zz, 1))
	muhat <- sx * pmax(0, xma - mucor) * s
	return(muhat)
}