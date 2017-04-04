"laplace.threshzero" <-
function(x, s, w, a = 0.5)
{
#
# the function that has to be zeroed to find the threshold with the Laplace
#    prior.  
#  only allow a < 20 for input value.
	a <- min(a, 20)
  xma <- x/s - s*a
	z <- pnorm(xma) - (dnorm(xma) * (1/w + beta.laplace(x, s, a)))/a/s
	return(z)
}