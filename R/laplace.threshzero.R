"laplace.threshzero" <-
function(x, s, w, a = 0.5)
{
#
#  The function that has to be zeroed to find the threshold with the Laplace
#   prior.
  # Only allow a < 20 for input value
	a <- min(a, 20)
  xma <- x/s - s*a
	z <- pnorm(xma) - 1/a * (1/s*dnorm(xma)) * (1/w + beta.laplace(x, s, a))
	return(z)
}