"postmean.laplace" <-
function(x, s, w, a = 0.5)
{
#
#  Find the posterior mean for the double exponential prior for 
#   given x, s (sd), w and a.
#
  # Only allow a < 20 for input value.
  a <- min(a, 20)
  # First find the probability of being non-zero
	wpost <- wpost.laplace(w, x, s, a)
	# Now find the posterior mean conditional on being non-zero
	sx <- sign(x)
	x <- abs(x)
	xpa <- x/s + s*a
	xma <- x/s - s*a
	xpa[xpa > 35] <- 35
	xma[xma < -35] <- -35
	
	cp1 <- pnorm(xma)
	cp2 <- pnorm( - xpa)
	ef <- exp(pmin(2 * a * x, 100))
	postmeancond <- x - a * s^2 * ( 2 * cp1/(cp1 + ef * cp2) - 1)	
  # Calculate posterior mean and return
	mutilde <- sx * wpost * postmeancond
	return(mutilde)
}
