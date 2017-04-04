"negloglik.laplace" <-
function(xpar, xx, ss, tlo, thi)
{
#
#  marginal negative log likelihood function for laplace prior
#  
#  xx data
#  xpar vector of two parameters:
#      xpar[1] :  w standardized to [0, 1] given a
#      xpar[2] :  scale factor a
#
	a <- xpar[2]
	# Calculate the range of w given a, using negative monotonicity between w and t
	wlo <- wfromt(thi, ss, a = a)
	whi <- wfromt(tlo, ss, a = a)
	wlo <- max(wlo)
	whi <- min(whi)
	loglik <- sum(log(1 + (xpar[1] * (whi - wlo) + wlo) * beta.laplace(xx, ss, a)))
	return( - loglik)
}