"negloglik.laplace" <-
function(xpar, xx, ss, tlo, thi)
{
#
#  Marginal negative log likelihood function for laplace prior
#  
#  xx   :data
#  xpar :vector of two parameters:
#      xpar[1] :  a value between [0, 1] which will be adjusted to the range of w 
#      xpar[2] :  scale factor a
#  ss   :vector of standard deviations
#  tlo  :lower bound of thresholds
#  thi  :upper bound of thresholds
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
