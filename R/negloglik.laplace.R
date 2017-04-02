"negloglik.laplace" <-
function(xpar, xx, ss)
{
#
#  marginal negative log likelihood function for laplace prior
#  
#  xx data
#  xpar vector of two parameters:
#      xpar[1] :  threshold of minimum sd
#      xpar[2] :  scale factor a
#
	a <- xpar[2]
	w <- wfromt(xpar[1], min(ss), a = a)
	loglik <- sum(log(1 + w * matrix(beta.laplace(xx, ss, a), nrow=1)))
	return( - loglik)
}