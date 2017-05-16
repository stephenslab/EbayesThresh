"tfromx" <-
function(x, s, prior = "laplace", bayesfac = FALSE, a = 0.5, universalthresh=TRUE)
{
#
#  Given the data x, the prior, and any other parameters, 
#   find the threshold corresponding to the marginal maximum likelihood estimator
#   of the mixing weight.
#
  pr <- substring(prior, 1, 1)
  if(pr == "c") s = 1
	if ( pr=="l" && is.na (a) ) 
	{ wa  <-  wandafromx(x, s, universalthresh)
	w  <-  wa$w
	a  <-  wa$a 
	}  	else	
	{w <- wfromx(x, s, prior = prior, a = a)}
	t <- tfromw(w, s, prior = prior, bayesfac = bayesfac, a = a)
	return(t)
}
