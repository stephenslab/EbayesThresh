"ebayesthresh" <-
function(x, prior = "laplace", a = 0.5, bayesfac = FALSE, sdev = NA, verbose = FALSE, 
	threshrule = "median")
{
#  Given a vector of data x, find the marginal maximum likelihood estimator
#   of the mixing weight w, and apply an appropriate thresholding rule using
#   this weight.
#  If the prior is laplace and a=NA, then the scale factor is also found by MML.
#  Heterogeneous variance is allowed only for laplace prior.
#  The thresholding rules allowed are "median", "mean", "hard", "soft" and "none";
#   if "none" is used, then only the parameters are worked out.
#  If hard or soft thresholding is used, the argument "bayesfac" specifies
#   whether to use the bayes factor threshold or the posterior median threshold.
#  If verbose=TRUE then the routine returns a list with several arguments, including
#   muhat which is the result of the thresholding.
#  If verbose=FALSE then only muhat is returned.
#  It is assumed that the standard deviation of the data is sdev; if sdev=NA, then
#   it is estimated using the function mad(x).
#
#  find the standard deviation if necessary and estimate the parameters
  if(length(sdev)==1){
  	if(is.na(sdev)) sdev <- mad(x, center = 0)
  } else{
    if(length(sdev)!=length(x)) stop("Standard deviation has to be homogeneous or has the same length as x.")
  }

  m_sdev <- mean(sdev)
  s <- sdev/m_sdev
  x <- x/m_sdev
  
	pr <- substring(prior, 1, 1)
	if((pr == "l") & is.na(a)) {
		pp <- wandafromx(x, s)
		w <- pp$w
		a <- pp$a
	}
	else w <- wfromx(x, s, prior = prior, a = a)	#
	if(pr != "m" | verbose) {
		tt <- tfromw(w, s, prior = prior, bayesfac = bayesfac, a = a)
		tcor <- m_sdev * tt
	}
	if(threshrule == "median")
		muhat <- postmed(x, s, w, prior = prior, a = a)
	if(threshrule == "mean")
		muhat <- postmean(x, s, w, prior = prior, a = a)
	if(threshrule == "hard")
		muhat <- threshld(x, tt)
	if(threshrule == "soft")
		muhat <- threshld(x, tt, hard = FALSE)
	if(threshrule == "none") muhat <- NA	#
# Now return desired output
#
	muhat <- m_sdev * muhat
	if(!verbose)
		return(muhat)
	retlist <- list(muhat = muhat, x = x, threshold.sdevscale = tt, 
		threshold.origscale = tcor, prior = prior, w = w, a = a, 
		bayesfac = bayesfac, sdev = sdev, threshrule = threshrule)
	if(pr == "c")
		retlist <- retlist[-7]
	if(threshrule == "none")
		retlist <- retlist[-1]
	return(retlist)
}
