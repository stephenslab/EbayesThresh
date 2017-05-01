"ebayesthresh" <-
function(x, prior = "laplace", a = 0.5, bayesfac = FALSE, sdev = NA, verbose = FALSE, 
	threshrule = "median", threshrestriction = TRUE, normalization = FALSE)
{
#  Given a vector of data x, find the marginal maximum likelihood estimator
#   of the mixing weight w, and apply an appropriate thresholding rule using
#   this weight.
#  If the prior is laplace and a=NA, then the scale factor is also found by MML.
#   Heterogeneous variance is allowed only for laplace prior.
#  The thresholding rules allowed are "median", "mean", "hard", "soft" and "none";
#   if "none" is used, then only the parameters are worked out.
#  If hard or soft thresholding is used, the argument "bayesfac" specifies
#   whether to use the bayes factor threshold or the posterior median threshold.
#  If threshrestriction=TRUE, the universal bound of threshold will be implemented;
#   otherwise, weight w will be searched in [0, 1]
#  If normalization=TRUE, the observations and standard deviations will be first 
#   normalized by the mean of all standard deviations. In the case of homogeneous
#   variance, the standard deviations will be normalized to 1 automatically.
#  If verbose=TRUE then the routine returns a list with several arguments, including
#   muhat which is the result of the thresholding.
#  If verbose=FALSE then only muhat is returned.
#  It is assumed that the standard deviation of the data is sdev; if sdev=NA, then
#   it is estimated using the function mad(x).
#
#  find the standard deviation if necessary and estimate the parameters
  pr <- substring(prior, 1, 1)
  
  if(length(sdev)==1){
  	if(is.na(sdev)) { sdev <- rep(mad(x, center = 0), length(x))
  	} else { sdev <- rep(sdev, length(x))}
  } else{
    if(pr == "c") stop("Standard deviation has to be homogeneous for Cauchy prior.")
    if(length(sdev)!=length(x)) stop("Standard deviation has to be homogeneous or has the same length as observations.")
  }
  normalization_condition = (length(sdev)==1) | normalization
  if(normalization_condition){
    m_sdev <- mean(sdev)
    s <- sdev/m_sdev
    x <- x/m_sdev
  } else { s <- sdev }
  
	if((pr == "l") & is.na(a)) {
	 pp <- wandafromx(x, s, threshrestriction)
		w <- pp$w
		a <- pp$a
	}
	else w <- wfromx(x, s, prior = prior, a = a)
	if(pr != "m" | verbose) {
	  tt <- tfromw(w, s, prior = prior, bayesfac = bayesfac, a = a)
	  if(normalization_condition) {
	    tcor <- tt * m_sdev
	  } else { tcor <- tt }
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
	if(normalization_condition) {
	  muhat <- muhat * m_sdev
	}
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
