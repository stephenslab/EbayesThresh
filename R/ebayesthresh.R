ebayesthresh <- function (x, prior = "laplace", a = 0.5, bayesfac = FALSE,
                          sdev = NA, verbose = FALSE, threshrule = "median",
                          universalthresh = TRUE,  stabadjustment) {
#  
#  Given a vector of data x, find the marginal maximum likelihood
#  estimator of the mixing weight w, and apply an appropriate
#  thresholding rule using this weight.
#
#  If the prior is laplace and a=NA, then the scale factor is also
#  found by MML.
#
#  Standard deviation sdev can be a vector (heterogeneous variance) or
#  a single value (homogeneous variance). If sdev=NA, then it is
#  estimated using the function mad(x). Heterogeneous variance is
#  allowed only for laplace prior currently.
#    
#  The thresholding rules allowed are "median", "mean", "hard", "soft"
#  and "none"; if "none" is used, then only the parameters are worked
#  out.
#
#  If hard or soft thresholding is used, the argument "bayesfac"
#  specifies whether to use the bayes factor threshold or the
#  posterior median threshold.
#    
#  If universalthresh=TRUE, the thresholds will be upper bounded by
#  universal threshold adjusted by standard deviation; otherwise,
#  weight w will be searched in [0, 1].
#    
#  If stabadjustment=TRUE, a stability adjustment is made.
#  The observations and standard deviations
#  will be first divided by the mean of the standard deviations
#  to reduce inefficiency due to large value of standard
#  deviation. In the case of homogeneous variance, the standard
#  deviations will be normalized to 1 automatically. Output (posterior mean/median)
#  is then scaled at end so results should remain the same.  
#
#  
#  If verbose=TRUE then the routine returns a list with several
#  arguments, including muhat which is the result of the
#  thresholding. If verbose=FALSE then only muhat is returned.
#
    
  # Find the standard deviation if necessary and estimate the parameters
  pr <- substring(prior, 1, 1)

  
  if(length(sdev) == 1) {
      if(!missing(stabadjustment))
        stop(paste("Argument stabadjustment is not applicable when",
                   "variances are homogeneous."))
      if(is.na(sdev)) {
          sdev <- mad(x, center = 0)
      }
      stabadjustment_condition = TRUE
  } else{
    if(pr == "c")
      stop("Standard deviation has to be homogeneous for Cauchy prior.")
    if(length(sdev) != length(x))
      stop(paste("Standard deviation has to be homogeneous or has the",
                 "same length as observations."))
    if(missing(stabadjustment))
      stabadjustment <- FALSE
    stabadjustment_condition = stabadjustment
  }
  
  if (stabadjustment_condition) {
    m_sdev <- mean(sdev)
    s <- sdev/m_sdev
    x <- x/m_sdev
  } else { s <- sdev }
  
	if ((pr == "l") & is.na(a)) {
	  if(universalthresh){
	    pp <- wandafromx(x, s, universalthresh)
	  } else {
	    pp <- wandafromx.mle(x, s) 
	  } 
    w  <- pp$w
    a  <- pp$a
	}
	else
          w <- wfromx(x, s, prior = prior, a = a, universalthresh)
	if(pr != "m" | verbose) {
	  tt <- tfromw(w, s, prior = prior, bayesfac = bayesfac, a = a)
	  if(stabadjustment_condition) {
	    tcor <- tt * m_sdev
          } else {
            tcor <- tt
          }
	}
	
	if ((pr == "l")){
	  loglik = loglik.laplace(x,s,a,w) #sum(log(1+w*beta.laplace(x,s,a))) + sum(dnorm(x,0,s,log=TRUE)) # value of log-likelihood achieved
	} else {
	  loglik = NA #log likelihood only implemented for point-laplace prior
	}
	
	if ((pr == "l")){
	  postmean2 = postmean2.laplace(x,s,w,a)
	} else {
	  postmean2 = NA #log likelihood only implemented for point-laplace prior
	}
	
	if(threshrule == "median")
		muhat <- postmed(x, s, w, prior = prior, a = a)
	if(threshrule == "mean")
    muhat <- postmean(x, s, w, prior = prior, a = a)
	if(threshrule == "hard")
		muhat <- threshld(x, tt)
	if(threshrule == "soft")
		muhat <- threshld(x, tt, hard = FALSE)
        if(threshrule == "none")
                muhat <- NA

	# Now return desired output
	if(stabadjustment_condition) {
	  muhat <- muhat * m_sdev
	  postmean2 <- postmean2 * m_sdev^2
	}
	if(!verbose)
            return(muhat)
	retlist <- list(muhat = muhat, x = x, threshold.sdevscale = tt, 
                        threshold.origscale = tcor, prior = prior, w = w,
                        a = a, bayesfac = bayesfac, sdev = sdev,
                        threshrule = threshrule, loglik = loglik, postmean2 = postmean2)
	if(pr == "c")
		retlist <- retlist[-7]
	if(threshrule == "none")
		retlist <- retlist[-1]
	
	return(retlist)
}
