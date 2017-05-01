"tfromx" <-
function(x, sdev=NA, prior = "laplace", bayesfac = FALSE, a = 0.5, universalthresh=TRUE, stabadjustment=FALSE)
{
#  given the data x, the prior, and any other parameters, 
#   find the threshold
#   corresponding to the marginal maximum likelihood estimator
#   of the mixing weight.
#
  pr <- substring(prior, 1, 1)
  if(length(sdev)==1){
    if(is.na(sdev)) { sdev <- rep(mad(x, center = 0), length(x))
    } else { sdev <- rep(sdev, length(x))}
  } else{
    if(pr == "c") stop("Standard deviation has to be homogeneous for Cauchy prior.")
    if(length(sdev)!=length(x)) stop("Standard deviation has to be homogeneous or has the same length as observations.")
  }
  stabadjustment_condition = (length(sdev)==1) | stabadjustment
  if(stabadjustment_condition){
    m_sdev <- mean(sdev)
    s <- sdev/m_sdev
    x <- x/m_sdev
  } else { s <- sdev }
  
	if ( pr=="l" && is.na (a) ) 
	{ wa  <-  wandafromx(x, s, universalthresh)
	w  <-  wa$w
	a  <-  wa$a 
	}  	else	
	{w <- wfromx(x, s, prior = prior, a = a)}
	tt <- tfromw(w, s, prior = prior, bayesfac = bayesfac, a = a)
	if(stabadjustment_condition) {
	  t <- tt * m_sdev
	} else { t <- tt }
	return(t)
}
