wandafromx <- function(x, s = 1, universalthresh = TRUE) {
#
#  Find the marginal max lik estimators of w and a given standard
#   deviation s, using a bivariate optimization;
#    
#  If universalthresh=TRUE, the thresholds will be upper bounded by
#   universal threshold adjusted by standard deviation. The threshold
#   is constrained to lie between 0 and sqrt ( 2 log (n)) *
#   s. Otherwise, threshold can take any nonnegative value;
#
#  If running R, the routine optim is used; in S-PLUS the routine is
#   nlminb.
#
    
  # Range for thresholds
  if(universalthresh) {
    thi <- sqrt(2 * log(length(x))) * s
  } else{
    thi <- Inf
  }
  
	tlo <- rep(0, length(s))
	lo  <-  c(0,0.04)
	hi  <-  c(1,3)
	startpar  <-  c(0.5,0.5)
	if (exists("optim")) {
  	  uu <- optim(startpar, negloglik.laplace, method="L-BFGS-B",
                      lower = lo, upper = hi, xx = x, ss = s, thi = thi,
                      tlo = tlo)
          uu <- uu$par
	}
	else {
          uu <- nlminb(startpar, negloglik.laplace, lower = lo,
                       upper = hi, xx = x, ss = s, thi = thi, tlo = tlo)
          uu <- uu$parameters
        }
	
	a <- uu[2]
	wlo <- wfromt(thi, s, a = a)
	whi <- wfromt(tlo, s, a = a)
	wlo <- max(wlo)
	whi <- min(whi)
	w <- uu[1]*(whi - wlo) + wlo
	return(list(w=w, a=a))
}
