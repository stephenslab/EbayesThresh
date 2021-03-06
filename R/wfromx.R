wfromx <- function (x, s = 1, prior = "laplace", a = 0.5,
                    universalthresh = TRUE) {
#    
#  Given the vector of data x and s (sd),
#   find the value of w that zeroes S(w) in the
#   range by successive bisection, carrying out nits harmonic bisections
#   of the original interval between wlo and 1.  
#  
	pr <- substring(prior, 1, 1)
	if(pr == "c")
          s = 1
	if(universalthresh) {
    	  tuniv <- sqrt(2 * log(length(x))) * s
    	  wlo <- wfromt(tuniv, s, prior, a)
    	  wlo <- max(wlo)
	} else 
          wlo = 0
	if(pr == "l")
	  beta <- beta.laplace(x, s, a)
	if(pr == "c")
	  beta <- beta.cauchy(x)
	whi  <- 1
	beta <- pmin(beta, 1e20) 
	shi  <- sum(beta/(1 + beta))
	if(shi >= 0)
  	  return(1)
	slo <- sum(beta/(1 + wlo * beta))
	if(slo <= 0)
	  return(wlo)
	for(j in (1:30)) {
	  wmid <- sqrt(wlo * whi)
	  smid <- sum(beta/(1 + wmid * beta))
	  if(smid == 0)
	    return(wmid)
	  if(smid > 0)
	    wlo <- wmid
  	  else
	    whi <- wmid
        }
	return(sqrt(wlo * whi))
}
