postmean <- function(x, s = 1, w = 0.5, prior = "laplace", a = 0.5) {
#
#  Find the posterior mean for the appropriate prior for 
#   given x, s (sd), w and a.
#
	pr <- substring(prior, 1, 1)
	if(pr == "l")
		mutilde <- postmean.laplace(x, s, w, a = a)
	if(pr == "c"){
	  if(any(s != 1)) stop("Only standard deviation of 1 is allowed for Cauchy prior.")
		mutilde <- postmean.cauchy(x, w)
	}
	return(mutilde)
}
