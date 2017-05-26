postmean <- function(x, s, w = 0.5, prior = "laplace", a = 0.5) {
#
#  Find the posterior mean for the appropriate prior for 
#   given x, s (sd), w and a.
#
	pr <- substring(prior, 1, 1)
	if(pr == "l")
		mutilde <- postmean.laplace(x, s, w, a = a)
	if(pr == "c"){
	  if(!(missing(s))) stop("Standard deviation is not allowed for Cauchy prior.")
		mutilde <- postmean.cauchy(x, w)
	}
	return(mutilde)
}
