postmed <- function (x, s, w = 0.5, prior = "laplace", a = 0.5) {
#
#  Find the posterior median for the appropriate prior for 
#   given x, s (sd), w and a. 
#
	pr <- substring(prior, 1, 1)
	if(pr == "l")
		muhat <- postmed.laplace(x, s, w, a)
	if(pr == "c") {
	  if(!(missing(s))) stop("Standard deviation is not allowed for Cauchy prior.")
		muhat <- postmed.cauchy(x, w)
	}
	return(muhat)
}
