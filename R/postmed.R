postmed <- function (x, s, w, prior = "laplace", a = 0.5) {
#
#  Find the posterior median for the appropriate prior for 
#   given x, s (sd), w and a. 
#
	pr <- substring(prior, 1, 1)
	if(pr == "l")
		muhat <- postmed.laplace(x, s, w, a = a)
	if(pr == "c")
		muhat <- postmed.cauchy(x, w)
	return(muhat)
}
