"tfromw" <-
function(w, s, prior = "laplace", bayesfac = FALSE, a = 0.5)
{
#
#  Given the vector of weights w and s (sd), find the threshold or vector of
#   thresholds corresponding to these weights, under the specified prior.
#  If bayesfac=TRUE the Bayes factor thresholds are found, otherwise the posterior median
#   thresholds are found.
#  If the Laplace prior is used, a gives the value of the scale factor
#
	pr <- substring(prior, 1, 1)
	if(bayesfac) {
		z <- 1/w - 2
		if(pr == "l"){ 
		  if(length(w)>=length(s)) {
		    zz <- z
		  } else { zz <- rep(z, length(s)) }
		  tt <- vecbinsolv(zz, beta.laplace, 0, 10, s = s, a = a)
		}
		if(pr == "c")
			tt <- vecbinsolv(z, beta.cauchy, 0, 10)
	}
	else {
	  z <- 0
		if(pr == "l"){
		  zz <- rep(0, max(length(s), length(w)))
		  # When x/s-s*a>25, laplace.threshzero has value close to 1/2;
		  #  The boundary value of x can be treated as the upper bound for search.
			tt <- vecbinsolv(zz, laplace.threshzero, 0, s*(25+s*a), s = s, w = w, 
				a = a)
		}
		if(pr == "c")
			tt <- vecbinsolv(z, cauchy.threshzero, 0, 10, w = w)
	}
	return(tt)
}
