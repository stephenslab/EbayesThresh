postmean.laplace <- function(x, s = 1, w = 0.5, a = 0.5) {
#
#  Find the posterior mean for the double exponential prior for 
#   given x, s (sd), w and a.
#
  # First find the probability of being non-zero
	wpost <- wpost.laplace(w, x, s, a)
  return(wpost*pmeancond(x,s,a))

}

# compute posterior mean under Laplace prior, from equation 2.8 in thesis
# conditional on non-zero
pmeancond = function(x,s,a){
  l=lambda(x,s,a)
  l *(x-s^2*a) + (1-l)*(x+s^2*a)
}
