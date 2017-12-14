postmean.laplace <- function(x, s = 1, w = 0.5, a = 0.5) {
#
#  Find the posterior mean for the double exponential prior for 
#   given x, s (sd), w and a.
#
  # First find the probability of being non-zero then return that time conditional mean
	wpost <- wpost.laplace(w, x, s, a)
  return(wpost*pmeancond(x,s,a))
}

# compute posterior mean under Laplace prior, conditional on non-zero
pmeancond = function(x,s,a){
  l=lambda(x,s,a)
  l * ashr:::my_etruncnorm(0,Inf,x-s^2*a,s) + (1-l)*ashr:::my_etruncnorm(-Inf,0,x+s^2*a,s)
}

# compute posterior mean under Laplace prior, conditional on non-zero
# from equation 2.8 in MS paper by Xu
# This alternative approach for computing mean seems 
# to agree with above, *except* for case where a is very big
# I'm not clear why
# When a is very big the prior gets peaked about 0, so posterior mean should tend to 0
# but this one does not. 
# eg
#> EbayesThresh:::pmeancond(1,1,1e6)
#[1] 0
#> EbayesThresh:::pmeancond_alt(1,1,1e6)
#[1] 1
pmeancond_alt = function(x,s,a){
  l=lambda(x,s,a)
  l *(x-s^2*a) + (1-l)*(x+s^2*a)
}
