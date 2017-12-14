# compute posterior second moments under laplace prior

postmean2.laplace <- function(x, s = 1, w = 0.5, a = 0.5) {
  #
  #  Find the posterior second moment for the double exponential prior for 
  #   given x, s (sd), w and a.
  #
  
  # Only allow a < 20 for input value.
  a <- min(a, 20)
  
  # First find the probability of being non-zero
  wpost <- wpost.laplace(w, x, s, a)
  
  # Now find the posterior mean conditional on being non-zero
  return(wpost* pmean2cond(x,s,a))
  
}


# computes the lambda function equation (2.7) from Kan Xu's thesis, which is posterior probability of being negative
# given a non-zero effect
lambda = function(x,s,a){
  m1 = exp(-a*x) * pnorm(x/s - s*a)
  m2 = exp(a*x) * (1-pnorm(x/s + s*a))
  return(m1/(m1+m2))
}

# compute posterior second moment under Laplace prior, conditional on non-zero
pmean2cond = function(x,s,a){
  l=lambda(x,s,a)
  l * ashr:::my_e2truncnorm(0,Inf,x-s^2*a,s) + (1-l)*ashr:::my_e2truncnorm(-Inf,0,x+s^2*a,s)
}