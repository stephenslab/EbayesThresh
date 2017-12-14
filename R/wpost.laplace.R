wpost.laplace <- function(w, x, s = 1, a = 0.5)
#
#  Calculate the posterior weight for non-zero effect
#
{
  
  if(w==0){return(rep(0,length(x)))}
  if(w==1){return(rep(1,length(x)))}
  lg = logg.laplace(x,s,a)
  lf = dnorm(x,0,s,log=TRUE)
  return(w/(w+(1-w)*exp(lf-lg)))
}

wpost.laplace.orig <- function(w, x, s = 1, a = 0.5)
  #
  #  Calculate the posterior weight for non-zero effect
  #
  1 - (1 - w)/(1 + w * beta.laplace(x, s, a))

 


