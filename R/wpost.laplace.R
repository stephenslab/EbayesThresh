"wpost.laplace" <- 
function(w, x, s, a)
{
#
#  Calculate the posterior weight for non-zero effect
#
  wpost <- 1 - (1 - w)/(1 + w * beta.laplace(x, s, a))
  return(wpost)
}