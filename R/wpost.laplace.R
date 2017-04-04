"wpost.laplace" <- 
function(w, x, s, a)
{
  wpost <- 1 - (1 - w)/(1 + w * beta.laplace(x, s, a))
  return(wpost)
}