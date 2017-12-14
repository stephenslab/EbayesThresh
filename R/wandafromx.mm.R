#' @title Compute estimates of w and a for data (x,s) using method of moments
wandafromx.mm <- function(x, s) {
  m4 = mean(x^4-3*s^4)  # 4th moment
  m2 = mean(x^2-s^2)    # second moment
  a2 = 12/((m4/m2) - 6*mean(s^2))
  pi0 = 1- a2*m2/2
  if(a2<0){a2=0; pi0=1}
  if(pi0<0){pi0=0; a2 =2/m2}
  if(pi0>1){pi0=1; a2 = 1} #value of a2 here is arbitrary as pi0=1
    
  return(list(w=1-pi0,a=sqrt(a2)))
}